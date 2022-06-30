library(dplyr)

#### generalne ####
alpha = 0.05
n = 35; orig_n = c(25, 35, 50)
I = 26; orig_I = c(26, 101, 251)
by = 1000/(I-1) #na potrzeby podziałek
errortype <- c("N", "L", "M")
rho <- c(0, 0.25, 0.5); orig_rho = c(0, 0.25, 0.5)
xi <- c('M0to3' = 0.05, 'M4to7' = 0.5)
time <- seq(0, 1, by = 0.001)

#### funkcje modeli ####
m_01 <- function(t){ sqrt(6*t/pi)*exp(-6*t)}
m_11 <- function(t){ sqrt(13*t/pi/2)*exp(-13*t/2)}
m_21 <- function(t){ sqrt(11*t/pi/2)*exp(-11*t/2)}
m_31 <- function(t){ sqrt(5)*t^(2/3)*exp(-7*t)}
m_02 <- function(t){ sin(2*pi*t^2)^5 }
m_12 <- function(t){ sin(2*pi*t^2)^3 }
m_22 <- function(t){ sin(2*pi*t^2)^7 }
m_32 <- function(t){ sin(2*pi*t^(9/5))^3 }

#### błędy ####
bridge <- function(){
  time <- seq(0, 1, by = 0.001)
  #ruch Browna -- odstęp to 1/1001, więc sd to pierwiastek z tego
  X <- cumsum(rnorm(1001, mean = 0, sd = sqrt(1 / 1001))) 
  #most Browna z ruchu Browna
  Z <- X - time * X[1001]
}
eps_1 = function(brdg1, xi){ xi*brdg1 }
eps_2 = function(brdg2, eps1, xi, rho){ rho*eps1 + xi*sqrt(1-rho^2)*brdg2 }
#dla lognormalnych expuję ręcznie

#### symulacja -- zwraca 3 podziały ####
modelmaker <- function(modelId, .m1, .m2, .xi,
                       errortype = "N", rho = 0.25, n = 35){
  # boilerplate
  outputs <- list()
  
  # main
  data <- matrix(rep(0, n * 2002), nrow = n)
  for(i in 1:n){ #pętla obserwacji
    e1 <- eps_1(bridge(), .xi)
    e2 <- eps_2(bridge(), e1, .xi, rho)
    if(errortype == "L"){ e1 <- exp(e1); e1 <- e1 - mean(e1) }
    if(errortype %in% c("L", "M")){ e2 <- exp(e2); e2 <- e2 - mean(e2) }
    data[i, 1:1001] <- .m1 + e1
    data[i, 1002:2002] <- .m2 + e2
    for(j in 1:length(I)){ #pętla dokładności (I)
      outputs[[paste(modelId,
                     "_n", n,
                     "_rho", rho,
                     "_e", errortype,
                     "_I", I[j],
                     sep = "")]] <- data[, c(seq(1, 1001, by = by[j]),
                                             seq(1002, 2002, by = by[j]))]
    }
  }
  outputs
}

#### testy ####
calc.cn <- function(yy){
  n = nrow(yy)
  p = ncol(yy)
  ## warto?? statystyki testowej
  Cn = n*sum((colMeans(yy[,1:(p/2)])-colMeans(yy[,(p/2+1):p]))^2)
  return(Cn)
}

fpp1.test = function(yy, N = 1000, Nboot = 1000, R = 1000){
  ## dane i podstawowe obliczenia
  n = nrow(yy)
  p = ncol(yy)
  ## warto?? statystyki testowej
  Cn = n*sum((colMeans(yy[,1:(p/2)])-colMeans(yy[,(p/2+1):p]))^2)
  
  ## test A
  CC = var(yy)    ## macierz kowariancji dla [0,2]
  zb = MASS::mvrnorm(n = N, rep(0, nrow(CC)), CC)
  CnAb = rowSums((zb[,1:(p/2)]-zb[,(p/2+1):p])^2)
  Apvalue = mean(CnAb>=Cn)
  
  ## test B
  CnBb = numeric(Nboot)
  for(i in 1:Nboot){
    yyB = yy[floor(runif(n)*(n-1)) + 1,]
    CnBb[i] = n*sum((colMeans(yyB[,1:(p/2)])-colMeans(yy[,1:(p/2)])+colMeans(yy[,(p/2+1):p])-colMeans(yyB[,(p/2+1):p]))^2)
  }
  Bpvalue = mean(CnBb>Cn)
  
  ## test P
  CnPb = numeric(R)
  yy = as.matrix(yy)
  for(i in 1:R){
    yyP = matrix(rep(0, n*p), nrow = n)
    for(j in 1:n){
      if(runif(1)<0.5){
        yyP[j,] = yy[j,]
      }else{
        yyP[j, 1:(p/2)] = yy[j, (p/2+1):p]
        yyP[j, (p/2+1):p] = yy[j, 1:(p/2)]
      }
    }
    CnPb[i] = n*sum((colMeans(yyP[,1:(p/2)])-colMeans(yyP[,(p/2+1):p]))^2)
  }
  Ppvalue = mean(CnPb>Cn)
  
  ## test TC and 3C
  KK = CC[1:(p/2), 1:(p/2)] - CC[1:(p/2), (p/2+1):p] - CC[(p/2+1):p, 1:(p/2)] + CC[(p/2+1):p, (p/2+1):p]
  A = sum(diag(KK))
  B = sum(diag(KK %*% KK))
  #C = sum(diag(KK %*% KK %*% KK))
  beta = B/A
  d = (A^2)/B
  TCpvalue = 1-pchisq(Cn/beta, d)
  
  return(c(A = Apvalue, B = Bpvalue, P = Ppvalue, BT = TCpvalue))
}

#### faktyczne obliczenia ####
sim.n.test <- function(modelId, m1, m2, xi, .errortype = errortype, .rho, lp = 1000, write.results = T){
  results <- vector(mode = "list", length = length(I)*length(.errortype))
  names(results) <- .errortype
  lp <- 1000
  
  pb <- txtProgressBar(0, lp, style = 3)
  for(.lp in 1:lp){
    for(i in 1:length(.errortype)){
      x <- modelmaker(modelId, m1, m2, xi,
                      errortype = .errortype[i], rho = .rho, n = n) %>% lapply(fpp1.test)
      results[[i]] <- results[[i]] %>% rbind(x[[1]])
    }
    setTxtProgressBar(pb, .lp)
  }
  close(pb)
  
  if(write.results){
    #results$N %>% write.table(file = paste("datamaker/", modelId, "N,", .rho, ".txt", sep = ""))
    #results$L %>% write.table(file = paste("datamaker/", modelId, "L,", .rho, ".txt", sep = ""))
    #results$M %>% write.table(file = paste("datamaker/", modelId, "M,", .rho, ".txt", sep = ""))
    
    if(.errortype %>% length() > 1) .errortype <- NULL
    lapply(results, function(x){ 100 * colMeans(x < alpha) }) %>%
      do.call(rbind, .) %>%
      as.data.frame() %>%
      write.table(file = paste("datamaker/_", modelId, ",", .rho, .errortype, ".txt", sep = ""))
  }
  beepr::beep()
}

#### przemielenie gotowych danych danych ####
outputtable <- function(model1, model2){
  bind_cols(
    read.table(paste("datamaker/_",model1,",0.txt", sep = "")) %>%
      tibble::rownames_to_column("err") %>%
      bind_rows(read.table(paste("datamaker/_",model1,",0.25.txt", sep = "")) %>%
                  tibble::rownames_to_column("err")) %>%
      bind_rows(read.table(paste("datamaker/_",model1,",0.5.txt", sep = "")) %>%
                  tibble::rownames_to_column("err")),
    read.table(paste("datamaker/_",model2,",0.txt", sep = "")) %>% as_tibble() %>%
      bind_rows(read.table(paste("datamaker/_",model2,",0.25.txt", sep = "")) %>% as_tibble()) %>%
      bind_rows(read.table(paste("datamaker/_",model2,",0.5.txt", sep = "")) %>% as_tibble())
  ) %>%
    mutate(err = recode(err, N = "normalny", L = "lognormalny", M = "mieszany"))
}