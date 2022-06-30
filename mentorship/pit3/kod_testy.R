###
### testy A, B, P, TC (two-cumulant)
###

fpp1.test = function(yy, N = 1000, Nboot = 1000, R = 1000){
  ## dane i podstawowe obliczenia
  n = nrow(yy)
  p = ncol(yy)
  ## warto?? statystyki testowej
  Cn = n*sum((colMeans(yy[,1:(p/2)])-colMeans(yy[,(p/2+1):p]))^2)
  
  ## test A
  CC = var(yy)    ## macierz kowariancji dla [0,2]
  require(MASS)   ## generowanie proces?w zb
  zb = mvrnorm(n = N, rep(0, nrow(CC)), CC)
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
  C = sum(diag(KK %*% KK %*% KK))
  beta = B/A
  d = (A^2)/B
  TCpvalue = 1-pchisq(Cn/beta, d)
  
  return(list(A = Apvalue, B = Bpvalue, P = Ppvalue, Z = TCpvalue))
}
