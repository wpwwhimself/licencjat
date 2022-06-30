setwd("~/Pobrane/kody_dane_WP")

source("kod_testy.R")

n <- 25
g <- 0

time = seq(0, 1, by = 0.001)
lp = 1000

# I = 26
set.seed(1001)
set.seed(sample(100:10000, 2, replace = FALSE)[2])
## set.seed(4185)
temp <- matrix(NA, lp, 4)
for (uu in 1:lp) {
  yy <- matrix(rep(0, n * 2002), nrow = n)
  M0 <- sqrt(6 * time/pi) * exp(-6 * time)
  M1 <- sqrt(6 * time/pi) * exp(-6 * time)
  for(i in 1:n){
    X <- cumsum(rnorm(1001,mean=0,sd=sqrt(1/1001)))
    Z <- X - time * X[1001]
    X1 <- cumsum(rnorm(1001,mean=0,sd=sqrt(1/1001)))
    Z1 <- X1-time * X1[1001]
    e1 <- (1/20) * Z
    yy[i,1:1001] <- M0 + e1
    e2c <- exp(g*e1 + (1/20)*sqrt(1-g^2)*Z1)
    yy[i,1002:2002] <- M1 + e2c - mean(e2c)
  }
  yy <- yy[,c(seq(1, 1001, by = 40), seq(1002, 2002, by = 40))]
  
  wynik <- fpp1.test(yy)
  temp[uu, ] <- c(wynik$A, wynik$B, wynik$P, wynik$Z)
}
100 * colMeans(temp < 0.05)

# I = 101
set.seed(1001)
set.seed(sample(100:10000, 2, replace = FALSE)[2])
## set.seed(4185)
temp <- matrix(NA, lp, 4)
for(uu in 1:lp){
  yy <- matrix(rep(0, n * 2002), nrow = n)
  M0 <- sqrt(6 * time/pi) * exp(-6 * time)
  M1 <- sqrt(6 * time/pi) * exp(-6 * time)
  for(i in 1:n){
    X <- cumsum(rnorm(1001,mean=0,sd=sqrt(1/1001)))
    Z <- X - time * X[1001]
    X1 <- cumsum(rnorm(1001,mean=0,sd=sqrt(1/1001)))
    Z1 <- X1-time * X1[1001]
    e1 <- (1/20) * Z
    yy[i,1:1001] <- M0 + e1
    e2c <- exp(g*e1 + (1/20)*sqrt(1-g^2)*Z1)
    yy[i,1002:2002] <- M1 + e2c - mean(e2c)
  }
  yy <- yy[,c(seq(1, 1001, by = 10), seq(1002, 2002, by = 10))]
  
  wynik <- fpp1.test(yy)
  temp[uu, ] <- c(wynik$A, wynik$B, wynik$P, wynik$Z)
}
100 * colMeans(temp < 0.05)

# I = 251
set.seed(1001)
set.seed(sample(100:10000, 2, replace = FALSE)[2])
## set.seed(4185)
temp <- matrix(NA, lp, 4)
for (uu in 1:lp) {
  yy <- matrix(rep(0, n * 2002), nrow = n)
  M0 <- sqrt(6 * time/pi) * exp(-6 * time)
  M1 <- sqrt(6 * time/pi) * exp(-6 * time)
  for(i in 1:n){
    X <- cumsum(rnorm(1001,mean=0,sd=sqrt(1/1001)))
    Z <- X - time  *  X[1001]
    X1 <- cumsum(rnorm(1001,mean=0,sd=sqrt(1/1001)))
    Z1 <- X1-time * X1[1001]
    e1 <- (1/20) * Z
    yy[i,1:1001] <- M0 + e1
    e2c <- exp(g*e1 + (1/20)*sqrt(1-g^2)*Z1)
    yy[i,1002:2002] <- M1 + e2c - mean(e2c)
  }
  yy <- yy[,c(seq(1, 1001, by = 4), seq(1002, 2002, by = 4))]
  
  wynik <- fpp1.test(yy)
  temp[uu, ] <- c(wynik$A, wynik$B, wynik$P, wynik$Z)
}
100 * colMeans(temp < 0.05)
