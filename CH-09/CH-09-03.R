########################################################
# Description:
# 1.for Book 'R with applications to financial quantitive analysis'
# 2.Chapter: CH-09-03
# 3.Section: 9.3
# 4.Purpose: European option
# 5.Author: Qifa Xu
# 6.Date: Aug 20, 2014.
# 7.Revised: Aug 24, 2014.
########################################################
# Contents:
# 1. approximate Brownian motion by standard normal random variates or random walk
# 2. compute payoff value and profit
# 3. do Binomial tree option pricing
# 4. price option through explicit fourmulas
# 5. price option through Monte Carlo approach
# 6. do sensitivity analysis
# 7. calculate Greeks
#########################################################

# 0. initialize
# (1) set path
setwd('F:/programe/book/R with application to financial quantitive analysis/CH-09')
rm(list=ls())

# (2) load packages
library(xlsx)                         # for reading data from EXCEL file
library(fOptions)                     # for option pricing
library(quantmod)                     # for reading data from Yahoo
library(tseries)                      # for reading data from Yahoo
library(fImport)                      # for reading data from Yahoo
source('Sub-09.R')                    # for loading our own functions

# 1. approximate Brownian motion by standard normal random variates or random walk
# (1) define function
appBM <- function(n=500, t=0.5, method='RW'){
  if (method=='RW') X <- sample(c(-1,1), n, replace=TRUE)   # generate random number
  if (method=='NORMAL') X <- rnorm(n, mean=0, sd=1)         # generate random number

  S <- cumsum(X)                                            # compute cumlative sum
  W <- S[floor(n*t)]/sqrt(n)                                # compute Wn
  if (any(t==0)) W <- c(0, W)                               # set start point from 0
  W
}

# (2) show Brownian motion path
n <- 500
t <- seq(0,1,length=200)
set.seed(12345)
W.RW <-appBM(n=n, t=t, method='RW')
W.NORMAL <- appBM(n=500, t=t, method='NORMAL')
par(mfrow=c(1,2))
plot(t, W.RW, type='l', xlab='时间', ylab='W', main='随机行走模拟')
plot(t, W.NORMAL, type='l', xlab='时间', ylab='W', main='正态随机变量模拟')

# (3) check the normality distributed
B <- 1000                                                  # set repeat times
set.seed(12345)
W.05 <- numeric(B)
W.08 <- numeric(B)
for (b in 1:B){
  W.05[b] <- appBM(n=n, t=0.5, method='RW')                # set t=0.5
  W.08[b] <- appBM(n=n, t=0.8, method='RW')                # set t=0.8
}
dn.05 <- function(x) dnorm(x, mean=0, sd=sqrt(0.5))
dn.08 <- function(x) dnorm(x, mean=0, sd=sqrt(0.8))

par(mfrow=c(1,2))
plot(density(W.05), xlim=c(-3,3), ylim=c(0,0.7), lty=1, xlab='', ylab='密度', main='t=0.5')
curve(dn.05, -3, 3, add=TRUE, lty=2)
legend('topleft', legend=c('随机游走', '布朗运动'), lty=c(1,2))

plot(density(W.08), xlim=c(-3,3), ylim=c(0,0.6), lty=1, xlab='', ylab='密度', main='t=0.8')
curve(dn.08, -3, 3, add=TRUE, lty=2)
legend('topleft', legend=c('随机游走', '布朗运动'), lty=c(1,2))

# 2. compute payoff value and profit
# (1) define payoff function
payoff_call <- function(S, K){
  sapply(S, function(S, K) max(c(S-K, 0)), K=K)
}
payoff_put <- function(S, K){
  sapply(S, function(S, K) max(c(K-S, 0)), K=K)
}

# (2) compute payoff value
K <- 1.5
S <- seq(0, 3, length=100)
C0 <- P0 <- 0.5
payoff.call <- payoff_call(S, K)
payoff.put <- payoff_put(S, K)
profit.call <- payoff.call-C0
profit.put <- payoff.put-P0

par(mfrow=c(1,2))
plot(c(S,S), c(payoff.call, profit.call), type='n', main='看涨期权', xlab='到期价格',ylab='利润（价值）')
lines(S, payoff.call, lty=1, lwd=2)
lines(S, profit.call, lty=2, lwd=2)
abline(h=0, lty=3)
legend('topleft', legend=c('交割函数', '利润函数'), lty=c(1,2), lwd=c(2,2))
plot(c(S,S), c(payoff.put, profit.put), type='n', main='看跌期权', xlab='到期价格',ylab='利润（价值）')
lines(S, payoff.put, lty=1, lwd=2)
lines(S, profit.put, lty=2, lwd=2)
abline(h=0, lty=3)
legend('topright', legend=c('交割函数', '利润函数'), lty=c(1,2), lwd=c(2,2))
par(mfrow=c(1,1))


# 3. do Binomial tree option pricing
# (1) Cox-Ross-Rubinstein Binomial Tree Option Model
CRRBinomialTreeOption(TypeFlag='ce', S=100, X=110, Time=6/12, r=0.05, b=0.05, sigma=0.2, n=50)
GBSOption(TypeFlag='c', S=100, X=110, Time=6/12, r=0.05, b=0.05, sigma=0.2)@price

# (2) Plot CRR Option Tree
CRRTree = BinomialTreeOption(TypeFlag='pa', S=100, X=110, Time=6/12, r=0.05, b=0.05, sigma=0.2, n=6)
BinomialTreePlot(CRRTree, dy=1, cex=0.8, ylim=c(-6, 7), xlab='n期', ylab='期权价值', main='期权树')


# 4. price option through explicit fourmulas
# (1) set parameters
S0 <- 10
K <- 11
r <- 0.05
TT <- 1/4
sigma <- 0.15

# (2) define function in Sub-08.R
# function: price_call
# function: price_put

# (3) compare defined function with R function: GBSOption
(price.call <- price_call(S=S0, t=0, TT=TT, r=r, sigma=sigma, K=K))
(price.put <- price_put(S=S0, t=0, TT=TT, r=r, sigma=sigma, K=K))
round((price.call-S0+K*exp(-r*TT)), digits=4) == round(price.put, digits=4)       # check the put-call parity
round((price.call+K*exp(-r*TT)), digits=4) == round(price.put+S0, digits=4)       # check the put-call parity

(price.call.R <- GBSOption(TypeFlag='c', S=S0, X=K, Time=TT, r=r, b=r, sigma=sigma)@price)
(price.put.R <- GBSOption(TypeFlag='p', S=S0, X=K, Time=TT, r=r, b=r, sigma=sigma)@price)


# 5. price option through Monte Carlo approach
# (1) define Monte Carlo function for B-S
# function: price_MonCar

# (2) do Monte Carlo simulation
set.seed(12345)
N <- 100
price_MonCar(S=S0, t=0, TT=TT, r=r, sigma=sigma, K=K, f=f, N=N)

n.obs <- c(10, 50, 100, 150, 200, 250, 500, 1000)     # set sample size
B <- 500                                              # repeated times
C.MC <- matrix(NA, B, length(n.obs))                  # apply for NULL matrix
for (j in seq_along(n.obs)){
  for (b in 1:B){
    C.MC[b, j] <- price_MonCar(S=S0, t=0, TT=TT, r=r, sigma=sigma, K=K, f=f, N=n.obs[j])
  }
}
colnames(C.MC) <- paste('size=', n.obs, sep='')
mean.C.MC <- apply(C.MC, 2, mean)
err.C.MC <- apply(C.MC, 2, sd)

# (3) make comparison with B-S explicit fourmulas
C.Anl <- GBSOption(TypeFlag='c', S=S0, X=K, Time=TT, r=r, b=r, sigma=sigma)@price

# (4) show results
up.C.MC <- mean.C.MC + err.C.MC
down.C.MC <- mean.C.MC - err.C.MC
plot(n.obs, mean.C.MC, type='n', ylim=range(C.MC), axes=FALSE, xlab='MC的重复', ylab='MC的价格')
lines(n.obs, up.C.MC, col='red')
lines(n.obs, down.C.MC, col='red')
axis(1, n.obs)
axis(2, C.Anl, 'B-S formula')
boxplot(C.MC, add=TRUE, at=n.obs, boxwex=15, col='orange', axes=FALSE)
points(n.obs, mean.C.MC, col='red', lwd=3, lty=3)
abline(h=C.Anl, lty=2, col='blue', lwd=3)

# 6. do sensitivity analysis
# (1) impact of sigma
S0 <- seq(1, 20, by=1)
r <- 0.01
TT <- 50
sigma <-0
K <- 10
# Cprice.sigma <- matrix(NA, nrow=length(sigma), ncol=length(S0))
Cprice.sigma <- price_call(S=S0, t=0, TT=TT, r=r, sigma=sigma, K=K)

plot(S0, Cprice.sigma, type='l', xlab='S0', ylab='看涨价格')

# (2) impact of K
S0 <- 10
r <- 0.01
TT <- 50
sigma <- seq(0, 1, length=100)
K <- c(8, 10, 15)
Cprice.K <- matrix(NA, nrow=length(sigma), ncol=length(K))
for (j in seq_along(K)){
  Cprice.K[,j] <- price_call(S=S0, t=0, TT=TT, r=r, sigma=sigma, K=K[j])
}
colnames(Cprice.K) <- paste('K=', K, sep='')

plot(rep(sigma,3), c(Cprice.K), type='n', xlab=expression(sigma), ylab='看涨价格')
lines(sigma, Cprice.K[,1], lty=1)
lines(sigma, Cprice.K[,2], lty=2)
lines(sigma, Cprice.K[,3], lty=3)
legend('bottomright', legend=colnames(Cprice.K), lty=c(1,2,3))


# (3) impact of T
S0 <- 10
r <- 0.01
TT <- c(10, 50, 100)
sigma <- seq(0, 1, length=100)
K <- 10
Cprice.T <- matrix(NA, nrow=length(sigma), ncol=length(TT))
for (j in seq_along(TT)){
  Cprice.T[,j] <- price_call(S=S0, t=0, TT=TT[j], r=r, sigma=sigma, K=K)
}
colnames(Cprice.T) <- paste('T=', TT, sep='')

plot(rep(sigma,3), c(Cprice.T), type='n', xlab=expression(sigma), ylab='看涨价格')
lines(sigma, Cprice.T[,1], lty=1)
lines(sigma, Cprice.T[,2], lty=2)
lines(sigma, Cprice.T[,3], lty=3)
legend('bottomright', legend=colnames(Cprice.T), lty=c(1,2,3))

# 7. calculate Greeks
# (1) set parameters
S0 <- 10
K <- 11
r <- 0.01
sigma <- 0.05
TT <- 50
tt <- 10

# (2) do calculation
GBSCharacteristics(TypeFlag='c', S=S0, X=K, Time=TT-tt, r=r, b=r, sigma=sigma)

# 8. estimate implied volatility and volatility smiles
# (1) read data from EXCEL file
Call.1412 <- read.xlsx(file='GoogleOptions.xlsx', sheetName='Call_Expire14-12')
Put.1412 <- read.xlsx(file='GoogleOptions.xlsx', sheetName='Put_Expire14-12')
MergeDat <- merge(Call.1412, Put.1412, by='Strike')
OptionDat <- MergeDat[,c(1,5,6,12,13)]
colnames(OptionDat) <- c('K', 'C.Bid', 'C.Ask', 'P.Bid', 'P.Ask')
OptionDat <- OptionDat[-c(1:5),]
OptionDat <- OptionDat[OptionDat$K>=500,]
write.csv(OptionDat, file='OptionDat.csv', row.names=FALSE)
OptionDat <- read.csv(file='OptionDat.csv')
str(OptionDat)

# (2)  calculate implied volatility
S0 <- 580.20
TT <- 60/252
r <- 0.02


n <- nrow(OptionDat)
c.vol.Ask <- numeric(n)
c.vol.Bid <- numeric(n)
p.vol.Ask <- numeric(n)
p.vol.Bid <- numeric(n)

for(i in 1:n){
  c.vol.Ask[i] <- GBSVolatility(price=OptionDat$C.Ask[i], TypeFlag='c', S=S0, X=OptionDat$K[i], Time=TT, r=r, b=r)
  c.vol.Bid[i] <- GBSVolatility(price=OptionDat$C.Bid[i], TypeFlag='c', S=S0, X=OptionDat$K[i], Time=TT, r=r, b=r)
  p.vol.Ask[i] <- GBSVolatility(price=OptionDat$P.Ask[i], TypeFlag='p', S=S0, X=OptionDat$K[i], Time=TT, r=r, b=r)
  p.vol.Bid[i] <- GBSVolatility(price=OptionDat$P.Bid[i], TypeFlag='p', S=S0, X=OptionDat$K[i], Time=TT, r=r, b=r)
}

# (3) show results
par(mfrow=c(1,2))
plot(c(OptionDat$K, OptionDat$K), c(c.vol.Bid, c.vol.Ask), typ='n', main='看涨的波动微笑', xlab='行权价格', ylab='隐含波动')
lines(OptionDat$K, c.vol.Bid, lty=1)
lines(OptionDat$K, c.vol.Ask, lty=2)
abline(v=S0, col='red', lty=3)
legend('bottomleft', c('Ask', 'Bid'), lty=c(1,2))

plot(c(OptionDat$K, OptionDat$K), c(p.vol.Bid, p.vol.Ask), typ='n', main='看跌的波动微笑', xlab='行权价格', ylab='隐含波动')
lines(OptionDat$K, p.vol.Bid, lty=1)
lines(OptionDat$K, p.vol.Ask, lty=2)
abline(v=S0, col='red', lty=3)
legend('bottomleft', c('Ask', 'Bid'), lty=c(1,2))




