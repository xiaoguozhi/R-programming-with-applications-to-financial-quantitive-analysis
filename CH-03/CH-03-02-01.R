########################################################
# Description:
# 1.for Book 'R with applications to financial quantitive analysis'
# 2.Chapter: CH-03
# 3.Section: 3.2
# 4.Purpose: LASSO regression
# 5.Author: Qifa Xu
# 6.Date: Aug 20, 2014.
# 7.Revised: Aug 28, 2014.
########################################################
# Contents:
# 1. generate data
# 2. do regression for one simulation
# 3. do regression for 50 simulations
#########################################################

# 0. Initializing
# (1) set path
setwd('F:/programe/book/R with application to financial quantitive analysis/CH-03')
rm(list = ls())

# (2) load packages
library(lars)           # for LASSO regression
library(ridge)          # for ridge regression
library(mvtnorm)        # for multivariate normal distribution

# 1. generate data
N <- 30
B <- 50
rho <- 0.5
sigma <- 3
betas <- c(3.5,1,0,2,0,0)
set.seed(12345)
I <- matrix(rep(1:length(betas), times=length(betas)), byrow=FALSE, nrow=length(betas))
J <- matrix(rep(1:length(betas), times=length(betas)), byrow=TRUE, nrow=length(betas))
(corr <- rho^abs(I-J))

eps <- rnorm(N, mean=0, sd=1)
X <- rmvnorm(n=N, mean=rep(0, length(betas)), sigma=corr)
cor(X)
Y <- X %*% betas + sigma * eps
dat <- data.frame(Y, X)

# 2. do regression for one simulation
# (1) do OLS regression
model.ols <- lm(Y~.-1, data=dat)
summary(model.ols)
coef.ols <- coef(model.ols)
coef.ols[coef.ols!=0]                           # show those coefficients not equal to 0

# (2) do ridge regression
model.rid <- linearRidge(Y~.-1, data=dat)
summary(model.rid)
coef.rid <- coef(model.rid)
coef.rid[coef.rid!=0]                            # show those coefficients not equal to 0

# (3) do lasso regression
model.lasso <- lars(X, Y, type='lasso')          # make model
plot(model.lasso)                                # give plot
summary(model.lasso)
set.seed(12345)
CV.lasso <- cv.lars(X, Y, K=10)                  # do cross validation
(best <- CV.lasso$index[which.min(CV.lasso$cv)])   # select best value
(coef.lasso <- coef.lars(model.lasso, mode='fraction', s=best))
names(coef.lasso) <- colnames(dat)[-1]
coef.lasso[coef.lasso!=0]                        # show those coefficients not equal to 0

# 3. do regression for 50 simulations
# (1) define Monte Carlo function
MonteCarlo <- function(N, betas, type='lasso'){
  # (1) generate data
  I <- matrix(rep(1:length(betas), times=length(betas)), byrow=FALSE, nrow=length(betas))
  J <- matrix(rep(1:length(betas), times=length(betas)), byrow=TRUE, nrow=length(betas))
  corr <- rho^abs(I-J)
  
  eps <- rnorm(N, mean=0, sd=1)
  X <- rmvnorm(n=N, mean=rep(0, length(betas)), sigma=corr)
  Y <- X %*% betas + sigma * eps
  dat <- data.frame(Y, X)
  
  # (2) do OLS regression
  model.ols <- lm(Y~.-1, data=dat)
  coef.ols <- coef(model.ols)
  
  # (3) do ridge regression
  model.rid <- linearRidge(Y~.-1, data=dat)
  coef.rid <- coef(model.rid)
  
  # (4) do lasso regression
  model.lasso <- lars(X, Y, type=type)
  CV.lasso <- cv.lars(X, Y, K=10, plot.it=FALSE)
  best <- CV.lasso$index[which.min(CV.lasso$cv)]
  coef.lasso <- coef.lars(model.lasso, mode='fraction', s=best)
  names(coef.lasso) <- colnames(dat)[-1]
  
  # (5) output
  ans <- list(coef.ols=coef.ols, coef.rid=coef.rid, coef.lasso=coef.lasso)
  ans
}

# (2) repeat simulation 50 times
coef.ols <- coef.rid <- coef.lasso <- matrix(NA, nrow=length(betas), ncol=B)
for (b in 1:B){
  coef.MC <- MonteCarlo(N, betas, type='lasso')
  coef.ols[,b] <- coef.MC$coef.ols
  coef.rid[,b] <- coef.MC$coef.rid
  coef.lasso[,b] <- coef.MC$coef.lasso
}

coef.methods <- matrix(NA, nrow=B, ncol=3)
for (i in 1:length(betas)){
  coef.methods <- cbind(coef.ols[i,], coef.rid[i,], coef.lasso[i,])
  colnames(coef.methods) <- c('OLS', 'ridge', 'LASSO')
  boxplot(coef.methods, main=paste('X', i, 'µÄÏµÊý', sep=''))
  abline(h=betas[i], lty=3)
}

