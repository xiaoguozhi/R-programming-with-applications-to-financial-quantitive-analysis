########################################################
# Description:
# 1.for Book 'R with applications to financial quantitive analysis'
# 2.Chapter: CH-08-04
# 3.Section: 8.4
# 4.Purpose: large scale portfolio (simulation)
# 5.Author: Qifa Xu
# 6.Date: Jan 11, 2014.
# 7.Revised: Aug 20, 2014.
########################################################
# Contents:
# 1. set parameters
# 2. simulate data (the parameters are taken from Fan, et al. (2008))
# 3. portfolio through quadratic programming with constraints
# 4. portfolio through lasso regression
#########################################################

# 0. initialize
# (1) set work path
setwd('F:/programe/book/R with application to financial quantitive analysis/CH-08')
rm(list=ls())

# (2) load package
library(MASS)
library(Rdonlp2)
library(lars)
library(msgps)
source('Sub-08.R')

# 1. set parameters
# (1) set number of obs and vars
obs <- 252
p <- 100 # 500 #   

# 2. simulate data (the parameters are taken from Fan, et al. (2008))
# (1) factor loadings
set.seed(12345)
mu_b <- c(0.7828, 0.5180, 0.4100)
cov_b <- matrix(c(0.02914,0.02387,0.01018, 0.02387,0.05395,-0.00696, 0.010184,-0.006967, 0.086856), nrow=3)
b <- mvrnorm(n=p, mu=mu_b, Sigma=cov_b)

# (2) three factors
mu_f <- c(0.02355, 0.01298, 0.02071)
cov_f <- matrix(c(1.2507,-0.0350,-0.2042,-0.0350,0.3156,-0.0023,-0.2042,-0.0023,0.1930), nrow=3)
f <- mvrnorm(n=obs, mu=mu_f, Sigma=cov_f)

# (3) random errors
shape.value <- 3.3586*3/4
scale.value <- 0.1876*3/4
var.error <- shape.value*scale.value^2
var(rgamma(n=10000, shape=shape.value, scale=scale.value))
epsi <- matrix(rgamma(n=p*obs, shape=shape.value, scale=scale.value), nrow=p)

# (4) returns
R <- b %*% t(f) + epsi
R <- t(R)
covR_est_cov <- cov(R)                          # through sample covariance
# covR_est_cov <- cov_FF(Y=t(R), f=t(f))          # through Fama-French three factors model
covR_true <- b%*%cov_f%*%t(b) + diag(rep(var.error, times=p))   
norm(covR_est_cov-covR_true)

# 3. portfolio through quadratic programming with constraints 精确算法
# (1) do portfolio selection with gorss exposure values精确算法基于样本估计和真实的方差协方差矩阵
c.values <- seq(1, 6, length=50)
ptm <- proc.time()
port_QP_actual <- port_QP(R=R, covM_act=covR_true, covM_emp=covR_est_cov, c.values, choice='actRisk')
port_QP_empir <- port_QP(R=R, covM_act=covR_true, covM_emp=covR_est_cov, c.values, choice='empRisk')
proc.time() - ptm

# (3) show results
# windows(width=10, height=6)
par(mfrow=c(1,2))
plot(c(c.values, c.values), c(port_QP_actual$volsOpt, port_QP_empir$volsOpt), type='n', xlab='约束参数c', ylab='投资组合的日波动率')
lines(c.values, port_QP_actual$volsOpt, lty=1)
lines(c.values, port_QP_empir$volsOpt, lty=2)
legend('topright', legend=c('真实-精确', '样本-精确'), lty=c(1,2))

plot(c.values, port_QP_actual$n.assets, type='l', xlab='约束参数c', ylab='资产数目')

###################################近似求解############################################
# 4. portfolio through lasso regression
# (1) do portfolio selection with gorss exposure values
par(mfrow=c(1,1))
noshortsalePort.act <- port_QP_actual$portOpt[1,]
port.lars.act <- port_method(noshortsalePort.act, R, method='lars', plot.method=TRUE, plot.vol=TRUE)
noshortsalePort.emp <- port_QP_empir$portOpt[1,]
port.lars.emp <- port_method(noshortsalePort.emp, R, method='lars', plot.method=TRUE, plot.vol=TRUE)

# (2) extract information
d <- port.lars.act$d                  # equal to port.lars.emp$d ?
w <- port.lars.act$w                  # equal to port.lars.emp$w ?
nAssets <- port.lars.act$nAssets      # equal to port.lars.emp$n.assets ?

# (3) make portfolio  近似算法基于样本估计和真实的方差协方差矩阵
vol.lars.act <- portComp(w=w, covM=covR_true)
vol.lars.emp <- portComp(w=w, covM=covR_est_cov)

# windows(width=10, height=6)
par(mfrow=c(1,1))
plot(c(d, d), c(vol.lars.act, vol.lars.emp), type='n', xlim=c(0,6), xlab='d', ylab='日波动率')
lines(d, vol.lars.act, lty=1)
lines(d, vol.lars.emp, lty=2)
legend('topright', legend=c('真实+近似', '经验+近似'), lty=c(1,2))

