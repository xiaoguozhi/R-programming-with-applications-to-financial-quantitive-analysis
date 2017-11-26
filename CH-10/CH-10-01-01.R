########################################################
# Description:
# 1.for Book 'R with applications to financial quantitive analysis'
# 2.Chapter: CH-10-01
# 3.Section: 10.1
# 4.Purpose: Unit Root Test With Artificially Generated Data
# 5.Author: Lin Kang, polished by Qifa Xu
# 6.Date: May 10, 2014.
# 7.Revised: Aug 24, 2014.
########################################################
# Contents:
# 1. generage data
# 2. do ADF test
# 3. do PP test
# 4. do KPSS test
#############################################################
# 0. Initializing
# (1) set path
setwd('F:/programe/book/R with application to financial quantitive analysis/CH-10')
rm(list=ls())

# (2) load packages
library(urca)                      # for unit root and cointegration tests for time series data

#1. generate data
set.seed(12345)
x <- rnorm(1000)                   #stationary time series
y <- cumsum(x)                     #pure random walk
t <- 1:1000
x1 <- 0.1*t+x                      #stationary time series with drift
y1 <- 10+0.1*t+y                   #non-stationary time series with drift and deterministic trends
par(mfrow= c(2,2),mar= c(4.5,3,2,2))
plot(x,type="l",xlab="平稳序列 ", ylab=" ")
legend("topleft","x",lty=1)
plot(y,type="l",xlab="随机游动 ", ylab=" ")
legend("topleft","y",lty=1)
plot(x1,type="l",xlab=" 带趋势项的平稳序列", ylab=" ")
legend("topleft","x1",lty=1)
plot(y1,type="l",xlab=" 带漂移项和趋势项的随机游动", ylab=" ")
legend("topleft","y1",lty=1)

# 2. do ADF test
adf.x <- ur.df(x,type="none",selectlags="AIC")              #ADF test for time series x
summary(adf.x)
adf.y <- ur.df(y,type="none",selectlags="AIC")              #ADF test for time series y
summary(adf.y)
adf.x1 <- ur.df(x1,type="trend",selectlags="AIC")           #ADF test for time series x1
summary(adf.x1)
adf.y1 <- ur.df(y1,type="trend",selectlags="AIC")           #ADF test for time series y1
summary(adf.y1)

# 3. do PP test
pp.x <- ur.pp(x,type="Z-tau")                               #PP test for time series x
summary(pp.x)
pp.y <- ur.pp(y,type="Z-tau")                               #PP test for time series y
summary(pp.y)
pp.x1 <- ur.pp(x1,type="Z-tau",model="trend")               #PP test for time series x1
summary(pp.x1)
pp.y1 <- ur.pp(y1,type="Z-tau",model="trend")               #pp test for time series y1
summary(pp.y1)

# 4. do KPSS test
library(tseries)                  #install package “tseries”
kpss.test(x)                      #KPSS test for time series x
kpss.test(y)                      #KPSS test for time series y
kpss.test(x1,null="Trend")        #KPSS test for time series x1   
kpss.test(y1,null="Trend")        #KPSS test for time series y1




