########################################################
# Description:
# 1.for Book 'R with applications to financial quantitive analysis'
# 2.Chapter: CH-10-01
# 3.Section: 10.1
# 4.Purpose: Johansen Cointegeration Test With Artificially Generated Data
# 5.Author: Lin Kang, polished by Qifa Xu
# 6.Date: May 10, 2014.
# 7.Revised: Aug 24, 2014.
########################################################
# Contents:
# 1. generate data
# 2. do Johansen cointegeration test
# 3. do impulse response analysis
# 4. do forecast error variance decomposition
#############################################################
# 0. initializing
# (1) set path
setwd('F:/programe/book/R with application to financial quantitive analysis/CH-10')
rm(list=ls())

# (2) load packages
library(urca)                                 # for unit root and cointegration tests for time series data
library(vars)                                 # install package ¡°vars¡±

# 1. generate data
set.seed(12345)
u1 <- rnorm(500)
u2 <- arima.sim(list(ar = 0.6), n = 500)
u3 <- arima.sim(list(ar = 0.4), n = 500)
y1 <- cumsum(u1)
y2 <- 0.4*y1+u2         
y3 <- 0.8*y1+u3

# 2. do Johansen cointegeration test
data <- data.frame(y1=y1, y2=y2, y3=y3)           # Organize data to the data frame
model.vecm <- ca.jo(data)                         # Johansen Cointegration Test 
slotNames(model.vecm)                             # The Slots in "model.vecm"
head(model.vecm@x)
model.vecm@type
model.vecm@model
model.vecm@test.name
summary(model.vecm)

vecm.trlong <- ca.jo(data, type="trace", spec="longrun")  # Test with trace statistic and longrun VECM
summary(vecm.trlong)
vecm.trshort <- ca.jo(data, type="trace", spec="transitory") # Test with trace statistic and transitory VECM
summary(vecm.trshort)

cajorls(model.vecm,r=2)                           # estimate VECM
model.var <- vec2var(model.vecm,r=2)              # VECM as var in levels
model.var

# 3. do impulse response analysis
irf.y1 <- irf(model.var,impulse="y1")
par(mfrow=c(3,3))
plot(irf.y1)
irf.y2 <- irf(model.var,impulse="y2")
plot(irf.y2)
irf.y3 <- irf(model.var,impulse="y3")
plot(irf.y3)
par(mfrow=c(1,1))

# 4. do forecast error variance decomposition
var.fevd <- fevd(model.var)
plot(var.fevd)

