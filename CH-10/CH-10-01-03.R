########################################################
# Description:
# 1.for Book 'R with applications to financial quantitive analysis'
# 2.Chapter: CH-10-01
# 3.Section: 10.1
# 4.Purpose: E-G Test through simulation and real application
# 5.Author: Lin Kang, polished by Qifa Xu
# 6.Date: May 10, 2014.
# 7.Revised: Aug 24, 2014.
########################################################
# Contents:
# 1. do simulation for cointegration test
# 2. do real application
#############################################################
# 0. Initializing
# (1) set path
setwd('F:/programe/book/R with application to financial quantitive analysis/CH-10')
rm(list=ls())

# (2) load packages
library(urca)                                 # for unit root and cointegration tests for time series data
library(vars)                                 # install package ¡°vars¡±

# 1. do simulation for cointegration test
# (1) generate data
set.seed(12345)
u <- rnorm(500)
x <- cumsum(u)
y <- x+u         

# (2) do cointegration test: Engel-Granger Two-Step Procedure
model.lm <- lm(y~x)                                     # Cointegration regression
summary(model.lm)
re.lm <- residuals(model.lm)                            # Extract residual
summary(ur.df(re.lm,type="none",selectlags="AIC"))      # Cointegration test

# (3) estimate an error correction model
dy <- diff(y)                                           # difference operation
dx <- diff(x)
error.term <- re.lm[-length(y)]                         # Remove missing values
data.ecm <- data.frame(dy=dy,dx=dx, error.term=error.term)   # Organize data to the data frame
model.ecm <- lm(dy~dx+error.term, data=data.ecm)             # creat error correction model
summary(model.ecm)

# 2. do real application
# (1) load data from last example
load('EGData.RData')

# (2) do cointegration test: Engel-Granger Two-Step Procedure
model.lm <- lm(LNSH~LNSZ)                           # Cointegration regression 
summary(model.lm)
re.lm<- resid(model.lm)                             # Extract residual
summary(ur.df(re.lm,type="none",selectlags="AIC"))  # Cointegration test 

# (3) estimate Error Correction Model
error.term <-re.lm[-length(LNSH)]             # Remove missing values
data.ecm <- data.frame(dy1=DLNSH,dx1=DLNSZ, error.term=error.term)  # Organize data to the data frame
model.ecm <- lm(DLNSH~DLNSZ+error.term,data=data.ecm)               # creat error correction model
summary(model.ecm)

# (4) do Granger causality test
data.causal <- data.frame(LNSH=LNSH, LNSZ=LNSZ)         # Organize data to the data frame
model.var <- VAR(data.causal,p=1)
LNSH.causal <- causality(model.var ,cause="LNSH")       # Granger causal test
LNSH.causal
LNSZ.causal <- causality(model.var ,cause="LNSZ")
LNSZ.causal
