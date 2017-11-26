########################################################
# Description:
# 1.for Book 'R with applications to financial quantitive analysis'
# 2.Chapter: CH-10-01
# 3.Section: 10.1
# 4.Purpose: Johansen Cointegeration Test With Real Data Application
# 5.Author: Lin Kang, polished by Qifa Xu
# 6.Date: May 10, 2014.
# 7.Revised: Aug 24, 2014.
########################################################
# Contents:
# 1. read data
# 2. do unit root test
# 3. do Johansen cointegration test
# 4. do Johansen cointegration test with structural shift
#############################################################
# 0. initializing
# (1) set path
setwd('F:/programe/book/R with application to financial quantitive analysis/CH-10')
rm(list=ls())

# (2) load packages
library('RODBC')                              # for reading EXCEL file
library(urca)                                 # for unit root and cointegration tests for time series data
library(tseries)                              # for time series
library(xts)                                  # for time series
library(vars)                                 # for VAR

# 1. read data
X <- odbcConnectExcel('Data-10-01.xls')    
Y <- sqlFetch (X,'Sheet2')
Y <- as.xts(Y[,-1], order.by=Y[,1])
LNY <- log(Y)
colnames(LNY) <- c('LNLD', 'LNND', 'LNSH', 'LNRJ')
matplot(LNY, type='l', lty=1:ncol(LNY), col=1:ncol(LNY), ylim=c(7.0, 10.5), ylab='对数价格')
legend('topleft', legend=colnames(LNY), lty=1:ncol(LNY), col=1:ncol(LNY))

# 2. do unit root test
# (1) The level sequence
ADF.LEVEL <- PP.LEVEL <- KPSS.LEVEL <- matrix(NA, nrow=2, ncol=ncol(LNY))
for (j in 1:ncol(LNY)){
  ADF.LEVEL[,j] <- c(adf.test(LNY[,j])$stat, adf.test(LNY[,j])$p.value)
  PP.LEVEL[,j] <- c(pp.test(LNY[,j])$stat, pp.test(LNY[,j])$p.value)
  KPSS.LEVEL[,j] <- c(kpss.test(LNY[,j])$stat, kpss.test(LNY[,j])$p.value)
}
colnames(ADF.LEVEL) <- colnames(LNY); rownames(ADF.LEVEL) <- c('stat', 'p.value')
colnames(PP.LEVEL) <- colnames(LNY); rownames(PP.LEVEL) <- c('stat', 'p.value')
colnames(KPSS.LEVEL) <- colnames(LNY); rownames(KPSS.LEVEL) <- c('stat', 'p.value')
print(ADF.LEVEL); print(PP.LEVEL); print(KPSS.LEVEL)

# (2) The difference sequence
DLNY <- na.omit(diff(LNY))
ADF.DIFF <- PP.DIFF <- KPSS.DIFF <- matrix(NA, nrow=2, ncol=ncol(LNY))
for (j in 1:ncol(DLNY)){
  ADF.DIFF[,j] <- c(adf.test(DLNY[,j])$stat, adf.test(DLNY[,j])$p.value)
  PP.DIFF[,j] <- c(pp.test(DLNY[,j])$stat, pp.test(DLNY[,j])$p.value)
  KPSS.DIFF[,j] <- c(kpss.test(DLNY[,j])$stat, kpss.test(DLNY[,j])$p.value)
}
colnames(ADF.DIFF) <- colnames(DLNY); rownames(ADF.DIFF) <- c('stat', 'p.value')
colnames(PP.DIFF) <- colnames(DLNY); rownames(PP.DIFF) <- c('stat', 'p.value')
colnames(KPSS.DIFF) <- colnames(DLNY); rownames(KPSS.DIFF) <- c('stat', 'p.value')
print(ADF.DIFF); print(PP.DIFF); print(KPSS.DIFF)


# 3. do Johansen cointegration test
model.eigen <- ca.jo(LNY, type="eigen", ecdet="none", spec="longrun")      # based on maximal eigenvalue statistic
summary(model.eigen)
model.trace <- ca.jo(LNY, type="trace",ecdet="none", spec="longrun")           # based on maximal trace statistic
summary(model.trace)

# 4. do Johansen cointegration test with structural shift
model.break <- cajolst(LNY)        
summary(model.break)
model.break@bp


