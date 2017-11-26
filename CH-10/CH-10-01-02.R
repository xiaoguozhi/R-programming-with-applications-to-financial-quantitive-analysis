########################################################
# Description:
# 1.for Book 'R with applications to financial quantitive analysis'
# 2.Chapter: CH-10-01
# 3.Section: 10.1
# 4.Purpose: Unit Root Test and Engel-Granger Two-Step Procedure
# 5.Author: Lin Kang, polished by Qifa Xu
# 6.Date: May 10, 2014.
# 7.Revised: Aug 24, 2014.
########################################################
# Contents: 
# 1. read data from EXCEL file
# 2. do unit root tests
# 3. do cointegration test: Engel-Granger Two-Step Procedure
# 4. Error Correction Model
# 5. Granger causality test
#############################################################
# 0. Initializing
# (1) set path
setwd('F:/programe/book/R with application to financial quantitive analysis/CH-10')
rm(list=ls())

# (2) load packages
library('RODBC')                              # for reading EXCEL file
library(zoo)                                  # for time series analysis
library(tseries)                              # for time series analysis
library(urca)                                 # for unit root and cointegration tests for time series data

# 1. read data from EXCEL file
X <- odbcConnectExcel('Data-10-01.xls')    
Y <- sqlFetch (X,'Sheet1')        
SH <- Y[["SH"]]                               # read Shanghai composite index data            
SZ <- Y[["SZ"]]                               # read Shenzhen Stock Market data
LNSH <- log(SH)                               # SH in logs
LNSZ <- log(SZ)                               # SZ in logs

par(mfrow=c(1,2))
plot(LNSH, xlab='时间', ylab='对数价格', main='SH', type='l')
plot(LNSZ, xlab='时间', ylab='对数价格', main='SZ', type='l')
# ts.plot(LNSH, LNSZ,lty=c(1,2),lwd=c(1,1.4),xlab="2011.1-2013.12",xlim=c(2011,2013))
# legend("topright",c("LNSH","LNSZ"),lty=c(1,2),lwd=c(1,1.4))
# ts.plot(diff(ts.LNSH),diff(ts.LNSZ),lty=c(1,2),lwd=c(1,1.4),xlab="2011.1-2013.12")
# legend("topright",c("DLNSH","DLNSZ"),lty=c(1,2),lwd=c(1,1.4))

# 2. do unit root tests
# (1) the level sequence
adf.test(LNSH)                                # ADF test of the level sequence
adf.test(LNSZ)
pp.test(LNSH)                                 # PP test of the level sequence
pp.test(LNSZ)
kpss.test(LNSH)                               # KPSS test of the level sequence
kpss.test(LNSZ)

# (2) the difference sequence
DLNSH <- diff(LNSH)                           # difference operation
DLNSZ <- diff(LNSZ)
adf.test(DLNSH)                               # ADF test of the difference sequence
adf.test(DLNSZ)
pp.test(DLNSH)                                # PP test of the difference sequence
pp.test(DLNSZ)
kpss.test(DLNSH)                              # KPSS test of the difference sequence
kpss.test(DLNSZ)

# 3. save results
save(LNSH, LNSZ, DLNSH, DLNSZ, file='EGData.RData')

