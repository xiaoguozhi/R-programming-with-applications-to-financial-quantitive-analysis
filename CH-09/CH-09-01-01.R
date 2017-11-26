########################################################
# Description:
# 1.for Book 'R with applications to financial quantitive analysis'
# 2.Chapter: CH-09-01
# 3.Section: 9.1
# 4.Purpose: basic CAPM
# 5.Author: Qifa Xu
# 6.Date: Aug 20, 2014.
# 7.Revised: Aug 22, 2014.
########################################################
# Contents:
# 1. read data from Yahoo
# 2. estimate CML and SML
# 3. do CAPM modeling
# 4. save results
#########################################################

# 0. initialize
# (1) set work path and workspace
setwd('F:/programe/book/R with application to financial quantitive analysis/CH-09')
rm(list=ls())

# (2) load packages
library(PerformanceAnalytics)                 # for performance and risk analysis
library(tseries)                              # for read data from Yahoo
source('Sub-09.R')

# 1. read data from Yahoo
start <- '2001-01-01'
end <- '2013-12-31'
prc.SSEC <- get.hist.quote(instrument='000001.SS', start=start, end=end, quote='Close', provider='yahoo')
prc.SZSE <- get.hist.quote(instrument='399001.SZ', start=start, end=end, quote='Close', provider='yahoo')
prc.PFYH <- get.hist.quote(instrument='600000.SS', start=start, end=end, quote='Close', provider='yahoo')
prc.GZBY <- get.hist.quote(instrument='600004.SS', start=start, end=end, quote='Close', provider='yahoo')
prc.WGGF <- get.hist.quote(instrument='600005.SS', start=start, end=end, quote='Close', provider='yahoo')
prc.PAYH <- get.hist.quote(instrument='000001.SZ', start=start, end=end, quote='Close', provider='yahoo')
prc.WKA <- get.hist.quote(instrument='000002.SZ', start=start, end=end, quote='Close', provider='yahoo')
prc.SZYA <- get.hist.quote(instrument='000006.SZ', start=start, end=end, quote='Close', provider='yahoo')

prc.ALL <- merge(prc.SSEC, prc.SZSE, prc.PFYH, prc.GZBY, prc.WGGF, prc.PAYH, prc.WKA, prc.SZYA)
rts.ALL <- diff(log(prc.ALL))
rts <- na.omit(rts.ALL)
colnames(rts) <- c('SSEC', 'SZSE', 'PFYH', 'GZBY', 'WGGF', 'PAYH', 'WKA', 'SZYA')

# 2. estimate CML and SML
# (1) change data frequency
time.ind <- as.yearmon(levels(as.factor(substr(index(rts), start=1, stop=7))))
rts.mon <- as.xts(apply(rts, 2, conv_to_mon), order.by=time.ind)             # covert to monthly data
Rf <- 0.003/12                                                               # set rate of riskfree

# (2) compute CML and SML
CAPM.RiskPremium(Ra=rts.mon[,3:8], Rf=Rf)
CAPM.CML.slope(Rb=rts.mon[,1,drop=FALSE], Rf=Rf)
CAPM.CML.slope(Rb=rts.mon[,2,drop=FALSE], Rf=Rf)
CAPM.SML.slope(Rb=rts.mon[,1,drop=FALSE], Rf=Rf)                             # has an error
CAPM.SML.slope(Rb=rts.mon[,2,drop=FALSE], Rf=Rf)                             # has an error
CAPM.CML(Ra=rts.mon[,3:8], Rb=rts.mon[,1:2], Rf=Rf)    # calculates the expected return of the asset against the benchmark Capital Market Line

# (3) plot CML and SML
CAPM_CML_PLOT(Rb=rts.mon[,1:2], Rf=Rf) 
CAPM_SML_PLOT(Ra=rts.mon[,3:8], Rb=rts.mon[,1:2], Rf=Rf) 

# 3. do CAPM modeling
# (1) estimate alphas and betas
CAPM.alpha(Ra=rts.mon[,3:8], Rb=rts.mon[,1:2], Rf=Rf)
CAPM.beta(Ra=rts.mon[,3:8], Rb=rts.mon[,1:2], Rf=Rf)

# (2) make CAPM table
tab.plot <- table.CAPM(Ra=rts.mon[,3:8], Rb=rts.mon[,1:2], Rf=Rf)
textplot(tab.plot, rmar=0.8, cmar=1.5,  max.cex=.9, halign="center", valign="top", row.valign="center",
         wrap.rownames=15, wrap.colnames=10, mar = c(0,0,3,0)+0.1)
title(main="CAPM-Related Statistics")

# 4. save results
save(rts, rts.mon, file='rtsMonth.RData')
