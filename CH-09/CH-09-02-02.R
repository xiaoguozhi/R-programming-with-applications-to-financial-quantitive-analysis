########################################################
# Description:
# 1.for Book 'R with applications to financial quantitive analysis'
# 2.Chapter: CH-09-02
# 3.Section: 9.2
# 4.Purpose: APT
# 5.Author: Qifa Xu
# 6.Date: Aug 20, 2014.
# 7.Revised: Aug 24, 2014.
########################################################
# Contents:
# 1. setup dataset
# 2. estimate factor loadings
# 3. estimate APT model
#########################################################

# 0. initialize
# (1) set work path and workspace
setwd('F:/programe/book/R with application to financial quantitive analysis/CH-09')
rm(list=ls())

# (2) load packages
library(xlsx)                         # read data from EXCEL file
library(tseries)                              # for read data from Yahoo

# 1. setup dataset
# (1) read data from Yahoo
start <- '2011-01-01'
end <- '2013-12-31'
prc.PFYH <- get.hist.quote(instrument='600000.SS', start=start, end=end, quote='Close', provider='yahoo')
prc.GZBY <- get.hist.quote(instrument='600004.SS', start=start, end=end, quote='Close', provider='yahoo')
prc.WGGF <- get.hist.quote(instrument='600005.SS', start=start, end=end, quote='Close', provider='yahoo')
prc.ZGSH <- get.hist.quote(instrument='600028.SS', start=start, end=end, quote='Close', provider='yahoo')
prc.YYSC <- get.hist.quote(instrument='600655.SS', start=start, end=end, quote='Close', provider='yahoo')
prc.PAYH <- get.hist.quote(instrument='000001.SZ', start=start, end=end, quote='Close', provider='yahoo')
prc.WKA <- get.hist.quote(instrument='000002.SZ', start=start, end=end, quote='Close', provider='yahoo')
prc.SZYA <- get.hist.quote(instrument='000006.SZ', start=start, end=end, quote='Close', provider='yahoo')
prc.NCP <- get.hist.quote(instrument='000061.SZ', start=start, end=end, quote='Close', provider='yahoo')
prc.ZXTX <- get.hist.quote(instrument='000063.SZ', start=start, end=end, quote='Close', provider='yahoo')

# (2) compute returns of stocks
prc.ALL <- merge(prc.PFYH, prc.GZBY, prc.WGGF, prc.ZGSH, prc.YYSC, prc.PAYH, prc.WKA, prc.SZYA, prc.NCP, prc.ZXTX)
rts.ALL <- diff(log(prc.ALL))
rts <- na.omit(rts.ALL)
colnames(rts) <- c('PFYH', 'GZBY', 'WGGF', 'ZGSH', 'YYSC', 'PAYH', 'WKA', 'SZYA', 'NCP', 'ZXTX')
n.assets <- ncol(rts)
obs.assets <- nrow(rts)

# 2. estimate factor loadings
# (1) determin the nubmer of factors
pvalue <- 0
for (k in 2:n.assets){
  cat('##################################', '\n')
  cat('the current k is:', k, '\n')
  cat('##################################', '\n')
  if (pvalue < 0.05){
    stat.fct <- factanal(rts, factors=k, method='mle')
    pvalue <- stat.fct$PVAL
  }else{
    print('the number of factos is')
    print(k-1)
    print(stat.fct)
    break
  }
}

# (2) show first two loadings
par(mfrow=c(2,1))
loads <- loadings(stat.fct)
barplot(loads[,1], main='因子1')
barplot(loads[,2], main='因子2')
par(mfrow=c(1,1))

# (3) rotate factor loadings
loads.rot <- loadings(varimax(loads))
# loads.rot <- loadings(promax(loads))

# 3. estimate APT model
# (1) set variables
R.bar <- colMeans(rts)
betas <- loads.rot[,1:ncol(loads.rot)]

# (2) make model
model.lm <- lm(R.bar~betas)
summary(model.lm)
