########################################################
# Description:
# 1.for Book 'R with applications to financial quantitive analysis'
# 2.Chapter: CH-08
# 3.Section: 8.3
# 4.Purpose: portfolio backtesting
# 5.Author: Qifa Xu
# 6.Date: Jan 11, 2014.
# 7.Revised: Aug 20, 2014.
########################################################
# Contents:
# 1. read data from yahoo
# 2. compute financial risk
# 3. do rolling analysis
#########################################################

# 0. initialize
# (1) set work path
setwd('F:/programe/book/R with application to financial quantitive analysis/CH-08')
rm(list=ls())

# (2) load packages
library(PerformanceAnalytics)                 # for performance and risk analysis
library(fPortfolio)                           # for solve portfolio model
library(fPortfolioBacktest)                   # for portfolio backtesting
library(tseries)                              # for read data from Yahoo

# 1. read data from yahoo
start <- '2000-12-01'
end <- '2013-12-31'
# prc.SSEC <- get.hist.quote(instrument='^SSEC', start=start, end=end, quote='Close', provider='yahoo')
prc.SSEC <- get.hist.quote(instrument='000001.SS', start=start, end=end, quote='Close', provider='yahoo')
prc.PFYH <- get.hist.quote(instrument='600000.SS', start=start, end=end, quote='Close', provider='yahoo')
prc.GZBY <- get.hist.quote(instrument='600004.SS', start=start, end=end, quote='Close', provider='yahoo')
prc.WGGF <- get.hist.quote(instrument='600005.SS', start=start, end=end, quote='Close', provider='yahoo')
prc.ZGGM <- get.hist.quote(instrument='600007.SS', start=start, end=end, quote='Close', provider='yahoo')

prc.ALL <- merge(prc.SSEC, prc.PFYH, prc.GZBY, prc.WGGF, prc.ZGGM)
rts.ALL <- diff(log(prc.ALL))
rts <- na.omit(rts.ALL)
colnames(rts) <- c('SSEC', 'PFYH', 'GZBY', 'WGGF', 'ZGGM')

# 2. compute financial risk
# (1) compute VaR
confidence <- 0.95
VaR.hist <- VaR(rts, p=confidence, method="historical")    # using historical simulation
VaR.gaus <- VaR(rts, p=confidence, method="gaussian")      # using Gaussian approximation
VaR.modi <- VaR(rts, p=confidence, method="modified")      # using modified Cornish Fisher calc to take non-normal distribution into account

# (2) compute CVaR or ES
CVaR.hist <- CVaR(rts, p=confidence, method="historical")    # using historical simulation
CVaR.gaus <- CVaR(rts, p=confidence, method="gaussian")      # using Gaussian approximation
CVaR.modi <- CVaR(rts, p=confidence, method="modified")      # using modified Cornish Fisher calc to take non-normal distribution into account

# (3) compute CDD or CDaR (some other packages:fPortfolioBacktest, fBasics, timeSeries, tseries)
DD <- findDrawdowns(rts)                                         # compute DD
chart.Drawdown(rts, legend.loc='topright', main='DDͼ')          # plot DD
MDD <- maxDrawdown(rts)
CDD <- CDD(rts, p=confidence)

# (4) show results
risk <- round(rbind(VaR.hist, VaR.gaus, VaR.modi, CVaR.hist, CVaR.gaus, CVaR.modi, MDD, CDD), digits=4)
rownames(risk) <- c('VaR.hist', 'VaR.gaus', 'VaR.modi', 'CVaR.hist', 'CVaR.gaus', 'CVaR.modi', 'MDD', 'CDD')
print(risk)

# 3. do rolling analysis
# (1) backtest a portfolio
rtsSpec <- portfolioSpec()
setType(rtsSpec) <- 'CVaR'                        # also can use 'MV', 'VaR' or others
setAlpha(rtsSpec) <- 0.05
rtsConstraints <- 'LongOnly'
rtsBacktest <- portfolioBacktest()
setWindowsHorizon(rtsBacktest) <- '18m'
setSmootherLambda(rtsBacktest) <- '6m'
names.assets <- colnames(rts)
rtsFormula <- as.formula(paste(paste(names.assets[1],'~'), paste(names.assets[2:length(names.assets)], collapse='+')))
rtsPortfolios <- portfolioBacktesting(formula=rtsFormula, data=timeSeries(rts), spec=rtsSpec,
                                      constraints=rtsConstraints,backtest=rtsBacktest, trace=FALSE)
names(rtsPortfolios)
Weights <- round(100*rtsPortfolios$weights,2)
barplot(t(Weights), col=seqPalette(length(names.assets), 'Blues'), legend.text=names.assets[2:length(names.assets)])

# (2) smooth the weights from a backtest
backtest <- portfolioBacktest()
setSmootherInitialWeights(backtest) <- rep(1/(length(names.assets)-1), times=length(names.assets)-1)
setSmootherLambda(backtest) <- '12m'
rtsSmooth <- portfolioSmoothing(object=rtsPortfolios, backtest=rtsBacktest)
smoothWeights <- round(100*rtsSmooth$smoothWeights,2)[1:12,]
barplot(t(smoothWeights), col=seqPalette(length(names.assets), 'Blues'), legend.text=names.assets[2:length(names.assets)])

# (3) plot and print backtesting results
backtestPlot(rtsSmooth)
netPerformance(rtsSmooth)
backtestStats(rtsSmooth)

# (4) write your own statistics functions (such as: DaR, CDaR, Shapiro-Wilk test)
CDaRstats <- backtestStats(rtsSmooth, FUN='rollingCDaR')
head(CDaRstats)
par(mfrow=c(2,2))
plot(backtestStats(rtsSmooth, FUN='rollingSigma'), main='sigma')
plot(backtestStats(rtsSmooth, FUN='rollingVaR'), main='VaR')
plot(backtestStats(rtsSmooth, FUN='rollingCVaR'), main='CVaR')
plot(backtestStats(rtsSmooth, FUN='rollingCDaR'), main='CDaR')

