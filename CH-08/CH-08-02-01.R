########################################################
# Description:
# 1.for Book 'R with applications to financial quantitive analysis'
# 2.Chapter: CH-08-02
# 3.Section: 8.2
# 4.Purpose: mean-CVaR portfolio model
# 5.Author: Qifa Xu
# 6.Date: Jan 11, 2014.
# 7.Revised: Aug 20, 2014.
########################################################
# Contents:
# 1. solve mean-CVaR portfolios
# 2. construct mean-CVaR portfolio frontiers
#########################################################

# 0. initialize
# (1) set work path
setwd('F:/programe/book/R with application to financial quantitive analysis/CH-08')

# (2) set workspace
rm(list=ls())
options(digits=4, width=70)

# (3) load packages and data
library(fPortfolio)                           # for solve portfolio model
load('rtsSixIndices.RData')                   # load data from last example

# 1. solve mean-CVaR portfolios
# (1) compute a feasible portfolio
feasSpec <- portfolioSpec()
setType(feasSpec) <- 'CVaR'
nAssets <- ncol(rts)
setWeights(feasSpec) <- rep(1/nAssets, times=nAssets)
setSolver(feasSpec) <- 'solveRglpk'
feasPortfolio <- feasiblePortfolio(data=rts, spec=feasSpec, constraints='LongOnly')
print(feasPortfolio)

weightsPie(feasPortfolio, radius=0.7)
text <- 'feasible mean-CVaR portfolio'
mtext(text, side=3, line=1.5, font=2, cex=0.7, adj=0)
weightedReturnsPie(feasPortfolio, radius=0.8, legend=FALSE)
covRiskBudgetsPie(feasPortfolio, radius=0.9, legend=FALSE)

# (2) compute portfolio with the lowest risk for a given return
minriskSpec <- portfolioSpec()
setType(minriskSpec) <- 'CVaR'
setAlpha(minriskSpec) <- 0.05
setSolver(minriskSpec) <- 'solveRglpk'
setTargetReturn(minriskSpec) <- getTargetReturn(feasPortfolio@portfolio)['mean']
minriskPortfolio <- efficientPortfolio(data=rts, spec=minriskSpec, constraints='LongOnly')
print(minriskPortfolio)

weightsPie(minriskPortfolio, radius=0.7)
text <- 'lowest risk mean-CVaR portfolio'
mtext(text, side=3, line=1.5, font=2, cex=0.7, adj=0)
weightedReturnsPie(minriskPortfolio, radius=0.8)
covRiskBudgetsPie(minriskPortfolio, radius=0.9)

# (3) compute the global minimum risk portfolio
globminSpec <- portfolioSpec()
setType(globminSpec) <- 'CVaR'
setAlpha(globminSpec) <- 0.05
setSolver(globminSpec) <- 'solveRglpk'
setTargetReturn(globminSpec) <- getTargetReturn(feasPortfolio@portfolio)['mean']
globminPortfolio <- minriskPortfolio(data=rts, spec=globminSpec, constraints='LongOnly')
print(globminPortfolio)

weightsPie(globminPortfolio, radius=0.7)
text <- 'global minmum risk portfolio'
mtext(text, side=3, line=1.5, font=2, cex=0.7, adj=0)
weightedReturnsPie(globminPortfolio, radius=0.8)
covRiskBudgetsPie(globminPortfolio, radius=0.9)


# 2. construct mean-CVaR portfolio frontiers
# (1) compute long-only portfolio frontiers
longSpec <- portfolioSpec()
setType(longSpec) <- 'CVaR'
setAlpha(longSpec) <- 0.05
setNFrontierPoints(longSpec) <- 5
setSolver(longSpec) <- 'solveRglpk'
longFrontier <- portfolioFrontier(data=rts, spec=longSpec, constraints='LongOnly')
print(longFrontier)

setNFrontierPoints(longSpec) <- 25
longFrontier <- portfolioFrontier(data=rts, spec=longSpec, constraints='LongOnly')
text <- 'mean-CVaR portfolio long only constraints'
tailoredFrontierPlot(object=longFrontier, mText=text, risk='CVaR')

weightsPlot(longFrontier, mtext=FALSE)
mtext(text, side=3, line=3, font=2, cex=0.9)
weightedReturnsPlot(longFrontier, mtext=FALSE)
covRiskBudgetsPlot(longFrontier, mtext=FALSE)


