########################################################
# Description:
# 1.for Book 'R with applications to financial quantitive analysis'
# 2.Chapter: CH-08-04
# 3.Section: 8.4
# 4.Purpose: large scale portfolio (application)
# 5.Author: Qifa Xu
# 6.Date: Jan 11, 2014.
# 7.Revised: Aug 20, 2014.
########################################################
# Contents:
# 1. read data from Yahoo
# 2. portfolio objective function and constraints
# 3. portfolio optimization with DEoptim
#########################################################

# 0. initialize
# (1) set work path
setwd('F:/programe/book/R with application to financial quantitive analysis/CH-08')
rm(list=ls())

# (2) load package
library(quantmod)                             # do quantitative financial modelling
library(DEoptim)                              # do differential evolution optimization
library(PerformanceAnalytics)                 # do performance and risk analysis
library(PortfolioAnalytics)                   # do portfolio analysis

# 1. read data from Yahoo
# (1) get data
tickers <- c( "VNO" , "VMC" , "WMT" , "WAG" , "DIS" , "WFC" , "WDC" ,
              "WY" , "WHR" , "WMB" , "WEC" , "XEL" , "XRX" , "XLNX" ,"ZION" ,"MMM" ,
              "ABT", "ADBE" , "AMD" , "AET" , "AFL" , "APD" , "ARG" ,"AA" , "AGN" ,
              "ALTR" , "MO" , "AEP" , "AXP" , "AIG" , "AMGN" , "APC" ,"ADI" , "AON" ,
              "APA", "AAPL" , "AMAT" ,"ADM" , "T" , "ADSK" , "ADP" , "AZO" , "AVY" ,
              "AVP", "BHI" , "BLL" , "BAC" , "BK" , "BCR" , "BAX" , "BBT" , "BDX" ,
              "BMS" , "BBY" , "BIG" , "HRB" , "BA" , "BMY" , "CA" , "COG" ,
              "CPB" , "CAH" , "CCL" , "CAT" , "CELG" , "CNP" , "CTL" , "CERN" ,
              "SCHW" , "CVX" , "CB" , "CI" ,"CINF" ,"CTAS" , "CSCO" , "C" , "CLF" ,
              "CLX", "CMS" , "KO" , "CCE" , "CL" , "CMCSA" ,"CMA" , "CSC" , "CAG" ,
              "COP" , "ED" , "GLW" , "COST" , "CSX" , "CMI" , "CVS" ,
              "DHR" , "DE")
print(tickers)
getSymbols(tickers, from='2000-12-01', to='2010-12-31')

# (2) compute returns
P <- NULL
seltickers <- NULL
for (ticker in tickers){
  tmp <- Cl(to.monthly(eval(parse(text=ticker))))
  if (is.null(P)) {timeP <- time(tmp)}
  if (any(time(tmp) != timeP)) next
  else P <- cbind(P, as.numeric(tmp))
  seltickers <- c(seltickers, ticker)
}
P <- xts(P,order.by=timeP)
colnames(P) <- seltickers
R <- diff(log(P))
R <- R[-1,]
dim(R)
mu <- colMeans(R)
sigma <- cov(R)


# 2. portfolio objective function and constraints
# (1) define objective function
obj <- function(w) {
  if (sum(w) == 0) {
    w <- w + 1e-2
  }
  w <- w / sum(w)
  CVaR <- ES(weights=w, method='gaussian',portfolio_method='component',mu=mu, sigma = sigma)
  tmp1 <- CVaR$ES
  tmp2 <- max(CVaR$pct_contrib_ES - 0.05, 0)
  out <- tmp1 + 1e3 * tmp2
  out
}

# (2) add constraints
N <- ncol(R)
minw <- 0
maxw <- 1
lower <- rep(minw, N)
upper <- rep(maxw, N)

eps <- 0.025
weight_seq <- generatesequence(min=minw, max=maxw, by=.001, rounding=3)
# rpconstraint <- constraint(assets=N, min_sum=(1-eps), max_sum=(1+eps), min=lower, max=upper, weight_seq=weight_seq)
set.seed(12345)
pspec <- portfolio.spec(assets=N, weight_seq=weight_seq)
pspec <- add.constraint(pspec, type="weight_sum", min_sum=(1-eps), max_sum=(1+eps))
rp<- random_portfolios(portfolio=pspec, permutations=N*10)               # generate random portfolios
rp <-rp/rowSums(rp)

# 3. portfolio optimization with DEoptim
controlDE <- list(reltol=.000001,steptol=150,itermax=5000,trace=250,strategy=6,c=.4,NP=as.numeric(nrow(rp)),initialpop=rp) 
set.seed(12345)
tmp <- proc.time()
out <- DEoptim(fn = obj, lower = lower, upper = upper, control = controlDE)
proc.time()-tmp                            # time difference
out$optim$iter
out$optim$bestval
weights <- out$optim$bestmem
weights <- round(weights/sum(weights), digits=3)
names(weights) <- tickers
weights[weights!=0]
barplot(sort(weights[weights!=0]), horiz=TRUE)


