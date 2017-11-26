########################################################
# Description:
# 1.for Book 'R with applications to financial quantitive analysis'
# 2.Chapter: CH-08-03
# 3.Section: 8.3
# 4.Purpose: higher moments portfolio
# 5.Author: Qifa Xu
# 6.Date: Aug 20, 2014.
# 7.Revised: Aug 20, 2014.
########################################################
# Contents:
# 1. read data and compute returns
# 2. compute descriptive statistics
# 3. compute covariance, coskewness and cokurtosis matrix
# 4. solve individual objective optimization
# 5. solve PGP optimization
# 6. compute equal weights portfolio
#########################################################

# 0. initialize
# (1) set work path
setwd('F:/programe/book/R with application to financial quantitive analysis/CH-08')
rm(list=ls())

# (2) load package
library(tseries)                              # for reading data from Yahoo
library(xlsx)                                 # for reading data from EXCEL file
library(xts)                                  # for time series
library(Rglpk)                                # solve linear programming
source('Sub-08.R')

# 1. read data and compute returns
# (1) read data from  EXCEL file or Yahoo
DJIA <- read.xlsx(file='stockprice.xlsx', sheetName='DJIA')
HKHS <- read.xlsx(file='stockprice.xlsx', sheetName='HKHS')
JN <- read.xlsx(file='stockprice.xlsx', sheetName='JN')
STI <- read.xlsx(file='stockprice.xlsx', sheetName='STI')
CISSM <- read.xlsx(file='stockprice.xlsx', sheetName='CISSM')
prc.DJIA <- xts(DJIA[,'Close'], order.by=DJIA[,'Date'])
prc.HKHS <- xts(HKHS[,'Close'], order.by=HKHS[,'Date'])
prc.JN <- xts(JN[,'Close'], order.by=JN[,'Date'])
prc.STI <- xts(STI[,'Close'], order.by=STI[,'Date'])
prc.CISSM <- xts(CISSM[,'Close'], order.by=CISSM[,'Date'])
prc.ALL <- merge(prc.DJIA, prc.HKHS, prc.JN, prc.STI, prc.CISSM)

# start <- '2011-01-01'
# end <- '2013-12-31'
# prc.DJIA <- get.hist.quote(instrument='DJIA', start=start, end=end, quote='Close', provider='yahoo')
# prc.HKHS <- get.hist.quote(instrument='^HSI', start=start, end=end, quote='Close', provider='yahoo')
# prc.JN <- get.hist.quote(instrument='^N225', start=start, end=end, quote='Close', provider='yahoo')
# prc.STI <- get.hist.quote(instrument='^STI', start=start, end=end, quote='Close', provider='yahoo')
# prc.CISSM <- get.hist.quote(instrument='000001.SS', start=start, end=end, quote='Close', provider='yahoo')
# prc.ALL <- merge(prc.DJIA, prc.HKHS, prc.JN, prc.STI, prc.CISSM)

# (2) compute returns
rts.ALL <- 100*diff(log(prc.ALL))
rts <- na.omit(rts.ALL)
colnames(rts) <- c('DJIA', 'HKHS', 'JN', 'STI', 'CISSM')
head(rts)
TT <- nrow(rts)
N <- ncol(rts)

# 2. compute descriptive statistics
stat.DJIA <- stat(x=as.numeric(rts[,'DJIA']), plot.it=FALSE)
stat.HKHS <- stat(x=as.numeric(rts[,'HKHS']), plot.it=FALSE)
stat.JN <- stat(x=as.numeric(rts[,'JN']), plot.it=FALSE)
stat.STI <- stat(x=as.numeric(rts[,'STI']), plot.it=FALSE)
stat.CISSM <- stat(x=as.numeric(rts[,'CISSM']), plot.it=FALSE)

tab <- cbind(c(stat.DJIA$stats, stat.DJIA$test), c(stat.HKHS$stats, stat.HKHS$test), c(stat.JN$stats, stat.JN$test),
             c(stat.STI$stats, stat.STI$test), c(stat.CISSM$stats, stat.CISSM$test))
colnames(tab) <- colnames(rts)
print(tab)

# 3. compute covariance, coskewness and cokurtosis matrix
# H <- CoVariance(rts, rts)
# S <- CoSkewness(rts, rts)
# K <- CoKurtosis(rts, rts)
comoments <- HSK(R=rts)
H <- comoments$H
S <- comoments$S
K <- comoments$K

# 4. solve individual objective optimization
# (1) max expected return
obj <- colMeans(rts)
mat <- rbind(matrix(rep(1, N), nrow=1, ncol=N), diag(N), -diag(N))
dir <- c('==', '<=','<=','<=','<=','<=','<=','<=','<=','<=','<=')
rhs <- c(1, rep(1, N), rep(0, N))
max <- TRUE
maxexprts <- Rglpk_solve_LP(obj, mat, dir, rhs, max=max)
w.exp <- maxexprts$solution
fval.exp <- maxexprts$optimum

# (2) min conditional variance
ones <- matrix(rep(1, N), nrow=1, ncol=N)
eps <- 0.001                                            # to keep sum(weihts)=1
ui <- rbind(ones, -ones, -diag(N), diag(N))
ci <- c(1-eps, -1-eps, -rep(1,N), rep(0,N))
minconvar <- constrOptim(rep(1/N,N), f_Var, grad=NULL, ui=ui, ci=ci, H=H)
w.var <- minconvar$par
fval.var <- minconvar$value

# (3) min conditional variance
maxconskew <- constrOptim(rep(1/N,N), f_Skew, grad=NULL, ui=ui, ci=ci, control=list(fnscale=-1), S=S)
w.skew <- maxconskew$par
fval.skew <- maxconskew$value

# (4) min conditional variance
minconkurt <- constrOptim(rep(1/N,N), f_Kurt, grad=NULL, ui=ui, ci=ci, K=K)
w.kurt <- minconkurt$par
fval.kurt <- minconkurt$value


# 5. solve PGP optimization
# (1) set optimal values at individual objective prog
R.star <- as.numeric(fval.exp)
H.star <- as.numeric(fval.var)
S.star <- as.numeric(fval.skew)
K.star <- as.numeric(fval.kurt)

# (2) solve multiple objetvice prog
lambda <- c(1,1,3,3) # c(1,3,1,1) # c(3,1,1,1) # c(1,1,0,0) # c(1,1,1,1)  # 
PGP <- constrOptim(rep(1/N,N), MinZ, grad=NULL, ui=ui, ci=ci,R=as.numeric(colMeans(rts)),
                   H=H,S=S,K=K,R.star=R.star,H.star=H.star,S.star=S.star,K.star=K.star,lambda=lambda)
w.PGP <- PGP$par
names(w.PGP) <- colnames(rts)
print(round(w.PGP, digits=4))

# (3) compute portfolio risk
port.PGP <- HMPort(w=w.PGP, R=rts)
print(round(port.PGP, digits=4))

# 6. compute equal weights portfolio
port.equal <- HMPort(w=rep(1/N, N), R=rts)
print(round(port.equal, digits=4))


