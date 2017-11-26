########################################################
# Description:
# 1.for Book 'R with applications to financial quantitive analysis'
# 2.Chapter: CH-09-01
# 3.Section: 9.1
# 4.Purpose: advanced CAPM
# 5.Author: Qifa Xu
# 6.Date: Aug 20, 2014.
# 7.Revised: Aug 22, 2014.
########################################################
# Contents:
# 1. read component stock data from Yahoo or RData file
# 2. read HS300 index from RData file
# 3. compute logrithm returns
# 4. calculate higher co-moment betas, or systematic variance, skewness, and kurtosis
# 5. test four-momnet CAPM
#########################################################

# 0. initialize
# (1) set work path and workspace
setwd('F:/programe/book/R with application to financial quantitive analysis/CH-09')
rm(list=ls())

# (2) load packages
library(PerformanceAnalytics)                 # for performance and risk analysis
library(xlsx)                                 # need to install Java platform and R package 'rJava'
library(tseries)                              # for read data from Yahoo
source('Sub-09.R')

# 1. read component stock data from Yahoo or RData file
# (1) extract component information from HS300
info.comp <- read.xlsx(file='HS300.xlsx', sheetName='component', startRow=2)
info.comp$code.names <- NA
info.comp[info.comp$exchange=='Shenzhen', 'code.names'] <- paste(info.comp[info.comp$exchange=='Shenzhen', 'code'], '.sz', sep='')
info.comp[info.comp$exchange=='Shanghai', 'code.names'] <- paste(info.comp[info.comp$exchange=='Shanghai', 'code'], '.ss', sep='')

# (2) read data from Yahoo(need time to process)
date.fr <- '2009-01-01'
date.to <- '2013-12-31'
stock.list <- list()
for (i in 1:nrow(info.comp)){
  stock.list[[i]] <- get.hist.quote(instrument=info.comp$code.names[i],start=date.fr, end=date.to)
  names(stock.list)[i] <- info.comp$code.names[i]
}
length(stock.list)

# (3) read data from RData file (we supply some stocks which cannot be downloaded from Yahoo)
load('stockdata.RData')
length(stock.data)
names.stock <- names(stock.data)

# (4) set index of zoo for '601318.ss' at the 258-th list
index(stock.data[['601318.ss']]) <- as.Date(index(stock.data[['601318.ss']]))

# (5) process prices of component stock
p.stocks <- stock.data[[1]][,'Close']
for (i in 2:length(names.stock)){
  temp <- stock.data[[i]]
  p.stocks <- merge(p.stocks, temp$Close, all=TRUE, suffixes = c('',''))
}
names(p.stocks) <- names.stock                        # add names to the object
num.NA <- apply(is.na(p.stocks), 2, sum)              # summary nos of NA less than 50
names.sel <- names.stock[num.NA<50]                   # keep stocks whoes nos of NA less than 50
p.sel <- p.stocks[, names.sel]                        # keep stocks whoes nos of NA less than 50
p.app <- na.approx(p.sel)                             # approximate those NA code           

# 2. read HS300 index from RData file
load('H300.RData')
p.H300 <- H300[-c(1:2), c('Idxtrd01', 'Idxtrd05')]    # clean data
names(p.H300) <- c('Enddt', 'price')
p.H300.ts <- zoo(as.numeric(p.H300$price), order.by=as.Date(p.H300$Enddt))
rm(H300)

# 3. compute logrithm returns
r.H300 <- 100 * diff(log(p.H300.ts))
r.stocks <- 100 * diff(log(p.app))
rts <- merge(r.stocks, r.H300)                        # the last column is index
dim(rts)
TT <- nrow(rts)
N <- ncol(rts) - 1


# 4. calculate higher co-moment betas, or systematic variance, skewness, and kurtosis
# (1) estimate higher co-moment betas
betas <- BetaCoVariance(Ra=rts[,1:N], Rb=rts[, 'r.H300', drop=FALSE])
gammas <- BetaCoSkewness(Ra=rts[,1:N], Rb=rts[, 'r.H300', drop=FALSE])
thetas <- BetaCoKurtosis(Ra=rts[,1:N], Rb=rts[, 'r.H300', drop=FALSE])

risks <- cbind(as.numeric(betas), as.numeric(gammas), as.numeric(thetas))
colnames(risks) <- c('beta', 'gamma', 'theta')
rownames(risks) <- names.sel
head(risks)

# (2) show the results
for (j in 1:3){
  par(mar=c(3,6,3,3))
  barplot(risks[1:50,j], main=colnames(risks)[j], horiz=TRUE, las=1)
  abline(v=1, lty=2)
}

# 5. test four-momnet CAPM
# (1) set model matrix
Rf <-  0.003/360   #  0 #           # set rate of riskfree
r.bar <- colMeans(r.stocks) - Rf
data.FourCAPM <- data.frame(r.bar,risks)

# (2) test through OLS
model.TwoCAPM <- lm(r.bar~beta, data=data.FourCAPM)
summary(model.TwoCAPM)

model.ThrCAPM <- lm(r.bar~beta+gamma, data=data.FourCAPM)
summary(model.ThrCAPM)

model.FourCAPM <- lm(r.bar~., data=data.FourCAPM)
summary(model.FourCAPM)

