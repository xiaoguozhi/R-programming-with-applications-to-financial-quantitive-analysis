########################################################
# Description:
# 1.for Book 'R with applications to financial quantitive analysis'
# 2.Chapter: CH-02-03-01
# 3.Section: 2.3
# 4.Purpose: VAR, impulse response analysns, and forecast error variance decomposition
# 5.Author: Qifa Xu
# 6.Founded: Aug 03, 2014.
# 7.Revised: Aug 03, 2014.
########################################################
# Contents:
# 1. load packages
# 2. setup data set
# 3. select optimal lags
# 4. estimate VAR(1) model
# 5. comput IRF and FEVD
#########################################################
# 0. initializing
setwd('F:/programe/book/R with application to financial quantitive analysis/CH-02')
rm(list = ls())                   

# 1. load packages
library(vars)
library(quantmod)

# 2. setup data set
# (1) get data from yahoo website
getSymbols('^SSEC', from='2000-01-01', to='2013-12-30')           # downloads daily price data of APPLE,please wait
getSymbols('^HSI', from='2000-01-01', to='2013-12-30')            # downloads daily price data of APPLE,please wait
dim(SSEC)                                                         # dimensions of the data
names(SSEC)                                                       # names of the data
detach(package:quantmod)

# (2) comput return of stocks
price.SSEC <- SSEC$SSEC.Adjusted
price.HSI <- HSI$HSI.Adjusted
r.SSEC <- 100*diff(log(price.SSEC))                                # comput daily return
r.HSI <- 100*diff(log(price.HSI))                                  # comput daily return
r.data <- merge(r.SSEC, r.HSI, join='inner')                       # merge data
R <- r.data[!apply(is.na(r.data), 1, any), ]                       # remove NA code
colnames(R) <- c('SSEC', 'HSI')                                    # change the names of the data

# 3. select optimal lags
VARselect(R, lag.max=8, type='both')

# 4. estimate VAR(1) model
VAR.model <- VAR(R, p=1, type='both')
VAR.model
summary(VAR.model, equation='SSEC')
plot(VAR.model, names='SSEC')
plot(VAR.model, names='HSI')

# 5. comput IRF and FEVD
# (1) estimate VECM
vecm <- ca.jo(R, type='trace', ecdet='trend', K=3, spec='transitory')
LR <- matrix(NA, nrow=2, ncol=2)
SR <- matrix(NA, nrow=2, ncol=2)
LR[1,2] <- 0
SR[2,1] <- 0
svec <-SVEC(vecm, r=1, LR=LR, SR=SR, lrtest=FALSE, boot=TRUE, runs=100)
summary(svec)

# (2) comput IRF
svec.irf <- irf(svec, response='SSEC', n.ahead=60, boot=TRUE)
par(mfrow=c(1,2))
plot(svec.irf, all.terms=TRUE, page=1)
plot(svec.irf, select=2)
par(mfrow=c(1,1))

# (3) comput FEVD
fevd <- fevd(svec, n.ahead=20)
fevd.SSEC <- fevd$SSEC
rownames(fevd.SSEC) <- 1:20
fevd.HSI <- fevd$HSI
rownames(fevd.HSI) <- 1:20
par(mfrow=c(1,2))
barplot(t(fevd.SSEC), legend.text=c('SSEC','HSI'), xlab='时期', xlim=c(0,28))
barplot(t(fevd.HSI), legend.text=c('SSEC','HSI'), xlab='时期', xlim=c(0,28))
par(mfrow=c(1,1))

