########################################################
# Description:
# 1.for Book 'R with applications to financial quantitive analysis'
# 2.Chapter: CH-12-02
# 3.Section: 12.2
# 4.Purpose: Sharpe style analysis
# 5.Author: Qifa Xu, Yingying Zhou
# 6.Date: Apr 03, 2014.
# 7.Revised: Sep 01, 2014.
########################################################
# Contents:
# 1. read data from EXCEL file
# 2. merge data
# 3. calculate descriptive statistics
# 4. calculate the correlation coefficient
# 5. do mean regression with constraints
# 6. do quantile regression with constraints
#############################################################
# 0. initializing
# (1) set path
setwd('F:/programe/book/R with application to financial quantitive analysis/CH-12')
rm(list=ls())

# (2) load packages
library(xlsx)                   # for reading EXCEL file
library(RODBC)                  # for reading EXCEL file
library(xts)                    # for time series
library(quadprog)               # for quadratic program
library(quantreg)               # for quantile regression
source('Sub-12.R')              # our own functions

# 1. read data from EXCEL file
connExcel <- odbcConnectExcel(xls.file='SharpeData')
Data_GB <- na.omit(sqlFetch(connExcel, '中信国债指数'))
Data_LV <- na.omit(sqlFetch(connExcel, '中信大盘价值'))
Data_LG <- na.omit(sqlFetch(connExcel, '中信大盘成长'))
Data_MV <- na.omit(sqlFetch(connExcel, '中信中盘价值'))
Data_MG <- na.omit(sqlFetch(connExcel, '中信中盘成长'))
Data_SV <- na.omit(sqlFetch(connExcel, '中信小盘价值'))
Data_SG <- na.omit(sqlFetch(connExcel, '中信小盘成长'))
Data_R <- na.omit(sqlFetch(connExcel, '收益率'))
close(connExcel)
rm(connExcel)

# 2. merge data
GB <- as.xts(Data_GB[,4], order.by=Data_GB[,3]); colnames(GB) <- 'R'
LV <- as.xts(Data_LV[,4], order.by=Data_LV[,3]); colnames(LV) <- 'R'
LG <- as.xts(Data_LG[,4], order.by=Data_LG[,3]); colnames(LG) <- 'R'
MV <- as.xts(Data_MV[,4], order.by=Data_MV[,3]); colnames(MV) <- 'R'
MG <- as.xts(Data_MG[,4], order.by=Data_MG[,3]); colnames(MG) <- 'R'
SV <- as.xts(Data_SV[,4], order.by=Data_SV[,3]); colnames(SV) <- 'R'
SG <- as.xts(Data_SG[,4], order.by=Data_SG[,3]); colnames(SG) <- 'R'
YYJJ <- as.xts(Data_R[,4], order.by=as.POSIXct(Data_R[,2])); colnames(YYJJ) <- 'R'

names.R <- c('GB', 'LV', 'LG', 'MV', 'MG', 'SV', 'SG', 'YYJJ')
dat <- merge(GB,LV,LG,MV,MG,SV,SG,YYJJ, all=FALSE)
colnames(dat) <- names.R
head(dat)

# 3. calculate descriptive statistics
stats <- matrix(NA, nrow=8, ncol=length(names.R))
for (j in 1:ncol(dat)){
  stats[,j] <- as.numeric(statSummary(dat[,names.R[j]]))
}
rownames(stats) <- names(statSummary(dat[,names.R[1]]))
colnames(stats) <- names.R
(stats <- round(stats, digits=4))


# 4. calculate the correlation coefficient
round(cor(dat[,-8]), digits=4)

# 5. do mean regression with constraints
Dmat <- cov(dat[,-8])
dvec <- cov(dat[,8], dat[,-8])
a1 <- rep(1, 7)
a2 <- matrix(0, 7, 7)
diag(a2) <- 1
w <- rep(0, 7)
Amat <- t(rbind(a1, a2))
b0 <- c(1, w)
model.mr <- solve.QP(Dmat, dvec, Amat, bvec=b0, meq=1)
(wgts.mr <- round(model.mr$solution, digits=4))

# 6. do quantile regression with constraints
taus <- c(0.1,0.3,0.5,0.7,0.9)
R <- cbind(0,rbind(rep(-1,7),rep(1,7),diag(7)))
r <- c(-1,1,rep(0,7))
model.qr <- rq(YYJJ ~., data=dat, R=R, r=r, tau=taus, method="fnc")
summary(model.qr)
(wgts.qr <- round(coef(model.qr)[-1,], digits=4))


