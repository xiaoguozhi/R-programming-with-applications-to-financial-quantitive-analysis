########################################################
# Description:
# 1.for Book 'R with applications to financial quantitive analysis'
# 2.Chapter: CH-12-02
# 3.Section: 12.2
# 4.Purpose: Fama-French three factors model
# 5.Author: Qifa Xu, Yingying Zhou
# 6.Date: Apr 03, 2014.
# 7.Revised: Sep 01, 2014.
########################################################
# Contents:
# 1. read data from EXCEL file
# 2. setup dataset
# 3. do mean regression
# 4. do quantile regresison
# 5. do graph
# 6. do rolling windows regression
# 7. do rolling window graph
#############################################################
# 0. initializing
# (1) set path
setwd('F:/programe/book/R with application to financial quantitive analysis/CH-12')
rm(list=ls())

# (2) load packages
library(RODBC)                  # for reading EXCEL file
library(xts)                    # for time series
library(quantreg)               # for quantile regression
source('Sub-12.R')              # our own functions

# 1. read data from EXCEL file
connExcel <- odbcConnectExcel('FamaFrenchData.xls')
Data_FF <- na.omit(sqlFetch(connExcel, '三因子模型月数据'))
Data_fund <- na.omit(sqlFetch(connExcel, '基金净值'))
Data_RF <- na.omit(sqlFetch(connExcel, '无风险利率'))
close(connExcel)
rm(connExcel)

# 2. setup dataset
# (1) select funds according to Fundcd
fund110002 <- subset(Data_fund, Fundcd==110002)

# (2) merge data
fund <- as.xts(fund110002[,'Accnav1'], order.by=as.POSIXct(fund110002[,'Date']))
FF <- as.xts(Data_FF[,3:ncol(Data_FF)], order.by=as.POSIXct(Data_FF[,'Date']))
RF <- as.xts(Data_RF[,ncol(Data_RF)], order.by=as.POSIXct(Data_RF[,'Date']))

dat <- merge(fund, FF, RF, all=FALSE)
dat$R <- diff(log(dat$fund))
dat <- na.omit(dat)[,-1]

VarNames <- c('RMRF_tmv', 'RMRF_mc', 'SMB_tmv', 'SMB_mc', 'HML_tmv', 'HML_mc', 'Rf', 'R')
colnames(dat) <- VarNames

# (3) calculate excess return
dat$ExcessR <- dat$R - dat$Rf*0.01
head(dat)

# 3. do mean regression
LR <- lm(ExcessR~RMRF_tmv + SMB_tmv + HML_tmv, data=dat)
summary(LR)

# 4. do quantile regresison
taus <- c(0.1,0.3,0.5,0.7,0.9)
(QR <- rq(ExcessR~RMRF_tmv + SMB_tmv + HML_tmv, tau=taus, data=dat))
summary(QR) 

# 5. do graph
# (1) coef of SMB 
if (max(QR$coefficients[3,]) < 0){
  Max_SMB <- 0
  Min_SMB <- min(QR$coefficients[3,])
} else if (min(QR$coefficients[3,]) > 0){
  Min_SMB <- 0
  Max_SMB <- max(QR$coefficients[3,])
} else{
  Min_SMB <- min(QR$coefficients[3,])
  Max_SMB <- max(QR$coefficients[3,])
}
Max_SMB <- 0.6
Min_SMB <- 0

plot(c(taus, taus), c(QR$coefficients[3,],rep(LR$coefficients[3],length(taus))), type='n',
     ylim=c(Min_SMB, Max_SMB), xlab=expression(tau), ylab='系数')
title('规模因子')
points(taus, QR$coefficients[3,],pch=20)
lines(taus, QR$coefficients[3,])
lines(taus, rep(LR$coefficients[3],length(taus)), lty=1, lwd=2)
abline(h=0, lty=2)
legend('topright', legend=c('分位数回归','均值回归'), pch=c(20,-1), lty=c(1,1), lwd=c(1,2))

# (2) coef of HML 
if (max(QR$coefficients[4,]) < 0){
  Max_HML <- 0
  Min_HML <- min(QR$coefficients[4,])
} else if (min(QR$coefficients[4,]) > 0){
  Min_HML <- 0
  Max_HML <- max(QR$coefficients[4,])
} else{
  Min_HML <- min(QR$coefficients[4,])
  Max_HML <- max(QR$coefficients[4,])
}
Max_HML <- 0.6
Min_HML <- 0

plot(c(taus, taus), c(QR$coefficients[4,],rep(LR$coefficients[4],length(taus))), type='n',
     ylim=c(Min_HML, Max_HML), xlab=expression(tau), ylab='系数')
title('账面市值比因子')
points(taus, QR$coefficients[4,],pch=20)
lines(taus, QR$coefficients[4,])
lines(taus, rep(LR$coefficients[4],length(taus)), lty=1, lwd=2)
abline(h=0, lty=2)
legend('topright', legend=c('分位数回归','均值回归'), pch=c(20,-1), lty=c(1,1), lwd=c(1,2))


# 6. do rolling windows regression
# (1) do regression
Window <- 50
coef_mean <- matrix(NA, 4,1)
coef_quant <- matrix(NA, 4,1)
for (t in 1:(nrow(dat) - Window + 1)){
  out <- Regress(dat[t:(t+Window-1),])
  coef_mean <- cbind(coef_mean, out[[1]])
  coef_quant <- cbind(coef_quant, out[[2]])
}

# (2) extract result
coef_mean <- coef_mean[,-1]
coef_quant <- coef_quant[,-1]

# 7. do rolling window graph
# (1) coef of SMB
time(dat)[Window:nrow(dat)]
Data_Graph <- ts(cbind(coef_mean['SMB_tmv',],coef_quant['SMB_tmv',c(seq(3, 5*t, by=5))],
                       coef_quant['SMB_tmv',c(seq(2, 5*t, by=5))], coef_quant['SMB_tmv',c(seq(4, 5*t, by=5))]),
                 frequency = 12, start = c(2008, 2)) 

plot(Data_Graph[,1],  type='n', ylim=c(-0.1,0.25), xaxs='i', xlab='时间', ylab='SMB系数')
lines(Data_Graph[,1], type='l', lty=1, lwd=2)
lines(Data_Graph[,2], type='l', lty=1, lwd=1)
lines(Data_Graph[,3], type='l', lty=2, lwd=1)
lines(Data_Graph[,4], type='l', lty=3, lwd=1)
abline(h=0,lty=4)
legend('topright', legend=c('均值回归','分位回归(0.5)','分位回归(0.3)','分位回归(0.7)'), 
       lty=c(1,1,2,3), lwd=c(2,1,1,1))
title('SMB系数')

# (2) coef of HML
Data_Graph <- ts(cbind(coef_mean['HML_tmv',],coef_quant['HML_tmv',c(seq(3, 5*t, by=5))],
                       coef_quant['HML_tmv',c(seq(2, 5*t, by=5))], coef_quant['HML_tmv',c(seq(4, 5*t, by=5))]),
                 frequency = 12, start = c(2008, 2)) 

plot(Data_Graph[,1],  type='n', ylim=c(0,0.7), xaxs='i', xlab='时间', ylab='HML系数')
lines(Data_Graph[,1], type='l', lty=1, lwd=2)
lines(Data_Graph[,2], type='l', lty=1, lwd=1)
lines(Data_Graph[,3], type='l', lty=2, lwd=1)
lines(Data_Graph[,4], type='l', lty=3, lwd=1)
abline(h=0, lty=4)
legend('topright', legend=c('均值回归','分位回归(0.5)','分位回归(0.3)','分位回归(0.7)'), 
       lty=c(1,1,2,3), lwd=c(2,1,1,1))
title('HML系数')



