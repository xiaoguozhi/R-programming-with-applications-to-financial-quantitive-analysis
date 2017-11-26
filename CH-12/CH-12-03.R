########################################################
# Description:
# 1.for Book 'R with applications to financial quantitive analysis'
# 2.Chapter: CH-12-03
# 3.Section: 12.3
# 4.Purpose: evaluate performance of funds
# 5.Author: Yuye Liu, polished by Qifa Xu
# 6.Date: Apr 03, 2014.
# 7.Revised: Sep 01, 2014.
########################################################
# Contents:
# 1. read data from EXCEL file
# 2. calculate risk measurment indicators
# 3. calculate performance measurment indicators
# 4. do mean regression
# 5. do quantile regression
#############################################################
# 0. initializing
# (1) set path
setwd('F:/programe/book/R with application to financial quantitive analysis/CH-12')
rm(list=ls())

# (2) load packages
library(xlsx)                   # for reading EXCEL file
library(zoo)                    # for time series
library(xts)                    # for time series
library(quantreg)               # for quantile regression
source('Sub-12.R')              # our own functions

# 1. read data from EXCEL file
# (1) read raw data 
StratInd <- read.xlsx(file='FundPerfData.xlsx', sheetName='Strategy')
r.ThreeFactor <- read.xlsx(file='FundPerfData.xlsx', sheetName='threefactors')
r.StyFactor_1 <- read.xlsx(file='FundPerfData.xlsx', sheetName='stylefactors')
r.StyFactor_2 <- read.xlsx(file='FundPerfData.xlsx', sheetName='bonds')
benchreturn <- read.xlsx(file='FundPerfData.xlsx', sheetName='benchmark')

# (2) interplote data
StratInd$RV <- na.approx(StratInd$RV)
StratInd$MF <- na.approx(StratInd$MF)
StratInd$ED <- na.approx(StratInd$ED)

# (3) data frequency and treat as time series
StratInd.xts <- data.freq(StratInd, freq='Month')
r.ThreeFactor.xts <- data.freq(r.ThreeFactor, freq='Month')
r.StyFactor_1.xts <- data.freq(r.StyFactor_1, freq='Month')
r.StyFactor_2.xts <- data.freq(r.StyFactor_2, freq='Month')
benchreturn.xts <- data.freq(benchreturn, freq='Month')

# (4) compute returns
r.StratInd.xts <- diff(log(StratInd.xts))                                                         
r.StratInd.xts <- r.StratInd.xts[-1,]

# (5) merge data
data_Str.Thr <- merge(r.StratInd.xts, r.ThreeFactor.xts, join='inner')
data_Str.Thr.Sty1 <- merge(data_Str.Thr, r.StyFactor_1.xts, join='inner')
data_Str.Thr.Sty1.Sty2 <- merge(data_Str.Thr.Sty1, r.StyFactor_2.xts, join='inner')

# (6) renames and attach data
sample_data <- data_Str.Thr.Sty1.Sty2
names.strat <- c('RV', 'MF', 'ES', 'FI', 'M', 'ED', 'FOF')
names.fact <- c('RF', 'SMB', 'HML', 'OPG', 'OPV', 'TPG', 'TPV', 'SPG', 'SPV', 'TB', 'CB')
X <- as.matrix(sample_data[,names.fact])
Y <- as.matrix(sample_data[,names.strat])
colnames(Y) <- names.strat
attach(as.data.frame(sample_data))
BP <- benchreturn.xts$BP

# 2. calculate risk measurment indicators
sd(RV)                                                                    # standard deviation
(rou<-cor(cbind(RV=RV,BP)))                                                  # correlation coefficient
(beta<-rou*sd(RV)/sd(BP))                                                 # beta coefficient
(Down.Risk <- sqrt(sum(pmin(0, RV)^2)/length(RV)))                        # downside risk

# 3. calculate performance measurment indicators
(TR<-mean(RV)/beta[1,2])                                                  # TR ratio
(ShR<-mean(RV)/sd(RV))                                                    # ShR ratio
(SoR<-mean(RV)/Down.Risk)                                                 # SoR ratio

# 4. do mean regression
# (1) do stepwise regression
lm.sol<-lm(RV~RF+SMB+HML+OPG+OPV+TPG+TPV+SPG+SPV+TB+CB)                   #linear regression                               
summary(lm.sol)
lm.step<-step(lm.sol)                                                     #step regression
summary(lm.step)

#£¨2£©calculate Jesen indices
alpha_mr <- numeric(length(names.strat))
names.sig_mr <- list()
for (i in 1:length(names.strat)){
  temp.mr <- meanreg(Y=Y[,names.strat[i]], X=X)
  alpha_mr[i] <- temp.mr$alpha
  names.sig_mr[[i]] <- temp.mr$names.sig
}
names(alpha_mr) <- names.strat
print(alpha_mr)

# 5. do quantile regression
# (1) optimal lambda and optimal AIC
taus <- c(0.1, 0.25, 0.5, 0.75, 0.9)
lambdas <- seq(0, 0.1, by=0.001)
lamdsOpt <- matrix(NA, nrow=length(names.strat), ncol=length(taus))
rownames(lamdsOpt) <- names.strat
colnames(lamdsOpt) <- paste('tau=', taus)
AICsOpt <- matrix(NA, nrow=length(names.strat), ncol=length(taus))
rownames(AICsOpt) <- names.strat
colnames(AICsOpt) <- paste('tau=', taus)
for (i in 1:length(names.strat)){
  temp.sel <- sel.labmda(Y=Y[,i], X=X, taus, lambdas)
  lamdsOpt[i,] <- temp.sel$lambdasOpt
  AICsOpt[i,] <- temp.sel$AICOpt
}
lamdsOpt
AICsOpt

#(2) parameter selection
coefs_1 <- selParms(Y=Y[,1], X, taus, lamdsOpt, names.fact)
coefs_2 <- selParms(Y=Y[,2], X, taus, lamdsOpt, names.fact)
coefs_3 <- selParms(Y=Y[,3], X, taus, lamdsOpt, names.fact)
coefs_4 <- selParms(Y=Y[,4], X, taus, lamdsOpt, names.fact)
coefs_5 <- selParms(Y=Y[,5], X, taus, lamdsOpt, names.fact)
coefs_6 <- selParms(Y=Y[,6], X, taus, lamdsOpt, names.fact)
coefs_7 <- selParms(Y=Y[,7], X, taus, lamdsOpt, names.fact)

print(coefs_1)


#£¨3£©alpha star indices
riskvalue <- seq(0, 1, by=0.2)
fund.names <- colnames(Y)
alpha_star <- matrix(NA, nrow=length(fund.names), ncol=length(riskvalue))
alpha_star[1,] <- coefs_1[1, 3] - riskvalue/2*(coefs_1[1, 4]-coefs_1[1, 2])
alpha_star[2,] <- coefs_2[1, 3] - riskvalue/2*(coefs_2[1, 4]-coefs_2[1, 2]) 
alpha_star[3,] <- coefs_3[1, 3] - riskvalue/2*(coefs_3[1, 4]-coefs_3[1, 2]) 
alpha_star[4,] <- coefs_4[1, 3] - riskvalue/2*(coefs_4[1, 4]-coefs_4[1, 2]) 
alpha_star[5,] <- coefs_5[1, 3] - riskvalue/2*(coefs_5[1, 4]-coefs_5[1, 2]) 
alpha_star[6,] <- coefs_6[1, 3] - riskvalue/2*(coefs_6[1, 4]-coefs_6[1, 2]) 
alpha_star[7,] <- coefs_7[1, 3] - riskvalue/2*(coefs_7[1, 4]-coefs_7[1, 2]) 
rownames(alpha_star) <- fund.names
colnames(alpha_star) <- paste('riskvalue=', riskvalue, sep='')
print(alpha_star)
