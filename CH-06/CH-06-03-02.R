########################################################
# Description:
# 1.for Book 'R with applications to financial quantitive analysis'
# 2.Chapter: CH-06-03-02
# 3.Section: 6.3
# 4.Purpose: manipulate high frequency data
# 5.Author: Qifa Xu
# 6.Founded: Dec 09, 2013.
# 7.Revised: Aug 06, 2014.
########################################################
# Contents:
# 1. clean HF data
# 2. aggregate HF data
# 3. comput descriptive statistics for realized returns
# 4. comput (weighted) realized volatility
# 5. modeling realized volatility
#########################################################

# 0. initializing
setwd('F:/programe/book/R with application to financial quantitive analysis/CH-06')
rm(list=ls())

# 1. clean HF data
library(highfrequency)
data(sample_tdataraw)
dim(sample_tdataraw)
head(sample_tdataraw)

args(tradesCleanup)
tdata_afterfirstcleaning <- tradesCleanup(tdataraw=sample_tdataraw, exchanges="N")
names(tdata_afterfirstcleaning)
print(tdata_afterfirstcleaning$report)
barplot(tdata_afterfirstcleaning$report)
dim(tdata_afterfirstcleaning$tdata)

# 2. aggregate HF data
# (1) load sample price data
data("sample_tdata")
ts <- sample_tdata$PRICE
head(ts)

# (2) aggregate previous tick to the 30-second sampling frequency
args(aggregatets)
tsagg30sec = aggregatets(ts, on="seconds", k=30)
head(tsagg30sec, 20)

# (3) aggregate previous tick to the 5-minute sampling frequency
tsagg5min <- aggregatets(ts, on="minutes", k=5)
head(tsagg5min, 20)

# 3. comput descriptive statistics for realized returns
source('sub-06.R')
data(sample_real5minprices)
pdata <- sample_real5minprices[substr(time(sample_real5minprices), 12, 19)!='00:00:00']
length(pdata)
mins <- c(5, 10, 15, 20, 25, 30)
rtn <- list()
stats <- matrix(NA, nrow=length(mins), ncol=7)
for (i in seq_along(mins)){
  rtn[[i]] <- HFrtn(pdata=pdata, on="minutes", k=mins[i])
  stats[i,] <- stat(as.vector(rtn[[i]]))
}
head(rtn[[1]])
head(rtn[[2]])
rownames(stats) <- paste('min=', mins, sep='')
colnames(stats) <- c('mean', 'sd', 'skew', 'kurt', 'median', 'min', 'max')
print(stats)

par(mfrow=c(2,2))
par(mar=c(4,4,2,2))
plot(mins, stats[,'mean'], xlab='分钟', ylab='均值', type='o', pch='*')
plot(mins, stats[,'sd'], xlab='分钟', ylab='标准差', type='o', pch='*')
plot(mins, stats[,'skew'], xlab='分钟', ylab='偏度', type='o', pch='*')
plot(mins, stats[,'kurt'], xlab='分钟', ylab='峰度', type='o', pch='*')
par(mfrow=c(1,1))

# 4. comput (weighted) realized volatility
days <- levels(factor(substr(index(pdata), 1, 10)))
rv <- matrix(NA, nrow=length(days), ncol=length(mins))
wrv <- matrix(NA, nrow=length(days), ncol=length(mins))
for (i in seq_along(mins)){
  ind.day <- substr(index(rtn[[i]]), 1, 10)
  rv[,i] <- aggregate(rtn[[i]], ind.day, sumsq)
  
  ind.time <- substr(index(rtn[[i]]), 12, 19)
  lambda <- aggregate(rtn[[i]], ind.time, sumsq)/sum(rtn[[i]]^2)
  ind.time <- index(lambda)[lambda!=0]
  ind <- substr(index(rtn[[i]]), 12, 19) %in% ind.time
  rtn.f <- rtn[[i]][ind]      
  ind.day <- substr(index(rtn.f), 1, 10)
  N <- length(ind.time)
  lambda <- as.numeric(lambda)[lambda!=0]
  weight <- 1/(N*lambda)
  weight <- weight/sum(weight)*N
  wrv[,i] <- aggregate(rtn.f, ind.day, wsumsq, w=weight)
}


rownames(rv) <- days
colnames(rv) <- paste('min=', mins, sep='')
matplot(1:nrow(rv), sqrt(rv), xlab='时间', ylab='已实现波动', type='l')
legend('topright', legend=paste('min=', mins, sep=''), lty=1:6)

rownames(wrv) <- days
colnames(wrv) <- paste('min=', mins, sep='')
matplot(1:nrow(wrv), sqrt(wrv), xlab='时间', ylab='加权已实现波动', type='l')
legend('topright', legend=paste('min=', mins, sep=''), lty=1:6)


# 5. modeling realized volatility
# (1) HARRV model
data(realized_library)                                               # Get sample daily Realized Volatility data
DJI_RV <- realized_library$Dow.Jones.Industrials.Realized.Variance;  # Select DJI
DJI_RV <- DJI_RV[!is.na(DJI_RV)]                                     # Remove NA's
DJI_RV <- DJI_RV['2008'] 
args(harModel)
HARRV <- harModel(data=DJI_RV, periods=c(1,5,22), RVest=c("rCov"), type="HARRV",h=1)
class(HARRV)
summary(HARRV)
plot(HARRV)
matplot(predict(HARRV, interval='confidence'), type='l', xlab='index', ylab='realized volatility')

# (2) HEAVY model
returns <-  realized_library$Dow.Jones.Industrials.Returns             # realized return
rk      <-  realized_library$Dow.Jones.Industrials.Realized.Kernel     # realized measure
returns <- returns[!is.na(rk)]
rk <- rk[!is.na(rk)]                                                   # remove NA's 
data <- cbind(returns^2, rk )                                          # make data matrix with squared returns and realized measures
backcast <- matrix(c(var(returns),mean(rk)), ncol=1) 

args(heavyModel)
startvalues <- c(0.004,0.02,0.44,0.41,0.74,0.56)                       # initial values
HEAVY <- heavyModel(data=as.matrix(data,ncol=2), compconst=FALSE, startingvalues=startvalues, backcast=backcast)
names(HEAVY)
print(HEAVY$estparams)
plot(sqrt(HEAVY$condvar), xlab='时间', ylab='波动率', main='', lty=1)
lines(sqrt(rk), lty=2)
legend('topleft', legend=c('可观测已实现核', 'HEAVY条件波动'), lty=c(1,2))


