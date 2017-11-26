########################################################
# Description:
# 1.for Book 'R with applications to financial quantitive analysis'
# 2.Chapter: CH-06-03-01
# 3.Section: 6.3
# 4.Purpose: high frequency data analysis
# 5.Author: Qifa Xu
# 6.Founded: Dec 09, 2013.
# 7.Revised: Aug 06, 2014.
########################################################
# Contents:
# 1. read data
# 2. comput price change and durations
# 3. extract trading information
# 4. comput returns
# 6. implement ACD modeling
#########################################################

# 0. initializing
setwd('F:/programe/book/R with application to financial quantitive analysis/CH-06')
rm(list=ls())

# 1. read data
caterp <- read.table('taq-cat-t-jan04t082010.txt', header=T)
head(caterp)
(TT <- nrow(caterp))                                      # sample size

# 2. comput price change and durations
source('sub-06.R')
hf.series <- compRtnDura(da=caterp, plot.it=TRUE)
print(hf.series$len.total)
print(hf.series$len.norm)
print(hf.series$len.duration)
print(hf.series$ratio)

layout(matrix(c(1,2,3,3), 2, 2, byrow=FALSE), c(3,2), c(2,2), respect=TRUE)
layout.show(3)
par(mar=c(4, 4, 2, 2))
plot(hf.series$prs, xlab='时间', ylab='价格', type='l')
plot(hf.series$prc, xlab='时间', ylab='价格变化', type='l')
hist(hf.series$prc, xlab='价格变化', ylab='频数', main='价格变化直方图')

# 3. extract trading information
extrTrad(da=caterp, date='2010-01-04', from='09:30', to='09:45', plot.ACF=TRUE)

# 4. comput returns
# (1) comput return series for different frequency
ints <- c(1:5, 10, 15, 20, 25, 30)                        # include 10 intervals
rtns <- list()
ntrads <- list()
for (i in seq_along(ints)){
  cat('the current interval is', ints[i], '\n')
  tmp <- hfanal(da=caterp, int=ints[i], basic=1)
  rtns[[i]] <- tmp$returns
  ntrads[[i]] <- tmp$ntrad
}

# (2) comput descriptive statistics for 1-min return series
par(mfrow=c(3,1))
par(mar=c(5.5, 5.5, 1.5, 2.5))
plot(rtns[[1]], xlab='时间', ylab='对数收益', type='l')
hist(rtns[[1]], xlab='对数收益', ylab='频数', main='')
acf(rtns[[1]], xlab='滞后期', lag.max=300, main='')
par(mfrow=c(1,1))

# # 5. comput realized volatility
# rv <- NULL
# for (i in seq_along(ints)){
#   cat('the current interval is', ints[i], '\n')
#   tmp <- hfanal(da=caterp, int=ints[i], basic=1)
#   rv <- cbind(rv, tmp$realized)
# }
# colnames(rv) <- paste('int=', ints, sep='')
# matplot(1:nrow(rv), rv, xlab='time', ylab='realized volatility', type='l')
# legend('topleft', legend=paste('int=', ints, sep=''), lty=1)

# 6. implement ACD modeling
# (1) prepare data
sec <- 3600*caterp$hour+60*caterp$minute+caterp$second # time in seconds
ist <- 3600*9+30*60;  end <- 3600*16
lunch <- 3600*12
idx <- c(1:length(sec))[sec < ist]         # before market opens
jdx <- c(1:length(sec))[sec > end]         # after market closes
sec.norm <- sec[-c(idx,jdx)]               # normal trading hours only.
dt <- diff(sec.norm)
kdx <- c(1:length(dt))[dt > 0]             # Positive durations only
ti <- sec.norm[2:length(sec.norm)]
dt <- dt[kdx]
ti <- ti[kdx]
st <- 3600*6.5
f1 <- (ti-lunch)/st                        # trading time

# (2) fit a simple time series model
m1 <- lm(log(dt)~f1+I(f1^2))
summary(m1)
names(m1)
fit <- m1$fitted.values
adjdt <- dt/exp(fit)
plot(adjdt[1:1200], type='l', xlab='时间', ylab='调整持续期')

# (3) ACD modeling
m2 <- acd(adjdt,order=c(1,1), cond.dist="exp")
m3 <- acd(adjdt,order=c(1,1), cond.dist="weibull")
m4 <- acd(adjdt,order=c(1,2), cond.dist="weibull")
m5 <- acd(adjdt,order=c(1,2), cond.dist="gamma")
names(m5)
ep2 <- m2$epsilon
ep3 <- m3$epsilon
ep4 <- m4$epsilon
ep5 <- m5$epsilon

par(mfrow=c(5,1))
par(mar=c(4,4,3,2))
acf(adjdt, xlab='滞后期', main='调整序列', lim=c(-0.05,0.25))
acf(ep2, xlab='滞后期', main=expression(epsilon[2]), ylim=c(-0.05,0.25))
acf(ep3, xlab='滞后期', main=expression(epsilon[3]), ylim=c(-0.05,0.25))
acf(ep4, xlab='滞后期', main=expression(epsilon[4]), ylim=c(-0.05,0.25))
acf(ep5, xlab='滞后期', main=expression(epsilon[5]), ylim=c(-0.05,0.25))
par(mfrow=c(1,1))

Box.test(ep5, lag=5, type='Ljung')
Box.test(ep5, lag=10, type='Ljung')
Box.test(ep5, lag=15, type='Ljung')
Box.test(ep5, lag=20, type='Ljung')
Box.test(ep5^2, lag=5, type='Ljung')
Box.test(ep5^2, lag=10, type='Ljung')
Box.test(ep5^2, lag=15, type='Ljung')
Box.test(ep5^2, lag=20, type='Ljung')

