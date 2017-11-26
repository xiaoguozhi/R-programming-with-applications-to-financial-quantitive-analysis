########################################################
# Description:
# 1.for Book 'R with applications to financial quantitive analysis'
# 2.Chapter: CH-07-02
# 3.Section: 7.2
# 4.Purpose: Calculation of VaR and ES
# 5.Author: Shijun Chen, polished by Qifa Xu
# 6.Date: February 20, 2014
# 7.Revised: April 2, 2014
########################################################
# Contents:
# 1. Estimates Parameters of linear quantile regression
# 2. Parametric nonlinear quantile regression
# 3. Nonparametric nonlinear quantile regression
# 4. Calculate VaR and ES via quantile regression 
########################################################

# 0.Initializing
setwd('F:/programe/book/R with application to financial quantitive analysis/CH-07')
rm(list=ls())

# 1.Estimates Parameters of linear quantile regression
# (1) quantile regression Via simplex method
library(quantmod)                                         # loads package  
getSymbols('^SSEC', from='2012-01-01', to='2013-12-31')   # downloads daily price data of SSEC,please wait,please wait
price <- as.numeric(SSEC$SSEC.Adjusted)               # gets price data of SSEC
volume <- as.numeric(SSEC$SSEC.Volume)                # gets volume data of SSEC
r.price <- diff(log(price))                           # gets daily log ruturns
r.volume <- (log(volume))[-1]                         # gets logarithmic of volume
SSEC <- data.frame(r.price,r.volume)                  # produces a dataframe

library(quantreg)                  # loads package
fit1 <- rq(formula = r.price ~ r.volume, tau = 0.5, data = SSEC)    # linear quantile regression fit
fit1                               # views coefficients

plot(r.volume, r.price, xlab= "交易量", ylab="价格", type = "n") 
points(r.volume, r.price, col= "blue")
abline(lm( r.price ~ r.volume), lty=1, lwd=2)
abline(rq( r.price ~ r.volume), lty=2, lwd=2)
taus <- c( 0.05, 0.1, 0.25, 0.75, 0.9, 0.95)
for(i in 1:length(taus)){
  abline(rq(r.price ~ r.volume, tau=taus[i]), lty=3)
}

# (2) Produces confidence intervals for the estimated parameters 
y <- r.price
x <- r.volume
fit2 <- rq(formula = y ~ x, tau = 0.5, method="br")   # linear quantile regression fit
fit2                        # views coefficients
summary(fit2, se = "rank")  # produces confidence intervals by inverting a rank test

# (3) quantile regression prediction
getSymbols('^SSEC', from='2014-01-07', to='2014-01-09')   # downloads daily price data of SSEC,please wait,please wait
volume <- as.numeric(SSEC$SSEC.Volume)
r.volume <- (log(volume))
fp <- predict.rq(fit1, newdata= data.frame(r.volume))
fp

# 2. Parametric nonlinear quantile regression
# (1) Box-Cox transformation
library(VGAM)                            # loads package  
getSymbols('^SSEC', from='2012-01-01', to='2013-12-31')   # downloads daily price data of SSEC,please wait,please wait
price <- as.numeric(SSEC$SSEC.Adjusted)               # gets price data of SSEC
volume <- as.numeric(SSEC$SSEC.Volume)                # gets volume data of SSEC
r.price <- diff(log(price))                           # gets daily log ruturns
r.volume <- (log(volume))[-1]                         # gets logarithmic of volume
SSEC <- data.frame(r.price,r.volume)                  # produces a dataframe

attach(SSEC) 
SSECdata <- subset(SSEC, r.volume>0)     # selects data
fit <- vgam(r.volume ~ s(r.price, df= c(4,2)), lms.bcn(zero=1), SSECdata)
head(predict(fit))
head(fitted(fit))
head(cdf(fit))  
par(bty = "l", mar = c(5, 4, 4,3) + 0.1, xpd = TRUE)
qtplot(fit, percentiles = c(5, 25, 75, 95), main='分位数', 
       las = 1, xlab='价格', ylab='交易量', lwd = 2, lcol = 4)

# (2) Locally polynomial quantile regression
getSymbols('^SSEC', from='2013-07-01', to='2013-12-31')   # downloads daily price data of SSEC,please wait,please wait
price <- as.numeric(SSEC$SSEC.Adjusted)               # gets price data of SSEC
volume <- as.numeric(SSEC$SSEC.Volume)                # gets volume data of SSEC
SSEC <- data.frame(price,volume)                      # produces a dataframe

par(mfrow=c(1,1))
plot(price, volume, xlab='价格', ylab='交易量')
taus <- c(0.05,0.50,0.95)
for(i in 1:length(taus)){
  tau = taus[i]
  fit <- lprq(price,volume,h=3,tau)
  lines(fit$xx,fit$fv,lty=i)
}                            # locally polynomial quantile regression fit

# 3. Nonparametric nonlinear quantile regression
library(np)
data("Italy")
attach(Italy)
bw <- npcdistbw(formula=gdp~ordered(year))    # selects bandwidth, please wait 
model.q0.25 <- npqreg(bws=bw, tau=0.25)       # calculates quantiles
model.q0.50 <- npqreg(bws=bw, tau=0.50)
model.q0.75 <- npqreg(bws=bw, tau=0.75)
plot(ordered(year), gdp, xlab="年份", ylab="GDP分位数")     # plots the resulting quantiles manually
lines(ordered(year), model.q0.25$quantile, col="red", lty=2)
lines(ordered(year), model.q0.50$quantile, col="blue", lty=3)
lines(ordered(year), model.q0.75$quantile, col="red", lty=2)
legend(ordered(1951), 32, c("tau = 0.25", "tau = 0.50", "tau = 0.75"), 
       lty=c(2, 3, 2), col=c("red", "blue", "red"))
detach(Italy)

# 4. Calculate VaR and ES via quantile regression
library(quantmod)
getSymbols('AAPL', from='2003-01-01', to='2013-12-31')   # downloads daily price data of APPLE,please wait
price <- AAPL$AAPL.Adjusted
len <- length(price)
lnprice <- as.matrix(log(price))                    # gets daily log return series of APPLE

r0 <- 100*diff(lnprice[4:len],lag=1)
r1 <- 100*diff(lnprice[3:len],lag=2)
r2 <- 100*diff(lnprice[2:len],lag=3)
r3 <- 100*diff(lnprice[1:len],lag=4)
re <- data.frame(r0,r1,r2,r3)
library(quantreg)
fit <- rq(r0 ~ ., tau = 0.05, data=re)
fit

ree <- re[2761:2765,]
fp <- predict(fit, newdata = ree)
fp
VaR <- -fp
VaR
