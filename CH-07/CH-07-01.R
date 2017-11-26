########################################################
# Description:
# 1.for Book 'R with applications to financial quantitive analysis'
# 2.Chapter: CH-07-01
# 3.Section: 7.1
# 4.Purpose: Calculation of VaR and ES
# 5.Author: Shijun Chen, polished by Qifa Xu
# 6.Date: February 15, 2014
# 7.Revised: April 2, 2014 
########################################################
# Contents:
# 1. Relationship between loss and return
# 2. Download financial time series data
# 3. Calculate VaR and ES via Riskmetrics model
# 4. Calculate VaR and ES via GARCH model
#########################################################

# 0.Initializing
setwd('F:/programe/book/R with application to financial quantitive analysis/CH-07')
rm(list=ls())

# 1. Relationship between loss and return
# (1) random data
set.seed(1)
L <- rchisq(1000, df=5)     # random generation for the chi-squared (chi^2) distribution
R <- -L

# (2) quantile
Q.L <- quantile(L, probs = c(0.05, 0.1, 0.5, 0.9, 0.95))  #produces sample quantiles corresponding to the given probabilities
Q.R <- quantile(R, probs = c(0.05, 0.1, 0.5, 0.9, 0.95))

# (3) figure
plot(c(density(L)$x, density(R)$x), c(density(L)$y, density(R)$y), xlab='收益或损失', ylab='密度', type='n')
lines(density(L), lty=1)
lines(density(R), lty=2)
legend('topleft', legend=c('损失', '收益'), lty=c(1,2))

# 2.Download financial time series data
library(quantmod)                                                 # loads package
getSymbols('AAPL', from='1989-12-01', to='2013-11-30')            # downloads daily price data of APPLE,please wait
dim(AAPL)                                                         # dimensions of daily price data
chartSeries(AAPL, theme='white')                                  # creates time series plot of price and deal
detach(package:quantmod)

price.AAPL <- AAPL$AAPL.Adjusted
R <- 100*diff(log(price.AAPL))                                    # gets daily log return series of APPLE
r <- (-1)*R[-1]
plot(r)

# 3.Calculate VaR and ES via Riskmetrics model
library(rugarch)                                               # load package
spec1 = ugarchspec(variance.model=list(model="iGARCH", garchOrder=c(1,1)), 
                   mean.model=list(armaOrder=c(0,0), include.mean=FALSE), 
                   distribution.model="norm", fixed.pars=list(omega=0,alpha1=0.06))        
# method for creating a univariate GARCH specification object prior to fitting
forecast1 <- ugarchforecast(spec1, data=r,n.roll=0,n.ahead=1,out.sample=1) 
# method for forecasting from a variety of univariate GARCH models
forecast1
VaR1 <- qnorm(0.95)*1.278
ES1 <- dnorm(qnorm(0.95))*1.278/0.05
VaR5 <- qnorm(0.95)*1.278*sqrt(5)
ES5 <- dnorm(qnorm(0.95))*1.278*sqrt(5)/0.05


# 4.Calculate VaR and ES via GARCH model
library(fGarch)
m1 <- garchFit(formula = ~arma(0,0)+garch(1,1), data = r, cond.dist = c("norm"), trace = FALSE)
# estimates the parameters of an univariate ARMA-GARCH/APARCH process
summary(m1)
p1 <- predict(m1,n.ahead=1)   #  1-step-ahead forecasts of the conditional mean and conditional variance
VaR <- p1$meanForecast+p1$standardDeviation*qnorm(0.95)
ES <- p1$meanForecast+p1$standardDeviation*dnorm(qnorm(0.95))/(1-0.95)

m2 <- garchFit(formula = ~arma(0,0)+garch(1,1), data = r, cond.dist = c("norm"), trace = FALSE)
p2 <- predict(m2,n.ahead=5)
VaR <- p2$meanForecast[5]+p2$standardDeviation[5]*qnorm(0.95)
ES <- p2$meanForecast[5]+p2$standardDeviation[5]*dnorm(qnorm(0.95))/(1-0.95)

m3 <- garchFit(formula = ~arma(0,0)+garch(1,1), data = r, cond.dist = c("std"), trace = FALSE)
summary(m3)
p3 <- predict(m3,n.ahead=1)
VaR <- p3$meanForecast+p3$standardDeviation*qt(0.95,5)/sqrt(5/3)
ES <- p3$meanForecast+p3$standardDeviation*dt(qt(0.95,5)/sqrt(5/3),5)/(1-0.95)

