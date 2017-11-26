########################################################
# Description:
# 1.for Book 'R with applications to financial quantitive analysis'
# 2.Chapter: CH-07
# 3.Section: 7.3
# 4.Purpose: Calculation of VaR and ES
# 5.Author: Shijun Chen, polished by Qifa Xu
# 6.Date: February 18, 2014
# 7.Revised: April 2, 2014
########################################################
# Contents:
# 1. Plot the pdf of GEV
# 2. Download financial time series data
# 3. MLE of GEV and compute VaR
# 4. Hill estimator of GEV
# 5. Mean excess plot
######################################################

# 0.Initializing
setwd('F:/programe/book/R with application to financial quantitive analysis/CH-07')
rm(list=ls())

# 1.Plot the pdf of GEV
x <- seq(-10, 10, by=0.1)       # generates regular sequences
library(evir)
Gumbel_density <- exp(-x-exp(-x))
Weibull_density <- dgev(x, xi = -0.3, mu = 0, sigma = 1)
Frechet_density <- dgev(x, xi = 0.8, mu = 0, sigma = 1)

plot(c(x,x,x), c(Gumbel_density,Weibull_density,Frechet_density),   # Plos the pdf of GEV
     type='n', xlab='x', ylab='ÃÜ¶È',las=1)
lines(x, Gumbel_density, type='l', lty=1, col='black')
lines(x, Weibull_density, type='l', lty=2, col='blue')
lines(x, Frechet_density, type='l', lty=3, col='red')
legend('topright', legend=c('Gumbel','Weibull','Frechet'), lty=c(1,2,3), col=c('black','blue','red'))

# 2.Download financial time series data
library(quantmod)                                       # loads package
getSymbols('AAPL', from='1989-12-01', to='2013-11-30')  # downloads daily price data of APPLE,please wait
dim(AAPL)                                         # dimensions of daily price data
chartSeries(AAPL, theme='white')                  # creates time series plot of price and deal
detach(package:quantmod)

price.AAPL <- AAPL$AAPL.Adjusted
R <- 100*diff(log(price.AAPL))                    # gets daily log return series of APPLE
r <- (-1)*R[-1]
plot(r)

# 3.MLE of GEV and compute VaR
library(evir)
m1 <- gev(r, block=21)         # fits generalized extreme value distribution
m1
names(m1)
xi <- as.numeric(m1$par.ests[1])
xi
sigma <- as.numeric(m1$par.ests[2])
sigma
mu <- as.numeric(m1$par.ests[3])
mu
VaR <- mu-(sigma/xi)*(1-(-21*log(1-0.05))^(-xi))
VaR
VaR5 <- 5^(xi)*VaR 
VaR5

# 4.Hill estimator of GEV
Hill <- function(x, H){
  # computes the Hill estimator of the shape parameter.
  # r: data
  # H: the number of order statistics used
  r <- as.numeric(r)
  s <- sort(r)
  TT <- length(r)
  ist <- TT - H
  y <- log(s[ist:TT])
  hill <- sum(y[2:length(y)])/H
  hill <- hill - y[1]
  sd <- sqrt(hill^2/H)
  cat('Hill estimate & std-err:', c(hill, sd), '\n')
  return(data.frame(hill=hill, sd=sd))
}
Hill(r,290)
Hill(r,300)
Hill(r,310)

# 5.Mean excess plot
library(evir)
meplot(r, las=1)                # plots sample mean excesses over increasing thresholds

m1 <- gpd(r, threshold=3.00)    # fits generalized pareto model
m1
par(mfcol=c(2,2))
plot(m1)                        # provides four different plots for assessing fitted GPD model
shape(r,end=1500)               # a plot showing the stability of the estimates

m2 <- pot(r, threshold=3.00)    # peaks over thresholds model
riskmeasures(m2,c(0.95,0.99))   # calculates point estimates of prescribed quantiles and expected shortfalls 


