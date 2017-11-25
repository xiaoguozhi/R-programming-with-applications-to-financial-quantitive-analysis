########################################################
# Description:
# 1.for Book 'R with applications to financial quantitive analysis'
# 2.Chapter: CH-02-02-01
# 3.Section: 2.2
# 4.Purpose: compare three binary choice models
# 5.Author: Qifa Xu
# 6.Founded: Aug 03, 2014.
# 7.Revised: Aug 03, 2014.
########################################################
# Contents:
# 1. define a function
# 2. read data
# 3. compute descriptive statistics and check the normality of the data
# 4. debug the function
#########################################################
# 0. initializing
setwd('F:/programe/book/R with application to financial quantitive analysis/CH-02')
rm(list = ls())                   

# 1. generate x
x <- seq(-5, 5, length=100)

# 2. compute CDFs
# (1) normal distribution
CDF.norm <- pnorm(q=x, mean=0, sd=1)

# (2) logitstic distribution
CDF.logit <- plogis(q=x, location=0, scale=1)

# (3) extreme distribution
pextreme <- function(x) 1-exp(-exp(x))
CDF.extr <- pextreme(x)

# 3. compare the results
par(mfrow=c(1,1))
plot(c(x,x,x), c(CDF.norm, CDF.logit, CDF.extr), type='n', xlab='x', ylab='CDF')
lines(x, CDF.norm, lty=1, lwd=2)
lines(x, CDF.logit, lty=2, lwd=2)
lines(x, CDF.extr, lty=3, lwd=2)
abline(h=0.5, lty=20)
abline(v=0, lty=20)
legend('topleft', legend=c('标准正态分布', '逻辑分布', '极值分布'), lty=c(1,2,3), lwd=c(2,2,2))






