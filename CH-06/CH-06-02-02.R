########################################################
# Description:
# 1.for Book 'R with applications to financial quantitive analysis'
# 2.Chapter: CH-06-02-02
# 3.Section: 6.2
# 4.Purpose: SV modeling through stochvol package
# 5.Author: Qifa Xu
# 6.Founded: Dec 09, 2013.
# 7.Revised: Aug 06, 2014.
########################################################
# Contents:
# 1. load package
# 2. numerical simualtion for univariate SV process
#########################################################

# 0. initializing
setwd('F:/programe/book/R with application to financial quantitive analysis/CH-06')
rm(list=ls())

# 1. load package
library(stochvol)

# 2. numerical simualtion for univariate SV process
# (1) simulate a highly persistent SV process
set.seed(12345)
sim <- svsim(500, mu = -5, phi = 0.95, sigma = 0.25)

# (2) obtain 5000 draws from the sampler
draws <- svsample(sim$y, draws=5000, burnin=500, priormu=c(-8, 1),priorphi=c(10, 1.2), priorsigma=0.2)

# (3) show estimats
print(sim)
summary(draws)

# (4) predict 20 days ahead
fore <- predict(draws, 20)
plot(draws, forecast = fore)
# plot(draws, pages=1, all.terms=TRUE, forecast = fore)
volplot(draws, forecast = 10)

# (5) re-plot with different quantiles
newquants <- c(0.01, 0.05, 0.25, 0.5, 0.75, 0.95, 0.99)
draws <- updatesummary(draws, quantiles = newquants)
plot(draws, forecast = 20, showobs = FALSE, col = seq(along = newquants),
     forecastlty = 3, showprior = FALSE)
volplot(draws, forecast = 10)
