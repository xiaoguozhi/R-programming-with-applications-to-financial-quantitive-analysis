########################################################
# Description:
# 1.for Book 'R with applications to financial quantitive analysis'
# 2.Chapter: CH-05-02
# 3.Section: 5.2
# 4.Purpose: claculate all kinds of returns
# 5.Author: Qifa Xu, Yingying Zhou
# 6.Date: Jan 11, 2014.
# 7.Revised: Aug 20, 2014.
########################################################
# Contents:
# 1. calculate individual stock returns
# 2. calculate multi-stock returns
# 3. calculate portfolio returns
#########################################################
# 0. initialize
# (1) set work path
setwd('F:/programe/book/R with application to financial quantitive analysis/CH-05')
rm(list=ls())

# (2) source packages
library(zoo)                    # for time series
library(timeSeries)             # for portfolio

# 1. calculate individual stock returns
# (1) read data from txt file
da <- read.table("5-1.txt",head=T,colClasses="character")
clsprc <- as.numeric(da[,3])
dates <- as.Date(da[,2])
cls <- zoo(clsprc, dates)                   # change object's class
lclsprc <- lag(cls,k=-1)                    # set one lag
clsprc <- as.numeric(da[2:237,3])
lclsprc <- as.numeric(lclsprc)

# (2) calculate daily returns use three approaches
re <- 100 * (log(clsprc)-log(lclsprc))                        # use formula
re.1 <- 100 * diff(log(cls))                                  # use function
re.2 <- returns(cls, method='continuous', percentage=TRUE)    # use function


# (3) calculate arithmetic mean
(am <- mean(re))                                              # the same thing
(am.1 <- mean(re.1))                                          # the same thing
(am.2 <- mean(re.2, na.rm=TRUE))                              # the same thing

# (4) calculate geometric mean
(gm <- 100*(cumprod(re/100+1)[length(cumprod(re/100+1))]^(1/length(re))-1))

# 2. calculate multi-stock returns
# (1) read data
da1 <- read.table("CAQC.txt",head=T,colClasses="character")
da2 <- read.table("ZXTX.txt",head=T,colClasses="character")
wcls1 <- as.numeric(da1[,3])
wcls2 <- as.numeric(da2[,3])
dates1 <- as.yearmon(da1[,2])
dates2 <- as.yearmon(da2[,2])
cls1 <- zoo(wcls1,dates1)
cls2 <- zoo(wcls2,dates2)
dat.merge <- merge(cls1, cls2)                                # merge data by date

# (2) calculate returns
r.merge <- returns(dat.merge, method='continuous', percentage=TRUE)
print(r.merge)

# 3. calculate portfolio returns
# (1) read data
da <- read.table("5-3.txt",head=T)
sum(is.na(da))                                           # check NA code
p.app <- na.approx(da)                                   # approximate NA code
r.stocks <- diff(log(p.app))*100

# (2) simulate a portfolio weights
set.seed(12345)
w <- runif(ncol(da), 0, 1)
(w <- w/sum(w))

# (3) make a portfolio
rp <- r.stocks %*% w



