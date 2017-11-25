########################################################
# Description:
# 1.for Book 'R with applications to financial quantitive analysis'
# 2.Chapter: CH-04
# 3.Section: 5.4
# 4.Purpose: explore statistics
# 5.Author: Qifa Xu, Yingying Zhou
# 6.Date: Jan 11, 2014.
# 7.Revised: Aug 20, 2014.
########################################################
# Contents:
# 1. calculate descriptive statistics
# 2. plot bivariate normal distribution
#########################################################
# 0. initialize
# (1) set work path
setwd('F:/programe/book/R with application to financial quantitive analysis/CH-05')
rm(list=ls())
# (2) source packages
library(fBasics)                   # for basic statistics
library(mnormt)                    # for multivariate normal distribution
library(ks)                        # for kernel smooth

# 1. calculate descriptive statistics
# (1) read data
da <- read.delim('5-20.txt',head=T)
head(da)

# (2) summary stats
basicStats(da[,2])

# 2. plot bivariate normal distribution
# (1) read data
da <- read.table('5-21.txt',head=T)
dim(da)
rt <- da[,2:3]

# (2) simulate data under bivariate normal distribution
(m1 <- apply(rt,2,mean))
(v1 <- cov(rt))
set.seed(12345)
x <- rmnorm(1000, mean=m1,varcov=v1)
dim(x)

# (3) compare scatter plots for real data and simulate data
par(mfrow=c(1,2))
plot(rt[,2], rt[,1], xlab='上证', ylab='深证', cex=0.8, main='真实数据')
abline(a=0, b=1, lty=2)
plot(x[,2], x[,1], xlab='上证', ylab='深证', cex=0.9, main='模拟数据')
abline(a=0, b=1, lty=2)
par(mfrow=c(1,1))

# (4) estimate kernel density of real data
dens.hat <- kde(rt)
plot(dens.hat, display='filled.contour2', cont=seq(10,90,by=10), xlim=c(-0.06, 0.06), ylim=c(-0.05, 0.06))
plot(dens.hat, display='persp',zlab='密度函数', thin=3, border=1, col="white")

