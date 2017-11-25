########################################################
# Description:
# 1.for Book 'R with applications to multivariable linear regression model'
# 2.Chapter: CH-02-01
# 3.Section: 2.1.4
# 4.Purpose: factor analysis
# 5.Author: Qifa Xu, Zhifeng Guo
# 6.Date: Dec 09, 2013.
# 7.Revised: Aug 30, 2014.
########################################################
# Contents:
# 1. read data from EXCEL file
# 2. do factor analysis
# 3. plot loadings
# 4. do comprehensive evaluation
#########################################################
# 0. initializing
# (1) set path
setwd('F:/programe/book/R with application to financial quantitive analysis/CH-02')
rm(list = ls())                   

# (2) load packages
library(xlsx)          # for xlsx data
library(nFactors)      # for the optimal number of factors

# 1. read data from EXCEL file
dat <- read.xlsx(file='MicEcoData.xlsx', sheetName='factor')
dat.fact <- dat[,-1]      # remove stock code
names(dat.fact) <- paste('x', 1:ncol(dat.fact), sep='')

# 2. do factor analysis
# (1) determine the optimal number of factors
ev <- eigen(cor(dat.fact)) # get eigenvalues
ap <- parallel(subject=nrow(dat.fact),var=ncol(dat.fact), rep=100, cent=.05)
nS <- nScree(x=ev$values, aparallel=ap$eigen$qevpea)
plotnScree(nS,main='碎石测验的非图形化解决方式',xlab='因子',ylab='特征值')

# (2) estimate
factor.result <- factanal(x=dat.fact, factor=2, scores="regression")
psy::scree.plot(dat.fact)                                   # scree plot
names(factor.result)
print(factor.result)

# 3. plot loadings
load <- factor.result$loadings[,1:2] 
plot(load, type="n",xlab='因子1',ylab='因子2')                # set up plot 
text(load, labels=names(dat.fact), cex=.7)                   # add variable names

# 4. do comprehensive evaluation
# (1) calculate weights
lambdas <- eigen(factor.result$correlation)$value            # variance of factors
(w <- lambdas[1:2]/sum(lambdas[1:2]))

# (2) evaluate
score <- factor.result$scores                                # get factor scores 
eva <- score %*% w                                           # evluate
eva.result <- data.frame(firm=dat[,1], eva=eva)  
eva.result[order(eva.result$eva, decreasing=TRUE),]          # sort results


