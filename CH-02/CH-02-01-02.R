########################################################
# Description:
# 1.for Book 'R with applications to multivariable linear regression model'
# 2.Chapter: CH-02-01
# 3.Section: 2.1.2
# 4.Purpose: stepwise regression
# 5.Author: Qifa Xu, Zhifeng Guo
# 6.Date: Dec 09, 2013.
# 7.Revised: Aug 30, 2014.
########################################################
# Contents:
# 1. read data from EXCEL file
# 2. do linear regression
# 3. do regression step by step
#########################################################
# 0. initializing
# (1) set path
setwd('F:/programe/book/R with application to financial quantitive analysis/CH-02')
rm(list = ls())                   

# (2) load packages
library(xlsx)          # for xlsx data

# 1. read data from EXCEL file
dat <- read.xlsx(file='MicEcoData.xlsx', sheetName='stepReg')
names(dat) <- c('y', paste('x', 1:7, sep=''))

# 2. do linear regression
model.lm <- lm(y~.,data=dat);
summary(model.lm)

# 3. do regression step by step
model.step <- step(model.lm)
summary(model.step)


