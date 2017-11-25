########################################################
# Description:
# 1.for Book 'R with applications to financial quantitive analysis'
# 2.Chapter: CH-04-03
# 3.Section: 4.3
# 4.Purpose: explore statistics
# 5.Author: Qifa Xu, Kai Deng
# 6.Date: Jan 11, 2014.
# 7.Revised: Aug 20, 2014.
########################################################
# Contents:
# 1. input data
# 2. read data from exist file
# 3. read data from internet
#########################################################
# 0. initialize
# (1) set work path
setwd('F:/programe/book/R with application to financial quantitive analysis/CH-04')
rm(list=ls())
# (2) source packages

# 1. input data
# (1) input a vector
(x<-c(11,12,13,14,15))
x<-scan()

# (2) input an array
x<-1:12
a<-array(data=x,dim=c(3,4))
print(a)

# (3) input a data.frame
time<-c('2014-2-24','2014-2-25','2014-2-26','2014-2-27','2014-2-28')
AAPL.Volume<-c(10318200,8284000,9864900,10781500,13284600)
AAPL.Adjusted<-c(527.55,522.06,517.35,527.67,526.24)
AAPL<-data.frame(time,AAPL.Volume,AAPL.Adjusted)
print(AAPL)

# (4) input a list
time<-c('2014-2-24','2014-2-25','2014-2-26','2014-2-27','2014-2-28')
AAPL.Volume<-c(10318200,8284000,9864900,10781500,13284600)
AAPL.Adjusted<-c(527.55,522.06,517.35,527.67,526.24)
mylist<-list(time=time,volume=AAPL.Volume,adjusted=AAPL.Adjusted)
print(mylist)

# 2. read data from exist file
# (1) use clipboard
data <- read.delim('clipboard')
print(data)

# (2) use txt or csv file
data <- read.table('BalanceSheet.txt')
data <- read.csv('BalanceSheet.csv')

# (3) use xlsx package
library(xlsx)
data <- read.xlsx('BalanceSheet.xlsx', 1)
data <- read.xlsx('BalanceSheet', 'sheet1')

# 3. read data from internet
library(quantmod)
getSymbols('AAPL', from='2014-01-01',to='2014-07-31')
head(AAPL)

PAYH <- getSymbols('600036.ss',from='2014-01-01',to='2014-07-31', auto.assign=FALSE)
tail(PAYH)
