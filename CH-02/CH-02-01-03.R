########################################################
# Description:
# 1.for Book 'R with applications to multivariable linear regression model'
# 2.Chapter: CH-02-01
# 3.Section: 2.1.3
# 4.Purpose: cluster analysis
# 5.Author: Qifa Xu, Zhifeng Guo
# 6.Date: Dec 09, 2013.
# 7.Revised: Aug 30, 2014.
########################################################
# Contents:
# 1. read data from EXCEL file
# 2. data process
# 3. do cluster
# 4. show results
#########################################################
# 0. initializing
# (1) set path
setwd('F:/programe/book/R with application to financial quantitive analysis/CH-02')
rm(list = ls())                   

# (2) load packages
library(xlsx)          # for xlsx data

# 1. read data from EXCEL file
dat <- read.xlsx(file='MicEcoData.xlsx', sheetName='clust')
names(dat) <- c('label', paste('x', 1:7, sep=''))

# 2. data process
dist.matrix <- dist(scale(dat))      # normalize and calculate distance matrix

# 3. do cluster
h.result1=hclust(dist.matrix, method="complete")
h.result2=hclust(dist.matrix, method="average")
h.result3=hclust(dist.matrix, method="ward")
h.result4=hclust(dist.matrix, method="centroid");

# 4. show results
plot(h.result1,hang=-1,main='¾ÛÀàÍ¼',xlab='¾àÀë¾ØÕó',ylab='¸ß¶È'); result1=rect.hclust(h.result1,k=8,border="red")
plot(h.result1,hang=-1,main='¾ÛÀàÍ¼',xlab='¾àÀë¾ØÕó',ylab='¸ß¶È'); result1=rect.hclust(h.result1,k=8,border="green")
plot(h.result1,hang=-1,main='¾ÛÀàÍ¼',xlab='¾àÀë¾ØÕó',ylab='¸ß¶È'); result1=rect.hclust(h.result1,k=8,border="blue")
plot(h.result1,hang=-1,main='¾ÛÀàÍ¼',xlab='¾àÀë¾ØÕó',ylab='¸ß¶È'); result1=rect.hclust(h.result1,k=8,border="yellow")
