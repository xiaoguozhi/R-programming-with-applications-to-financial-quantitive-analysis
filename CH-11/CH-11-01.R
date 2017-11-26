########################################################
# Description:
# 1.for Book 'R with applications to financial quantitive analysis'
# 2.Chapter: CH-11-01
# 3.Section: 11.1
# 4.Purpose: herd behavior through mean regression
# 5.Author: Xi Liu, polished by Qifa Xu
# 6.Date: Apr 03, 2014.
# 7.Revised: Aug 24, 2014.
########################################################
# Contents:
# 1. read data from EXCEL file
# 2. set check function
# 3. calculate and show CSSD and CSAD
# 4. source HerdBehavior_MR.R for mean regression
# 5. save results
#############################################################
# 0. Initializing
# (1) set path
setwd('F:/programe/book/R with application to financial quantitive analysis/CH-11')
rm(list=ls())

# (2) load packages
library('RODBC')                   # for reading EXCEL file
library(KernSmooth)                # for kernel smooth
library(quantreg)                  # for quantile regression
library(splines)                   # for spline functions
library(qrnn)                      # for quantile regression neural network
library(fGarch)                    # for GARCH model
library(caret)                     # for classification and regression training
library(fBasics)                   # for markets and basic statistics
source('Sub-11.R')                 # our own functions

# 1. read data from EXCEL file
Y <- odbcConnectExcel('data.xls')
Data_FF <-  sqlFetch(Y, 'Sheet1')        # Read some or all of a table from an ODBC database into a data frame
close(Y)
rm(Y)

Data <- na.omit(Data_FF)
sum(is.na(Data))
dim(Data)

# 2. set check function
# (1) D1
w1 <- quantile(Data$RM,  probs = 0.5)
D1 <- NULL
rhoD1 <- function(Data1, W){
  n <- length(Data1)
  for ( i in 1:n){
    if (Data1[i]< W)
      D1[i] <- 1
    else
      D1[i] <-0
  } 
  return(D1)  
}
Result1 <- rhoD1(Data1=Data$RM, W=w1)

# (2) D2
w2 <- quantile(Data$RM,  probs = 0.95)
D2 <- NULL
rhoD2 <- function(Data1, W){
  n <- length(Data1)
  for ( i in 1:n){
    if (Data1[i]> W)
      D2[i] <- 1
    else
      D2[i] <-0
  } 
  return(D2)  
}
Result2 <- rhoD2(Data1=Data$RM, W=w2)

# 3. calculate and show CSSD and CSAD
# (1) do calculate
CSSD <- sqrt(apply((Data - Data$RM)^2, MARGIN=1, FUN=sum)/nrow(Data)) # compute the CSSD
CSAD <- apply(abs(Data - Data$RM), MARGIN=1, FUN=sum)/nrow(Data)      # compute the CSAD

# (2) plot time series
plotCS(CS=CSSD)
plotCS(CS=CSAD)

# (3) calculate descriptive statistics
data.statistic(CSSD)
data.statistic(CSAD)

# 4. source HerdBehavior_MR.R for mean regression
source("HerdBehavior_MR.R")
HerdBehavior_MR(Data, CS=CSSD, Result1, Result2)
HerdBehavior_MR(Data, CS=CSAD, Result1, Result2)

# 5. save results
save(Data, CSSD, CSAD, Result1, Result2, file='HerdBeh.RData')


