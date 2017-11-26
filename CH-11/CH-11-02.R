########################################################
# Description:
# 1.for Book 'R with applications to financial quantitive analysis'
# 2.Chapter: CH-11-02
# 3.Section: 11.2
# 4.Purpose: herd behavior through quantile regression
# 5.Author: Liu Xi, polished by Qifa Xu
# 6.Date: Apr 03, 2014.
# 7.Revised: Aug 31, 2014.
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

# 1. load data from last example
load('HerdBeh.RData')

# 2. source HerdBehavior_QR.R for quantile regression
source("HerdBehavior_QR.R")
HerdBehavior_QR(Data, CS=CSSD, Result1, Result2)
HerdBehavior_QR(Data, CS=CSAD, Result1, Result2)


