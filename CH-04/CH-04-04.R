########################################################
# Description:
# 1.for Book 'R with applications to financial quantitive analysis'
# 2.Chapter: CH-04-04
# 3.Section: 4.4
# 4.Purpose: explore statistics
# 5.Author: Qifa Xu, Kai Deng
# 6.Date: Jan 11, 2014.
# 7.Revised: Aug 20, 2014.
########################################################
# Contents:
# 1. process time series data
# 2. process cross sectional data
#########################################################
# 0. initialize
# (1) set work path
setwd('F:/programe/book/R with application to financial quantitive analysis/CH-04')
rm(list=ls())
# (2) source packages
library(timeSeries)                                # for time series
library(tseries)                                   # for time series
library(RODBC)                                     # for data connect
library(quantmod)

# 1. process time series data
# (1) read and merge data
stock.names <- c('AAPL','GOOG')
getSymbols(stock.names, from='2014-03-01',to='2014-05-31')   # get two assets at the same time
start(AAPL)
end(AAPL)
class(AAPL)
dim(AAPL)                                            # obs of AAPLE is bigger than that of GOOGLE
dim(GOOG)                                            # less than that of AAPL
assets <- merge.xts(AAPL, GOOG, all=FALSE)
dim(assets)

# (2) subset data
assets <- assets[,c('AAPL.Close', 'GOOG.Close')]     # select columns
assets['2014-05']                                    # select rows
assets[start(assets),]                               # find the first record
assets[end(assets),]                                 # find the last record
subset(assets, (assets[,1]>600) & (assets[,2]<550))  # use subset function

# (3) sample randomly and sort data
assets.ts <- as.timeSeries(assets)                 # change class of obj
class(assets.ts)
assets.samp <- sample(assets.ts, size=40)
dim(assets.samp)
print(assets.samp)
sort(assets.samp)

# (4) align data
assets.ali <- align(assets.samp, by='1d', method='before', include.weekends=FALSE)
dim(assets.ali)
print(assets.ali)

# (5) change frequency
assets.m <- to.monthly(assets)
print(assets.m)


# (6) roll data
rollapply <- function(x, by, FUN, ...){
  ans <- applySeries(x, from=by$from, to=by$to, by=NULL, FUN=FUN, format=x@format,
                     zone=finCenter(x), FinCenter=finCenter(x), title=x@title, documentation=x@documentation, ...)
  attr(ans, 'by') <- data.frame(from=format(by$from), to=format(by$to))
  ans
}

rts <- diff(log(assets))
rts <- returns(assets, method='continuous', percentage=TRUE)
rts <- as.timeSeries(na.omit(rts))
by <- periods(time(rts), period='1m', by='1d')
rts.roll <- rollapply(rts, by=by, FUN='colSums')
print(rts.roll)


# 2. process cross sectional data
# (1) reading data from EXCEL
Z <- odbcConnectExcel2007('FirmData.xlsx')                                # run faster
Data_Qvalue <- sqlFetch(Z, 'Qvalue', max=65536)
Data_Industry <- sqlFetch(Z, 'Industry', max=2717)
Data_Concent <- sqlFetch(Z, 'Concent', max=43227)
Data_Ownership <- sqlFetch(Z, 'Ownership', max=40899)
Data_Finance <- sqlFetch(Z, 'Finance', max=65532)
Data_Banlance <- sqlFetch(Z, 'Banlance', max=115619)
close(Z)
detach(package:RODBC)

tail(Data_Industry)
tail(Data_Concent)
tail(Data_Ownership)
tail(Data_Finance)
tail(Data_Banlance)

# (2) add names of firms
firmName <- function(name){
  name.rev <- paste('000000', as.character(name), sep='')
  name.rev <- substr(name.rev, start=nchar(name.rev)-5, stop=nchar(name.rev))
  name.c <- paste('c', name.rev, sep='')
  name.c
}

Data_Qvalue$Comcd <- firmName(Data_Qvalue$Stkcd)
Data_Industry$Comcd <- firmName(Data_Industry$Stkcd)
Data_Concent$Comcd <- firmName(Data_Concent$Stkcd)
Data_Ownership$Comcd <- firmName(Data_Ownership$Stkcd)
Data_Finance$Comcd <- firmName(Data_Finance$Stkcd)
Data_Banlance$Comcd <- firmName(Data_Banlance$Stkcd)


# (3) extract industry information in year 2012
# identify financial firm by '1'
Data_Industry <- Data_Industry[!is.na(Data_Industry$Indcd),]
NonfinComcd <- levels(factor(as.character(Data_Industry[Data_Industry$Indcd != 1, 'Comcd'])))
# keep non financial firm
Qvalue.Nonfin <- Data_Qvalue[(Data_Qvalue$Comcd %in% NonfinComcd), ]
Industry.Nonfin <- Data_Industry[(Data_Industry$Comcd %in% NonfinComcd), ]
Concent.Nonfin <- Data_Concent[(Data_Concent$Comcd %in% NonfinComcd), ]
Ownership.Nonfin <- Data_Ownership[(Data_Ownership$Comcd %in% NonfinComcd), ]
Finance.Nonfin <- Data_Finance[(Data_Finance$Comcd %in% NonfinComcd), ]
Banlance.Nonfin <- Data_Banlance[(Data_Banlance$Comcd %in% NonfinComcd), ]

# (4) select rows and cols
names.Qvalue <- c('Comcd', 'Accper', 'QVal')
names.Industry <- c('Comcd', 'Indcd')
names.Concent <- c('Comcd', 'Reptdt', 'OwnCon1', 'OwnCon5', 'OwnCon10')
names.Ownership <- c('Comcd', 'Reptdt', 'Indicator', 'State')
names.Finance <- c('Comcd', 'Accper', 'ROA', 'ROE')
names.Banlance <- c('Comcd', 'Accper', 'CurrentAsset', 'TotalAsset', 'CurrentLib', 'TotalLib')

Qvalue.tab <- Qvalue.Nonfin[substr(Qvalue.Nonfin$Accper, start=6, stop=7)=='12', names.Qvalue]
Industry.tab <- Industry.Nonfin[,names.Industry]
Concent.tab <- Concent.Nonfin[substr(Concent.Nonfin$Reptdt, start=6, stop=7)=='12', names.Concent]
Finance.tab <- Finance.Nonfin[substr(Finance.Nonfin$Accper, start=6, stop=7)=='12', names.Finance]
Banlance.tab <- Banlance.Nonfin[substr(Banlance.Nonfin$Accper, start=6, stop=7)=='12', names.Banlance]
Ownership.tab <- Ownership.Nonfin[Ownership.Nonfin$Indicator==1, names.Ownership]
Ownership.tab$state <- 0
Ownership.tab$state[(Ownership.tab$State == 1100) | (Ownership.tab$State == 2100)] <- 1

# (5) compute variables
Qvalue.tab$Enddt <- as.numeric(substr(Qvalue.tab$Accper, 1, 4))
Concent.tab$Enddt <- as.numeric(substr(Concent.tab$Reptdt, 1, 4))
Ownership.tab$Enddt <- as.numeric(substr(Ownership.tab$Reptdt, 1, 4))
Finance.tab$Enddt <- as.numeric(substr(Finance.tab$Accper, 1, 4))
Banlance.tab$Enddt <- as.numeric(substr(Banlance.tab$Accper, 1, 4))

Concent.tab$EBD <- Concent.tab$OwnCon5/Concent.tab$OwnCon1
Banlance.tab$Size <- log(Banlance.tab$TotalAsset)
Banlance.tab$Currt <- Banlance.tab$CurrentAsset/Banlance.tab$CurrentLib
Banlance.tab$AssLibRatio <- Banlance.tab$TotalLib/Banlance.tab$TotalAsset

# (6) merge data
KeyField1 <- 'Comcd'         # key varialbes for matching
KeyField2 <- 'Enddt'         # key varialbes for matching

Qvalue.Finance <- merge(Qvalue.tab, Finance.tab, by=c(KeyField1, KeyField2))
Qvalue.Finance.Concent <- merge(Qvalue.Finance, Concent.tab, by=c(KeyField1, KeyField2))
Qvalue.Finance.Concent.Ownership <- merge(Qvalue.Finance.Concent, Ownership.tab, by=c(KeyField1, KeyField2))
Qvalue.Finance.Concent.Ownership.Banlance <- merge(Qvalue.Finance.Concent.Ownership, Banlance.tab, by=c(KeyField1, KeyField2))
Qvalue.Finance.Concent.Ownership.Banlance.Industry <- merge(Qvalue.Finance.Concent.Ownership.Banlance, Industry.tab,
                                                            by=c(KeyField1))

# (7) remove NA values
var.names <- c("Comcd", "Enddt", "QVal", "ROA", "ROE", "OwnCon1", "OwnCon10", "EBD", "state","Size", 
               "CurrentAsset", "TotalAsset", "CurrentLib", "TotalLib", "Currt", "AssLibRatio", "Indcd")
Data <- Qvalue.Finance.Concent.Ownership.Banlance.Industry[ ,var.names]
Data <- na.omit(Data)

# (8) set dummy variables
Data$Year02 <- 0
Data$Year03 <- 0
Data$Year04 <- 0
Data$Year05 <- 0
Data$Year06 <- 0
Data$Year07 <- 0
Data$Year08 <- 0
Data$Year09 <- 0
Data$Year10 <- 0
Data$Year11 <- 0
Data$Year12 <- 0
Data$Year13 <- 0

Data[Data$Enddt == 2004, 'Year04'] <- 1
Data[Data$Enddt == 2005, 'Year05'] <- 1
Data[Data$Enddt == 2006, 'Year06'] <- 1
Data[Data$Enddt == 2007, 'Year07'] <- 1
Data[Data$Enddt == 2008, 'Year08'] <- 1
Data[Data$Enddt == 2009, 'Year09'] <- 1
Data[Data$Enddt == 2010, 'Year10'] <- 1
Data[Data$Enddt == 2011, 'Year11'] <- 1
Data[Data$Enddt == 2012, 'Year12'] <- 1
Data[Data$Enddt == 2013, 'Year13'] <- 1

dim(Data)
print(Data)

