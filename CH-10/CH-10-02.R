########################################################
# Description:
# 1.for Book 'R with applications to financial quantitive analysis'
# 2.Chapter: CH-10-02
# 3.Section: 10.2
# 4.Purpose: common persistence
# 5.Author: Qifa Xu, Lin Kang
# 6.Date: May 10, 2014.
# 7.Revised: Aug 24, 2014.
########################################################
# Contents:
# 1. reading data
# 2. Unit Root Test
# 3. Johansen Cointegration Test
# 4. Johansen Cointegration Test with structural shift
#############################################################
# 0. initializing
# (1) set path
setwd('F:/programe/book/R with application to financial quantitive analysis/CH-10')
rm(list=ls())

# (2) load packages
library("R.matlab")                       # for connect to MATLAB
library('RODBC')                          # for reading EXCEL file

# 1. Start matlab
# (1) start matlab from R
Matlab$startServer()
# (2) start matlab externally
print(system.file('externals', package='R.matlab'))

# 2. Create a matlab client object used to communicate with matlab
matlab <- Matlab()
print(matlab)
isOpen <- open(matlab)
print(isOpen)
print(matlab)

# 3. read data from EXCEL file
stock.file = odbcConnectExcel('StockPriceIndex.xls')    
stock.data = sqlFetch(stock.file, 'Sheet1')    
close(stock.file)

# 4. compute returns and send them to matlab
SH <- stock.data$F1
SZ <- stock.data$F2
DLNSH <- diff(log(SH))
DLNSZ <- diff(log(SZ))
p <- q <- 1
setVariable(matlab, DLNSH=DLNSH)
setVariable(matlab, DLNSZ=DLNSZ)
setVariable(matlab, p=p)
setVariable(matlab, q=q)


#4. Estimate a full BEKK multivariate GARCH model
# (1) orgnize data in matlab
evaluate(matlab, 'data=[DLNSH, DLNSZ]')

#(2) Parameter estimation and  volatility estimation
# evaluate(matlab, 'cd H:/programe/book/CH-10')
evaluate(matlab, '[parameters, loglikelihood, Ht, likelihoods, stdresid, stderrors, A, B, scores]  = full_bekk_mvgarch(data,p,q,[])')
result <- getVariable(matlab, c('parameters', 'Ht'))

#(3) plotting  volatility of DLNSH and DLNSZ
par(mfrow= c(1,2),mar= c(4.5,3,2,2))
plot(result$Ht[1,1,],type="l",main="上证综指条件波动率估计")
plot(result$Ht[2,2,],type="l",main="深证成指条件波动率估计")


#(4) compute engien value and vector for judgement
evaluate(matlab, '[B0, B1, B2, V, D] = solEngi(data, parameters)')
engi <- getVariable(matlab, c('B0', 'B1', 'B2', 'V', 'D'))
(engi.vec <- as.numeric(engi$V[,1]))

# 5.  Done: close the matlab clinet
close(matlab)
print(matlab)
