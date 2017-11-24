########################################################
# Description:
# 1.for Book 'R with applications to financial quantitive analysis'
# 2.Chapter: CH-01-05-01
# 3.Section: 1.5
# 4.Purpose: define function
# 5.Author: Qifa Xu
# 6.Founded: Aug 01, 2014.
# 7.Revised: Aug 01, 2014.
########################################################
# Contents:
# 1. define a function
# 2. read data
# 3. compute descriptive statistics and check the normality of the data
# 4. debug the function
#########################################################
# 0. initializing
setwd('E:/programe/book/R with application to financial quantitive analysis/CH-01')
rm(list = ls())                   

# 1. define a function
stat <- function(x, plot.it=TRUE){
  if (!is.vector(x)) stop('argument x is not a vector, please check it.')
  if (!is.numeric(x)) stop('argument x is not a numeric, please check it.')
  mean <-  mean(x)
  median <- median(x)
  maximum <- max(x)
  minimum <- min(x)
  sd <- sd(x)
  skew <- moments::skewness(x)                                     # need package 'moments'
  kurt <- moments::kurtosis(x)                                     # need package 'moments'
  jbtst <- moments::jarque.test(x)                                 # need package 'moments'
  
  stats <- c(mean=mean, median=median, maximum=maximum, minimum=minimum, skew=skew, kurt=kurt)
  test <- c(JB=jbtst$statistic, p.value=jbtst$p.value)
  
  if (plot.it){
    par(mfrow=c(1,2))
    hist(x)
    qqnorm(x)
    qqline(x)
  }
  
  results<- list(stats=round(stats, digits=3), test=round(test, digits=3))   # set digits
  return(results)
}

# 2. read data
library(RODBC)                                                          # loading package
SSEC_Data <- odbcConnectExcel('CH-01-05.xls')                           # reading excel data
SSEC <-sqlFetch(SSEC_Data, 'SSEC')                                      # Extracting the data from SSEC workbook  
SSEC_shjc <- sqlFetch(SSEC_Data, 'SSEC-600009')                         # Extracting the data from SSEC-600009 workbook   
SSEC_scgf <- sqlFetch(SSEC_Data, 'SSEC-600008')                         # Extracting the data from SSEC-600008 workbook   
SSEC_zggm <- sqlFetch(SSEC_Data, 'SSEC-600007')                         # Extracting the data from SSEC-600007 workbook   
close(SSEC_Data)
detach(package:RODBC)


# 3. compute descriptive statistics and check the normality of the data
SSEC_shjc_stat <- stat(x=SSEC_shjc[,2], plot.it=TRUE)
SSEC_scgf_stat <- stat(x=SSEC_scgf[,2], plot.it=TRUE)
SSEC_zggm_stat <- stat(x=SSEC_zggm[,2], plot.it=TRUE)

stats <- rbind(SSEC_shjc_stat$stats, SSEC_scgf_stat$stats, SSEC_zggm_stat$stats)
rownames(stats) <- c('shjc', 'scgf', 'zggm')
print(stats)
tests <- rbind(SSEC_shjc_stat$test, SSEC_scgf_stat$test, SSEC_zggm_stat$test)
rownames(tests) <- c('shjc', 'scgf', 'zggm')
print(tests)


# 4. debug the function
rm(list=ls())
source('sub-01.R')
set.seed(12345)
data.sim.1 <- rnorm(1000)
data.sim.2 <- matrix(rnorm(2000), nrow=2)
stat(x=data.sim.1, plot.it=TRUE)           # argument x is a vector with normal distributed
stat(x=data.sim.2, plot.it=TRUE)           # argument x is not a vector


