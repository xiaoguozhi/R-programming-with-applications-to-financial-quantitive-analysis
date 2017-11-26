########################################################
# Description:
# 1.for Book 'R with applications to financial quantitive analysis'
# 2.Chapter: CH-11
# 3.Section: all
# 4.Purpose: sub function
# 5.Author: Qifa Xu
# 6.Founded: Sep 01, 2014.
# 7.Revised: Sep 20, 2014.
########################################################
# Contents:
# 1. calculate R1 for quantile regression model
# 2. do F test for quantile regression model
# 3. plot time series
# 4. calculate descriptive statistics
# 5. define check function
# 6. define AIC
#########################################################
# 1. calculate R1 for quantile regression model
R1.fit <- function(FORMULA, Y, DATA, TAUS){
  fit1 <- rq(FORMULA, data=DATA, tau=TAUS)
  fit0 <- rq(Y~1, tau=TAUS)
  R1 <- 1 - fit1$rho/fit0$rho
  return(R1)
}

# 2. do F test for quantile regression model
F.test <- function(FORMULA, Y, EXPLAINATORS, DATA, TAUS){
  fit1 <- rq(FORMULA, data=DATA, tau=TAUS)
  fit0 <- rq(Y~1, tau=TAUS)
  ESS <- fit0$rho - fit1$rho
  RSS <- fit1$rho
  df.ESS <- EXPLAINATORS
  df.RSS <- nrow(DATA) - df.ESS - 1
  
  F.stat <- (ESS/df.ESS)/(RSS/df.RSS)
  F.pvalue <- pf(F.stat, df.ESS, df.RSS, lower.tail = FALSE)
  return(data.frame(F.stat, F.pvalue))
}

# 3. plot time series
plotCS <- function(CS){
  win.graph(width=5, height=4.5)
  CS.ts <- ts(CS, frequency = 108, start=c(2005, 9))  #create time-series objects
  if (deparse(substitute(CS))== 'CSAD')                                  
    plot(CS.ts, main='SHCI-A',ylab='CSAD', xlab='时间')
  else
    plot(CS.ts, main='SHCI-A',ylab='CSSD', xlab='时间')
}


# 4. calculate descriptive statistics
data.statistic <- function(CS){
  n<-length(CS)                                                                             
  m<-mean(CS)
  v<-var(CS)
  s<-sd(CS)
  me<-median(CS)
  R<-max(CS)-min(CS)
  g1<-n/((n-1)*(n-2))*sum((CS-m)^3)/s^3
  g2<-(n*(n+1))/((n-1)*(n-2)*(n-3))*sum((CS-m)^4)/s^4-(3*(n-1)^2)/((n-2)*(n-3))
  data.frame(N=n,Mean=m,Var=v,Std1=s,Median=me,R=R,Skewness=g1,Kurtosis=g2,row.names=1)
}

# 5. define check function
rho <- function(u, tau) u*(tau-(u<0))

# 6. define AIC
AIC.qrnn <- function(y, yfit, n.parm, tau){
  N <- length(y)
  AIC <- log(mean(rho(y-yfit, tau))) + n.parm/N
  AIC
}
 
# AIC.min <- function(y, yfit, n.hidden, m, tau){
#   AICs <- numeric(length(n.hidden))
#   for (i in seq_along(n.hidden)){
#     J <- n.hidden[i]
#     n.parm <- m*J + 2*J + 1
#     AICs[i] <- AIC.rq(y, yfit, n.parm, tau)
#   }
#   AIC.opt <- AICs[which.min(AICs)]
#   n.hidden.opt <- n.hidden[which.min(AICs)]
#   data.frame(AIC.opt=AIC.opt, n.hidden.opt=n.hidden.opt)
# }
# 

