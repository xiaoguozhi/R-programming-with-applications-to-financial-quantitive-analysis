########################################################
# Description:
# 1.for Book 'R with applications to financial quantitive analysis'
# 2.Chapter: CH-09
# 3.Section: all
# 4.Purpose: sub function
# 5.Author: Qifa Xu
# 6.Founded: Aug 01, 2014.
# 7.Revised: Aug 20, 2014.
########################################################
# Contents:
# 1. plot CML
# 2. plot SML
# 3. convert daily data to monthly data
# 4. define payoff function
# 5. define B-S option pricing functions
# 6. define Monte Carlo function for B-S
#########################################################

# 1. define check function
rho <- function(tau, u){
  u*(tau - (u<0))
}

# 2. define loss function
loss <- function(tau, y, xi){
  sum(rho(tau, u=y-xi))
}

# 3. define objective function of qunantile
objFun <- function(tau, y, xi, plot.it=TRUE){
  R <- numeric(length(xi))
  
  for (i in 1:length(xi)){
    R[i] <- loss(tau=tau, y, xi[i])
  }
  
  if (plot.it){
    plot(xi, R, type='l', lwd=2, xlab=expression(xi), ylab='Ä¿±êº¯Êý')
    for (j in 1:length(y)){
      abline(v=y[j], lty=2)
    }
  }
  
  return(R)
}