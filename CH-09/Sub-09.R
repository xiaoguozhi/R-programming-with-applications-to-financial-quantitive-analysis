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

# 1. plot CML
CAPM_CML_PLOT <- function(Rb, Rf=0) 
{
  Rb.ncols <-  NCOL(Rb)
  CML.slopes <- numeric(Rb.ncols)
  for (j in 1:Rb.ncols){
    CML.slopes[j] <- CAPM.CML.slope(Rb=Rb[,j,drop=FALSE], Rf=Rf)
    sigma.M <- sd(Rb[,j], na.rm=TRUE)
    rts.M <- mean(Rb[,j], na.rm=TRUE)
    sigma <- seq(0, 2*sigma.M, length=100)
    rts <- Rf + CML.slopes[j]*sigma
    
    plot(sigma, rts, xlab=expression(sigma), ylab='E(Rp)', type='l',
         main=paste('CML:', colnames(Rb[,j,drop=FALSE])))
    points(sigma.M, rts.M, pch=19)
    lines(seq(0, sigma.M, length=100), rep(rts.M, 100), lty=2)
    lines(rep(sigma.M, 100), seq(0, rts.M, length=100), lty=2)
    text(sigma.M*(1+0.1), rts.M, 'M')
  }
}

# 2. plot SML
CAPM_SML_PLOT <- function(Ra, Rb, Rf=0) 
{
  Ra.ncols <-  NCOL(Ra)
  Rb.ncols <-  NCOL(Rb)
  
  SML.slopes <- numeric(Rb.ncols)
  
  for (j in 1:Rb.ncols){
    CAPM.betas <- CAPM.beta(Ra, Rb=Rb[,j,drop=FALSE], Rf=Rf)
    CAPM.means <- colMeans(Ra, na.rm=TRUE)
    
    SML.slopes[j] <- mean(Rb[,j], na.rm=TRUE) - Rf    # CAPM.SML.slope has an error
    beta.M <- 1
    rts.M <- Rf + SML.slopes[j]*beta.M
    betas <- seq(0, 2, length=100)
    rts <- Rf + SML.slopes[j]*betas
    rts.min <- min(c(CAPM.means, rts))

    plot(betas, rts, ylim=range(c(CAPM.means,rts)), xlab=expression(beta), ylab='E(R)', type='n',
         main=paste('SML:', colnames(Rb[,j,drop=FALSE])))
    abline(a=Rf, b=SML.slopes[j])
    points(beta.M, rts.M, pch=19)
    lines(seq(0, beta.M, length=100), rep(rts.M, 100), lty=2)
    lines(rep(beta.M, 100), seq(rts.min, rts.M, length=100), lty=2)
    text(beta.M*(1+0.1), rts.M, 'M')
    
    for (i in 1:Ra.ncols){
      points(CAPM.betas[i], CAPM.means[i])
      text(CAPM.betas[i]+0.15, CAPM.means[i], names(Ra[,i]))
    }
  }
}

# 3. convert daily data to monthly data
conv_to_mon <- function(rt){
  tmp <- to.monthly(rt)
  rt.mon <- tmp[,4,drop=FALSE]
  colnames(rt.mon) <- colnames(rt)
  return(rt.mon)
}

# 4. define payoff function
payoff_call <- function(S, K){
  sapply(S, function(S, K) max(c(S-K, 0)), K=K)
}
payoff_put <- function(S, K){
  sapply(S, function(S, K) max(c(K-S, 0)), K=K)
}


# 4. define B-S option pricing functions
price_call <- function(S=1, t=0, TT=1, r=1, sigma=1, K=1){
  d2 <- (log(S/K) + (r-0.5*sigma^2)*(TT-t))/(sigma*(sqrt(TT-t)))
  d1 <- d2 + sigma*sqrt(TT-t)
  ans <- S*pnorm(d1) - K*exp(-r*(TT-t))*pnorm(d2)
  ans
}

price_put <- function(S=1, t=0, TT=1, r=1, sigma=1, K=1){
  d2 <- (log(S/K) + (r-0.5*sigma^2)*(TT-t))/(sigma*(sqrt(TT-t)))
  d1 <- d2 + sigma*sqrt(TT-t)
  ans <- K*exp(-r*(TT-t))*pnorm(-d2) - S*pnorm(-d1)
  ans
}

# 5. define Monte Carlo function for B-S
price_MonCar <- function(S=1, t=0, TT=1, r=1, sigma=1, K=1, f, N=200){
  exp_f <- function(N){
    #     u <- rnorm(N, mean=0, sd=1)
    #     tmp <- S*exp((r-0.5*sigma^2)*(TT-t)+sigma*sqrt(TT-t)*u)
    u <- rnorm(floor(N/2))
    tmp1 <- S*exp((r-0.5*sigma^2)*(TT-t)+sigma*sqrt(TT-t)*u)
    tmp2 <- S*exp((r-0.5*sigma^2)*(TT-t)+sigma*sqrt(TT-t)*(-u))
    tmp <- c(tmp1, tmp2)
    mean(sapply(tmp, payoff_call, K=K))
  }
  expf <- exp_f(N=N)
  ans <- exp(-r*(TT-t)) * expf
  ans
}