########################################################
# Description:
# 1.for Book 'R with applications to financial quantitive analysis'
# 2.Chapter: CH-08
# 3.Section: all
# 4.Purpose: sub function
# 5.Author: Qifa Xu
# 6.Founded: Aug 01, 2014.
# 7.Revised: Aug 20, 2014.
########################################################
# Contents:
# 1. solve quadratic programming with constraints
# 2. portfolio through quadratic programming with constraints
# 3. make portfolio through LASSO, adaptive LASSO, SCAD
# 3-1. compute portfolio throuth approximation
# 4. make comparison
# 5. set year and month
# 6. rolling window
# 7. compute covariance through Fama-French three factors
# 8. compute expected returns, variance, skewness and kurtosis
# 9. compute PGP
# 10. compute covariance, coskewness and cokurtosis matrix
# 11. make higher moments portfolio
# 12. define a function for descriptive statistics and normality test in Ch-01
#########################################################

# 1. solve quadratic programming with constraints
QPconst <- function(R, covM, cGross){
  # (1) set initial values
  p <- rep(1/ncol(R), times=ncol(R))   
  
  # (2) set objective function
  fn <- function(x){
    t(x) %*% covM %*% x
  }
  
  # (3) set constraints
  A <- matrix(rep(1, ncol(R)), ncol=ncol(R))
  lin.l <- 1
  lin.u <- 1
  nlcon <- function(x) sum(abs(x))   # norm(t(x))
  nlin.l <- -Inf
  nlin.u <- cGross
  
  # (4) optimization
  ret <- donlp2(p, fn, A=A, lin.l=lin.l, lin.u=lin.u,
                nlin=list(nlcon), nlin.u=nlin.u, nlin.l=nlin.l)
  weights <- ret$par
  weights[abs(weights)<1e-03] <- 0
  wOpt <- weights/sum(weights)
  varOpt <- ret$fx
  portfolioOpt <- R%*%wOpt
  
  # (5) output
  wOpt
  #   list(wOpt=wOpt, portOpt=portfolioOpt, varOpt=varOpt)
}


# 2. portfolio through quadratic programming with constraints
port_QP <- function(R, covM_act, covM_emp, c.values, choice='actRisk'){
  # (1) set variables
  wsOpt <- matrix(NA, nrow=length(c.values), ncol=ncol(R))
  portsOpt <- matrix(NA, nrow=length(c.values), ncol=nrow(R))
  volsOpt <- matrix(NA, nrow=length(c.values), ncol=1)
  n.assets <- matrix(NA, nrow=length(c.values), ncol=1)
  
  # (2) solve QP
  for (i in seq_along(c.values)){
    cat('the', i,'-th step, and the current gross para is:', c.values[i], '\n')
    wOpt <- QPconst(R=R, covM=covM_emp, cGross=c.values[i])
    wsOpt[i,] <- wOpt
    n.assets[i,1] <- sum(wOpt != 0)
    
    if (choice == 'actRisk'){
      volsOpt[i,1] <- sqrt(t(wOpt) %*% covM_act %*% wOpt)
    }else{
      volsOpt[i,1] <- sqrt(t(wOpt) %*% covM_emp %*% wOpt)
    }
  }
  
  # (3) make portfolio
  portfolioOpt <- wsOpt %*% t(R)
  
  # (4) output
  results <- list(wsOpt=wsOpt, portOpt=portfolioOpt, volsOpt=volsOpt, n.assets=n.assets)
  results
}

# 3. make portfolio through LASSO, adaptive LASSO, SCAD
port_method <- function(noshortsalePort, R, method='lars', plot.method=FALSE, plot.vol=FALSE){
  leftAssets <- R[,-ncol(R)]
  # (1) make model
  if (method=='lars'){
    las <- lars(x=noshortsalePort-leftAssets, y=noshortsalePort, normalize=FALSE, type='lar')
    if (plot.method) plot(las)
    w_star <- coef.lars(las)
    #     w_star[abs(w_star) < 1e-3] <- 0
    #     n.assets <- apply(w_star!=0, 1, sum)
  }
  if (method=='adalasso'){
    msgps <- msgps(X=noshortsalePort-leftAssets, y=noshortsalePort, penalty='alasso')
    if (plot.method) plot(msgps)
    d <- seq(0, 250, by=0.1)
    w_star <- coef(msgps, tuning=d)[-1,]
    w_star <- t(w_star)
    #     w_star[abs(w_star) < 1e-3] <- 0
    #     n.assets <- apply(w_star!=0, 1, sum)
  }
  if (method=='scad'){
    scad <- ncvreg(X=noshortsalePort-leftAssets, y=noshortsalePort, penalty='SCAD')
    if (plot.method) plot(scad)
    w_star <- scad$beta[-1,]
    w_star <- t(w_star)
    #     w_star[abs(w_star) < 1e-3] <- 0
    #     n.assets <- apply(w_star!=0, 1, sum)
  }
  
  # (2) reaarange weights
  d <- apply(abs(w_star), 1, sum)
  w <- cbind(w_star, 1-w_star %*% matrix(1, nrow=ncol(leftAssets), ncol=1))
  w[abs(w) < 1e-3] <- 0
  n.assets <- apply(w!=0, 1, sum)
  
  # (3) output
  results <- list(w=w, nAssets=n.assets, d=d)
  results
  
  #   # (2) make portfolio
  #   port <- w_star%*%t(leftAssets) + (1-w_star%*%matrix(1, nrow=ncol(leftAssets), ncol=1))%*%matrix(noshortsalePort, nrow=1)
  #   d <- apply(abs(w_star), 1, sum)
  #   vol <- apply(port, 1, sd)
  #   if (plot.vol) plot(d, vol, type='l', xlab='d', ylab='volatility')
  
  #   # (3) make output
  #   data.frame(d=d, nAsst=n.assets, vol=vol)
}

# 3-1. compute portfolio throuth approximation
portComp <- function(w, covM){
  vol <- numeric(nrow(w))
  for (i in 1:nrow(w)){
    vol[i] <- sqrt(w[i,,drop=FALSE] %*% covM %*% t(w[i,,drop=FALSE]))
  }
  
  vol
}



# 4. make comparison
compare <- function(port.lars, port.adalasso, port.scad){
  # (1) assets number
  plot(c(port.lars$d, port.adalasso$d, port.scad$d), c(port.lars$nAsst, port.adalasso$nAsst, port.scad$nAsst),
       type='n', xlab='d', ylab='number of selected assets')
  lines(port.lars$d, port.lars$nAsst, lty=1, lwd=2)
  lines(port.adalasso$d, port.adalasso$nAsst, lty=2, lwd=2)
  lines(port.scad$d, port.scad$nAsst, lty=4, lwd=2)
  legend('topleft', legend=c('lars', 'adalasso', 'scad'), lty=c(1,2,4))
  
  # (2) portfolio volatility
  plot(c(port.lars$d, port.adalasso$d, port.scad$d), c(port.lars$vol, port.adalasso$vol, port.scad$vol),
       type='n', xlab='d', ylab='number of selected assets')
  lines(port.lars$d, port.lars$vol, lty=1, lwd=2)
  lines(port.adalasso$d, port.adalasso$vol, lty=2, lwd=2)
  lines(port.scad$d, port.scad$vol, lty=4, lwd=2)
  legend('topright', legend=c('lars', 'adalasso', 'scad'), lty=c(1,2,4))
}

# 5. set year and month
year_mon <- function(years, months){
  ym <- NULL
  for (i in 1:length(years)){
    ym <- c(ym, paste(years[i], '-', months, sep='', collapse = ))
  }
  ym
}

# 6. rolling window
rollData <- function(data, year.month, step){
  dates.month <- substr(index(data), start=1, stop=7)
  #   browser()
  yms.in <- year.month[step:(step+11)]
  yms.out <- year.month[step+12]
  ind.in <- (dates.month == yms.in[1])
  for (i in 2:length(yms.in)){
    temp <- (dates.month == yms.in[i])
    ind.in <- ind.in | temp
  }
  ind.out <- (dates.month == yms.out)
  sample.in <- sample.all[ind.in,]
  sample.out <- sample.all[ind.out,]
  
  output <- list(sample.in=sample.in, sample.out=sample.out)
  output
}


# 6. compute covariance through Fama-French three factors
cov_FF <- function(Y, f){
  # (1) set obs matrix
  X <- f
  n <- ncol(Y)
  ones <- matrix(1, nrow=n, ncol=1)
  
  # (2) estimation
  B <- Y %*% t(X) %*% solve(X %*% t(X))
  E <- Y - B %*% X
  
  # (3) compute cov
  #   browser()
  #   cov.f <- X %*% t(X)/(n-1) - X %*% ones %*% t(ones) %*% t(X)/(n*(n-1))
  cov.f <- cov(t(f))
  #   sig.eps <- diag(diag(E %*% t(E)/n))
  sig.eps <- diag(diag(cov(t(E))))
  cov.Y <- B %*% cov.f %*% t(B) + sig.eps
  
  # (4) output
  cov.Y
}


################################################################
# functions for higher moments portfolio
################################################################
# 8. compute expected returns, variance, skewness and kurtosis
f_Exp <- function(w, ER){
  f <- t(w) %*% ER
  f
}

f_Var <- function(w, H){
  f <- t(w) %*% H %*% w
  f
}

f_Skew <- function(w, S){
  f <- t(w) %*% S %*% kronecker(w, w)
  f
}

f_Kurt <- function(w, K){
  f <- t(w) %*% K %*% kronecker(kronecker(w, w), w)
  f
}

# 9. compute PGP
MinZ <- function(w, R, H, S, K, R.star, H.star, S.star, K.star, lambda){
#   browser()
#   f1 <- abs(1-t(w) %*% R/R.star)^lambda[1] + abs(1-t(w) %*% S %*% kronecker(w,w)/S.star)^lambda[3]
#   f2 <- abs(t(w)%*%H%*%w/H.star-1)^lambda[2] + abs(t(w)%*%K%*%kronecker(kronecker(w,w),w)/K.star-1)^lambda[4]
#   f <- as.numeric(f1 + f2)
  f1 <- abs(1-f_Exp(w, ER=R)/R.star)^lambda[1] + abs(1-f_Skew(w, S)/S.star)^lambda[3]
  f2 <- abs(f_Var(w, H)-1)^lambda[2] + abs(f_Kurt(w, K)/K.star-1)^lambda[4]
  f <- as.numeric(f1 + f2)
  f
}

# 10. compute covariance, coskewness and cokurtosis matrix
HSK <- function(R){
  TT <- nrow(R)
  N <- ncol(R)
  ones <- matrix(rep(1,TT), nrow=TT, ncol=1)
  RminMean <- matrix(rts,nrow=TT,ncol=N)-ones%*%matrix(colMeans(rts), nrow=1,ncol=5)
  
  H <- matrix(NA, nrow=N, ncol=N)
  S <- matrix(NA, nrow=N, ncol=N^2)
  K <- matrix(NA, nrow=N, ncol=N^3)
  for (i in 1:N){
    for (j in 1:N){
      H[i,j] <- sum(RminMean[,i]*RminMean[,j])/(TT-1)
      for (k in 1:N){
        S[j, (i-1)*N+k] <- sum(RminMean[,i]*RminMean[,j]*RminMean[,k])/(TT-1)
        for (l in 1:N){
          K[k,(i-1)*N^2+(j-1)*N+l] <- sum(RminMean[,i]*RminMean[,j]*RminMean[,k]*RminMean[,l])/(TT-1)
        }
      }
    }
  }
  out <- list(H=H, S=S, K=K)
  out
}

# 11. make higher moments portfolio
HMPort <- function(w, R){
  comoments <- HSK(R)
  H <- comoments$H
  S <- comoments$S
  K <- comoments$K
  
  port.ret <- f_Exp(w, ER=colMeans(R))
  port.var <- f_Var(w, H)
  port.skew <- f_Skew(w, S)
  port.kurt <- f_Kurt(w, K)
  
  risk <- c(port.ret, port.var, port.skew, port.kurt)
  names(risk) <- c('return', 'variance', 'skewness', 'kurtosis')
  risk
}

# 12. define a function for descriptive statistics and normality test in Ch-01
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
  
  stats <- c(mean=mean, median=median, maximum=maximum, minimum=minimum, sd=sd, skew=skew, kurt=kurt)
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
