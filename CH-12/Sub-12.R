########################################################
# Description:
# 1.for Book 'R with applications to financial quantitive analysis'
# 2.Chapter: CH-12
# 3.Section: all
# 4.Purpose: sub function
# 5.Author: Qifa Xu
# 6.Founded: Sep 01, 2014.
# 7.Revised: Sep 20, 2014.
########################################################
# Contents:
# 1. statistical summary
# 2. mean regression and quantile regression
# 3. time series and frequency transformation
# 4. mean regression
# 5. selection the optimal labmda by AICs
# 6. do quantile regression by lasso
#########################################################
# 1. statistical summary
statSummary <- function(x){
  n <- length(x)
  m <- mean(x)
  s <- sd(x)
  me <- median(x)
  max <- max(x)
  min <- min(x)
  g1 <- n/((n-1)*(n-2))*sum((x-m)^3)/s^3
  g2 <- ((n*(n+1))/((n-1)*(n-2)*(n-3))*sum((x-m)^4)/s^4
         - (3*(n-1)^2)/((n-2)*(n-3)))
  data.frame(N=n, Mean=m,std_dev=s, Median=me,Max=max,Min=min,Skewness=g1, Kurtosis=g2)
}

# 2. mean regression and quantile regression
Regress <- function(Data){
  LR <- lm(ExcessR~RMRF_tmv + SMB_tmv + HML_tmv, data=Data)
  QR <- rq(ExcessR~RMRF_tmv + SMB_tmv + HML_tmv, tau=taus, data=Data)
  list(coef(LR), coef(QR))
}

# 3. time series and frequency transformation
data.freq <- function(data, freq=c('Month')){
  # (1) time series transform
  rownames(data) <- data$date
  #     browser()
  data <- data[,-1,drop=FALSE]
  data <- as.xts(data)
  name <- names(data)
  
  # (2) frequency transformation
  data.xts <- NULL
  for (j in 1:ncol(data)){
    temp <- to.monthly(data[,j])
    data.xts <- cbind(data.xts, temp[,4])
  }
  names(data.xts) <- name
  return(data.xts)
}

# 4. mean regression
meanreg <- function(Y, X){
  # (1) total regression with all predictors
  data.designmatrix <- as.data.frame(cbind(Y, X))
  lm.sol<-lm(Y~., data=data.designmatrix)
  print(summary(lm.sol))
  
  # (2) do regression step by step
  lm.step <- step(lm.sol)   
  print(summary(lm.step))
  alpha <- coef(lm.step)[1]
  names(alpha) <- NULL
  
  # (3) significant variables
  names.sig <- names(lm.step$coef)[-1]
  
  # (4) output
  list(alpha=alpha, names.sig=names.sig)
}

# 5. selection the optimal labmda by AIC
sel.labmda <- function(Y, X, taus, lambdas){
  # (1) comput AIC vlaues
  data.matrixdesign <- as.data.frame(cbind(Y, X))
  AICs <- matrix(NA, nrow=length(lambdas), ncol=length(taus))
  n <- length(Y)
  k <- ncol(X)
  for (i in seq_along(lambdas)){
    f <- rq(Y~., data=data.matrixdesign, tau=taus, method="lasso", lambda = lambdas[i])
    AICs[i,] <- AIC(f)
  }
  
  # (2) select the optimal labmda
  lambdasOpt <- lambdas[apply(AICs, 2, which.min)]
  AICOpt <- AICs[apply(AICs, 2, which.min)]
  
  # (3) ouput
  results=list(lambdasOpt=lambdasOpt,AICOpt=AICOpt)
  return(results)
}

# 6. do quantile regression by lasso
rq.lasso <- function(Y, X, taus, lambdas, riskvalue=2, plot=FALSE){
  # (1) the optimal value of lambda and AIC
  selection <- sel.labmda(Y=Y, X=X, taus, lambdas, plot=plot)
  # (2) the coefficients of the lasso model
  coefs <- matrix(NA, nrow=ncol(X)+1, ncol=length(taus))
  for (i in seq_along(taus)){
    f <- rq(Y~X, tau=taus[i], method="lasso", lambda = selection$lambdasOpt[i])
    coefs[,i] <- coef(f)
  }
  rownames(coefs) <- c('alpha', colnames(X))
  colnames(coefs) <- paste('tau=', taus)
  # (3) compute alpha star
  alpha_star <- coefs['alpha', 3] - riskvalue/2*(coefs['alpha', 4]-coefs['alpha',2])
  # (4) output
  results <- list(lambdasOpt=selection$lambdasOpt, AICOpt=selection$AICOpt, 
                  coefs.lasso=coefs, alpha_star=alpha_star)
  return(results)
}

# 7. select parameters
selParms <- function(Y, X, taus, lamdsOpt, names.fact){
  coefs <- matrix(NA, nrow=ncol(X)+1, ncol=length(taus))
  for (i in seq_along(taus)){
    model.rq.lasso <- rq(Y~X, tau=taus[i], method="lasso", lambda=lamdsOpt[1,i])
    coefs[,i] <- coef(model.rq.lasso)
  }
  colnames(coefs) <- paste('tau=', taus, sep='')
  rownames(coefs) <- c('intercept', names.fact)
  round(coefs, digits=3)   
}
  
