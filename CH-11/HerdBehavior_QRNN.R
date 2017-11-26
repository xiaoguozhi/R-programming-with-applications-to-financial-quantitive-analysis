HerdBehavior_QRNN<-function(Data, CS){
  # 1. setup dataset
  Data_Ana <- data.frame(CS=CS, RM=Data$RM)
  x <- as.matrix(Data_Ana$RM)
  y <- as.matrix(Data_Ana$CS)
  
  # 2.  find the optimal n.hidden through AIC
  n.hidden <- 3:8
  m <- ncol(x)
  AICs <- numeric(length(n.hidden))
  for (i in 1:length(n.hidden)){
    set.seed(1)
    Fit.qrnn <- qrnn.fit(x, y, n.hidden=n.hidden[i], tau=0.5, n.trials=2)
    x.sort <- as.matrix(sort(x))
    yfit <- qrnn.predict(x.sort, Fit.qrnn)
    
    J <- n.hidden[i]
    n.parm <- m*J + 2*J + 1
    AICs[i] <- AIC.qrnn(y, yfit, n.parm, tau=0.5)
  }
  AIC.opt <- AICs[which.min(AICs)]
  n.hidden.opt <- n.hidden[which.min(AICs)]
  
  cat('################################', '\n')
  print(data.frame(AIC.opt=AIC.opt, n.hidden.opt=n.hidden.opt))
  cat('################################', '\n')

  # 3. fit a QRNN model
  win.graph(width=5,height=4.5) 
  Tau <- c(0.1, 0.5, 0.9)  
  plot(Data_Ana$RM, Data_Ana$CS, xlab='市场收益率', ylab='CSAD',main='神经网络分位数回归-CSAD')
  for (i in 1:length(Tau)){
    set.seed(1)
    Fit.qrnn <- qrnn.fit(x, y, n.hidden=n.hidden.opt, tau=Tau[i], n.trials=3)
    x.sort <- as.matrix(sort(x))
    Pre.qrnn <- qrnn.predict(x.sort, Fit.qrnn)
    lines(x.sort, Pre.qrnn,lty=i)
  }
  legend('bottomright', c('tau=0.1', 'tau=0.5', 'tau=0.9'), lty=1:length(Tau))
}

