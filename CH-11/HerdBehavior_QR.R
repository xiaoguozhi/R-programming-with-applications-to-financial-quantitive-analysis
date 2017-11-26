HerdBehavior_QR<-function(Data, CS, Result1, Result2){
  # 1. parametric quantile regression
  # (1) setup model
  if (deparse(substitute(CS))== 'CSAD')
    Formula <- CS ~ abs(Data$RM) + I(Data$RM^2)
  else
    Formula <- CS ~ Result1 + I(Result2^2)
  Tau <- seq(0.1, 0.9, by=0.1)
  
  # (2) estimate model
  QuanReg <- rq(Formula, tau=Tau) 
  cat('######################################', '\n')
  cat('summary of estimation', '\n')
  print(summary(QuanReg, se="nid"))
  cat('######################################', '\n')
  
#   summary(QuanReg)
#   summary(QuanReg, se="nid")
  
  R1.lm <- R1.fit(FORMULA=Formula, Y=CS,DATA=Data,TAUS=Tau)
  cat('######################################', '\n')
  cat('goodness of fit', '\n')
  print(R1.lm)
  cat('######################################', '\n')
  
  # 2. locally polynomial quantile regression
  # (1) selection of bandwidth
  win.graph(width=5,height=4.5)
  if (deparse(substitute(CS))== 'CSAD')
    plot(Data$RM, CS, xlab='市场收益率', ylab='CSAD',main='窗宽')
  else
    plot(Data$RM, CS, xlab='市场收益率', ylab='CSSD',main='窗宽')
  hs <- c(1, 2, 3)*10^(-1)
  for (i in 1:length(hs)){
    h <- hs[i]
    fit <- lprq(Data$RM, CS, h=h, tau=0.5)
    lines(fit$xx, fit$fv, lty=i)
  }
  legend('topright', c('h=1', 'h=2', 'h=3'), lty=1:length(hs))
  
  # (2) estimate LPQR
  win.graph(width=5,height=4.5)
  if (deparse(substitute(CS))== 'CSAD')
    plot(Data$RM, CS, xlab='市场收益率', ylab='CSAD',main='局部多项式分位数回归-CSAD')
  else
    plot(Data$RM, CS, xlab='市场收益率', ylab='CSSD',main='局部多项式分位数回归-CSSD')
  Tau <- c(0.1, 0.5, 0.9) 
  for (i in 1:length(Tau)){
    fit <- lprq(Data$RM, CS, h=0.1, tau=Tau[i])
    lines(fit$xx, fit$fv, lty=i)
  }
  legend('topright', c('tau=0.1', 'tau=0.5', 'tau=0.9'), lty=1:length(Tau))
  
  # 3. B-splines quantile regression
  win.graph(width=5,height=4.5)
  Data_Ana <- data.frame(CS=CS, RM=Data$RM)
  if (deparse(substitute(CS))== 'CSAD')
    plot(Data_Ana$RM, Data_Ana$CS, type='n', xlab='市场收益率', ylab='CSAD',main='B-样条分位数回归-CSAD')
  else
    plot(Data_Ana$RM, Data_Ana$CS, type='n', xlab='市场收益率', ylab='CSSD',main='B-样条分位数回归-CSSD')
  points(Data$RM, Data_Ana$CS, cex=0.75)
  for (i in 1:length(Tau)){
    fit <- rq(CS ~ bs(RM, df=7, degree=3), tau=Tau[i], data=Data_Ana)
    lines(sort(Data_Ana$RM), predict(fit, newdata=data.frame(RM=sort(Data_Ana$RM)), tau=Tau[i]), lty=i)
  }
  legend('bottomright', c('tau=0.1', 'tau=0.5', 'tau=0.9'), lty=1:length(Tau))
  
  F_test.nlm <- F.test(FORMULA=CS ~ bs(RM, df=7, degree=3),Y=Data_Ana$CS, EXPLAINATORS=7, DATA=Data_Ana, TAUS=Tau)
  cat('######################################', '\n')
  cat('F test', '\n')
  print(F_test.nlm)
  cat('######################################', '\n')
}