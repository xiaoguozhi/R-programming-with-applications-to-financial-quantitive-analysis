########################################################
# Description:
# 1.for Book 'R with applications to financial quantitive analysis'
# 2.Chapter: CH-06
# 3.Section: all
# 4.Purpose: sub function
# 5.Author: Qifa Xu
# 6.Founded: Aug 01, 2014.
# 7.Revised: Aug 06, 2014.
########################################################
# Contents:
# 1. SVfilter through Kalman filter
# 2. innovations Likelihood
# 3. comput and plot durations
# 4. creates a series of intraday price
# 5. comput intradaily returns
#########################################################

# 1. SVfilter through Kalman filter
SVfilter <- function (y, phi0, phi1, sQ, alpha, mu1, sR1, mu2, sR2) 
{
  y = as.numeric(y)
  num = length(y)
  
  Q = sQ^2
  R1 = sR1^2
  R2 = sR2^2
  xp = matrix(0, num, 1)
  Pp = matrix(0, num, 1)
  xp[1] = 0
  Pp[1] = phi1^2 + Q
  pi1 = 1/2
  pi2 = 1/2
  pit1 = 1/2
  pit2 = 1/2
  like = 0
  for (i in 2:num) {
    sig1 = Pp[i - 1] + R1
    sig2 = Pp[i - 1] + R2
    k1 = phi1 * Pp[i - 1]/sig1
    k2 = phi1 * Pp[i - 1]/sig2
    e1 = y[i] - xp[i - 1] - mu1 - alpha
    e2 = y[i] - xp[i - 1] - mu2 - alpha
    den1 = (1/sqrt(sig1)) * exp(-0.5 * e1^2/sig1)
    den2 = (1/sqrt(sig2)) * exp(-0.5 * e2^2/sig2)
    denom = pi1 * den1 + pi2 * den2
    pit1[i] = pi1 * den1/denom
    pit2[i] = pi2 * den2/denom
    xp[i] = phi0 + phi1 * xp[i-1] + pit1[i]*k1*e1 + pit2[i]*k2*e2
    Pp[i] = (phi1^2)*Pp[i-1] + Q - pit1[i]*(k1^2)*sig1 - pit2[i]*(k2^2)*sig2
    like = like - 0.5 * log(pit1[i]*den1 + pit2[i]*den2)
    #     like = like - 0.5 * log(pi1 * den1 + pi0 * den0)
  }
  list(xp = xp, Pp = Pp, like = like)
}

# 2. innovations Likelihood
Linn <- function(para, y){
  phi0<-para[1]; phi1<-para[2]; sQ<-para[3]; alpha<-para[4];
  mu1<-para[5]; sR1<-para[6]; mu2<-para[7]; sR2<-para[8]
  sv <- SVfilter(y, phi0, phi1, sQ, alpha, mu1, sR1, mu2, sR2)
  return(sv$like)
}

# 3. comput and plot durations
compRtnDura <- function(da, plot.it){
  # da: input high frequency data
  # plot.it: logit variable to decide to plot
  sec=3600*da$hour+60*da$minute+da$second      # time in seconds
  ist=3600*9+30*60;  end=3600*16
  lunch=3600*12
  TT <- length(sec)                                       # sample size
  idx <- c(1:TT)[sec < ist]                               # before market opens
  jdx <- c(1:TT)[sec > end]                               # after market closes
  sec.norm <- sec[-c(idx,jdx)]                            # normal trading only
  # (1) comput price change in normal trading
  prs <- da$PRICE
  prc <- diff(prs)
  
  # (2) comput durations
  dt <- diff(sec.norm)                                    # comput time difference
  kdx <- c(1:length(dt))[dt >= 0]                         # non negative durations
  
  ti <- sec[2:length(sec.norm)]
  dt <- dt[kdx]
  ti <- ti[kdx]
  ratio <- sum(dt==0)/length(dt)                          # ratio of 0 interval
  
  if (plot.it){
    plot(sec,xlab='时间',ylab='持续期')                   # show trading time
    abline(h=ist, lty=2)
    abline(h=end, lty=2)
    text(15000, end+2000, paste('最终=',end))
    text(20000, ist-2000, paste('初始=',ist))
    
    plot(dt, type='l',xlab='时间',ylab='持续期')         # show durations
  }  
  
  results <- list(prs=prs, prc=prc, len.total=TT, len.norm=length(sec.norm),
                  len.duration=length(kdx), ratio=ratio)
  return(results)
}

# 4. extract trading info
extrTrad <- function(da, date, from, to, plot.ACF=TRUE){
  # da: data in the format: date, hour, minute, second, price, volume
  # date: specify a date
  # from: begin time
  # to: end time
  # plot.ACF: logit variable to decide to plot ACF
  
  if(!is.matrix(da))da=as.matrix(da)
  # (1) remove trade outside of the normal trading hours
  timemid <- da[,2]*3600+da[,3]*60+da[,4]
  da <- cbind(da[,1],timemid,da[,5])
  colnames(da) <- c('Date', 'Time', 'Price')
  
  # (2) set time interval
  data <- subset(da, da[,'Date']==paste(unlist(strsplit(date, '-')), collapse=''))
  TT <- nrow(data)
  istart <- as.numeric(substr(from, 1, 2))*3600+as.numeric(substr(from, 4, 5))*60
  iend <- as.numeric(substr(to, 1, 2))*3600+as.numeric(substr(to, 4, 5))*60
  idx <- c(1:TT)[(data[,2] >= istart) & (data[,2] <= iend)]
  data <- data[idx,]
  
  # (3) comput numbers of trade
  data.agg <- aggregate(data, by=list(data[,'Time']), length)
  ntrad <- data.agg$Date
  if (plot.ACF){
    par(mfrow=c(2,1))
    par(mar=c(4, 4, 2, 2))
    plot(ntrad, xlab='时间', ylab='交易数', type='l')
    acf(ntrad, xlab='滞后期', lag.max=200, main='')
  }
}

# 5. comput HF returns
HFrtn <- function (pdata, on="minutes", k = 5, marketopen="09:30:00", marketclose="16:00:00"){
  require(chron)
  require(timeDate)
  options(warn=-1)
  dates <- unique(format(time(pdata), "%Y-%m-%d"))
  cDays <- length(dates)
  rdata <- c()
#   browser()
  if (on == "minutes") {
    intraday <- seq(from=times(marketopen), to=times(marketclose), by=times(paste("00:0", k, ":00", sep = "")))
  }
  if (tail(intraday, 1) != marketclose) {
    intraday <- c(intraday, marketclose)
  }
  intraday <- intraday[2:length(intraday)]
  for (d in 1:cDays) {
    pdatad <- pdata[as.character(dates[d])]
    pdatad <- aggregatets(pdatad, on = on, k = k)
#     pdatad <- aggregatePrice(pdatad, on = on, k = k, marketopen = marketopen, marketclose = marketclose)
    z <- xts(rep(1, length(intraday)), order.by = timeDate(paste(dates[d],  as.character(intraday), sep = ""), format = "%Y-%m-%d %H:%M:%S"))
    pdatad <- merge.xts(z, pdatad)$ts
    pdatad <- na.locf(pdatad)
    rdatad <- makeReturns(pdatad)
#     rdatad <- rdatad[time(rdatad) > min(time(rdatad))]
    rdata <- rbind(rdata, rdatad)
  }
  
  out <- xts(rdata, order.by = time(rdata))
  names(out) <- 'returns'
  return(out)
}

# 6. comput descriptive statistics
stat <- function(x){
  if (!is.vector(x)) stop('argument x is not a vector, please check it.')
  if (!is.numeric(x)) stop('argument x is not a numeric, please check it.')
  
  mean <-  mean(x)
  median <- median(x)
  maximum <- max(x)
  minimum <- min(x)
  sd <- sd(x)
  skew <- moments::skewness(x)                                     # need package 'moments'
  kurt <- moments::kurtosis(x)                                     # need package 'moments'
  stats <- c(mean=mean, sd=sd, skew=skew, kurt=kurt, median=median, minimum=minimum, maximum=maximum)
  
  stats <- round(stats, digits=6)                                  # set digits
  return(stats)
}

# 7. define (weighted) sum of square function
sumsq <- function(x) sum(x^2)
wsumsq <- function(x, w) sum(w*x^2)

# 4. creates a series of intraday price
intraDay <- function(da, basic){
  # da: matrix consisting of trade-by-trade
  # basic: base time interval (measured in minutes)
  
  if(!is.matrix(da)) da=as.matrix(da)
  ist <- 9*3600+30*60
  int <- basic*60
  Nob <- 6.5*3600/int    # Nob is the number of intervals with length "int"
  
  y <- NULL
  TT=nrow(da)
  idx <- c(1:TT)[da[,2]==ist]
  if(length(idx)<1){
    y <- da[1,3]
    jused <- 1
  }
  else {
    y <- da[length(idx),3]
    jused <- length(idx)
  }
  for (i in 1:Nob){
    iend <- ist+i*int
    idx <- c(1:TT)[(da[,2]-iend) <= 0]
    jj <- idx[length(idx)]
    if(jj < jused){
      y <- c(y,y[length(y)])
    }
    else {
      jused <- jj
      y <- c(y,da[jj,3])
    }
  }
  list(Price=y)
}


# 5. comput intradaily returns
intraRtn <- function(Pr, basic, int){
  # Pr: the transaction price
  # int: time interal
  # basic: each basic interval
  multi = int/basic
  base = basic*60
  intval=int*60
  Nob = 6.5*3600/base
  TT = Nob/multi
  idx=c(1,c(1:TT)*multi+1)
  pp = log(Pr[idx])
  rtn=diff(pp)
  
  list(returns=rtn)
}

# 6. Compute intraday LOG returns & realized volatility & number of trades in each trading day
hfanal <- function(da, int, basic=1){
  # da: data in the format: date, hour, minute, second, price, volume
  # int: time interval in minutes for which returns to be computed.
  # basic: in minutes, the base interval to process transaction data.

  if(!is.matrix(da))da=as.matrix(da)
  # First, remove trade outside of the normal trading hours
  istart <- 9*3600+30*60
  iend <- 16*3600
  timemid <- da[,2]*3600+da[,3]*60+da[,4]
  da <- cbind(da[,1],timemid,da[,5])
  colnames(da) <- c("Date","Time","Price")
  T0 <- nrow(da)
  idx <- c(1:T0)[da[,2] >= istart]
  da <- da[idx,]
  T1 <- nrow(da)
  jdx <- c(1:T1)[da[,2] <= iend]
  da <- da[jdx,]
  TT <- nrow(da)
#   print(c(T0,T1,TT))
  
  Ytot <- NULL
  RV <- NULL
  logrtn <- NULL
  ntrad <- NULL
  cnt <- 0
  # Process through days
  while (cnt < TT){
    date <- da[(cnt+1),1]
    idx <- c(1:TT)[da[,1]==date]
    x <- da[idx,]
    ntrad <- c(ntrad,nrow(x))
    pp <- log(x[,3])
    r1 <- diff(pp)
    y1 <- sum(r1^2)*252
    Ytot <- c(Ytot,sqrt(y1))
    #
    m1 <- intraDay(x,basic)
    Pr <- m1$Price
    m2 <- intraRtn(Pr,basic,int)
    rtn <- m2$returns
    v1 <- sum(rtn^2)*252
    RV <- c(RV,sqrt(v1))
    #
    logrtn <- c(logrtn,rtn)
    cnt <- cnt+nrow(x)
#     print(cnt)
  }
  
  hfanal <- list(returns=as.numeric(logrtn), Ytot=Ytot, realized=RV, ntrad=ntrad)
}

# 7. estimate ACD models (autoregressive conditional duration)
acd <- function(x,order=c(1,1),cond.dist="exp",ini.est=NULL){
  # psi_t = omega + \sum_{i=1}^r alpha_i x_{t-i} + \sum_{j=1}^s \beta_j psi_{t-j}
  # This program is written by Ruey S. Tsay on May 18, 2011.
  ##
  r=order[1]
  s=order[2]
  ist=max(r,s)+1
  # Assign global variables
  Xacd <<- x
  Acdorder <<- order
  
  # Assign parameters
  S=1e-6; Mean=mean(x)
  inialpha=NULL; lowalpha=NULL; uppalpha=NULL
  if(r > 0){
    inialpha=rep(0.1/r,r); lowalpha=rep(S,r); uppalpha=rep(1-S,r)
  }
  inibeta=NULL; lowbeta=NULL; uppbeta=NULL
  if(s > 0){
    inibeta=rep(0.8/s,s); lowbeta=rep(S,s); uppbeta=rep(1-S,s)
  }
  
  if(cond.dist=="exp"){
    lowerB=c(omega=S,alpha=lowalpha,beta=lowbeta)
    upperB=c(omega=5*Mean,alpha=uppalpha,beta=uppbeta)
    if(length(ini.est)==0){
      params=c(omega=Mean,alpha=inialpha,beta=inibeta)
    }
    else{ 
      if(r > 0)inialpha=ini.est[2:(1+r)]
      if(s > 0)inibeta=ini.est[(1+r+1):(1+r+s)]
      params=c(omega=ini.est[1],alpha=inialpha,beta=inibeta)
    }
    ##
    fit = nlminb(start = params, objective = explk,
                 lower = lowerB, upper = upperB)
    ##lower = lowerB, upper = upperB, control = list(trace=3))
    epsilon = 0.0001 * fit$par
    npar=length(params)
    Hessian = matrix(0, ncol = npar, nrow = npar)
    for (i in 1:npar) {
      for (j in 1:npar) {
        x1 = x2 = x3 = x4  = fit$par
        x1[i] = x1[i] + epsilon[i]; x1[j] = x1[j] + epsilon[j]
        x2[i] = x2[i] + epsilon[i]; x2[j] = x2[j] - epsilon[j]
        x3[i] = x3[i] - epsilon[i]; x3[j] = x3[j] + epsilon[j]
        x4[i] = x4[i] - epsilon[i]; x4[j] = x4[j] - epsilon[j]
        Hessian[i, j] = (explk(x1)-explk(x2)-explk(x3)+explk(x4))/
          (4*epsilon[i]*epsilon[j])
      }
    }
    cat("Maximized log-likehood: ",-explk(fit$par),"\n")
    # Step 6: Create and Print Summary Report:
    se.coef = sqrt(diag(solve(Hessian)))
    tval = fit$par/se.coef
    matcoef = cbind(fit$par, se.coef, tval, 2*(1-pnorm(abs(tval))))
    dimnames(matcoef) = list(names(tval), c(" Estimate",
                                            " Std. Error", " t value", "Pr(>|t|)"))
    cat("\nCoefficient(s):\n")
    printCoefmat(matcoef, digits = 6, signif.stars = TRUE)
  }
  
  if(cond.dist=="weibull"){
    lowerB=c(omega=S,alpha=lowalpha,beta=lowbeta,shape=S)
    upperB=c(omega=5*Mean,alpha=uppalpha,beta=uppbeta,shape=10)
    if(length(ini.est)==0){
      params=c(omega=Mean,alpha=inialpha,beta=inibeta,shape=1)
    }
    else{ 
      if(r > 0)inialpha=ini.est[2:(1+r)]
      if(s > 0)inibeta=ini.est[(1+r+1):(1+r+s)]
      params=c(omega=ini.est[1],alpha=inialpha,beta=inibeta,shape=ini.est[1+r+s+1])
    }
    ##
    fit = nlminb(start = params, objective = weilk,
                 lower = lowerB, upper = upperB)
    ##lower = lowerB, upper = upperB, control = list(trace=3))
    epsilon = 0.0001 * fit$par
    npar=length(params)
    Hessian = matrix(0, ncol = npar, nrow = npar)
    for (i in 1:npar) {
      for (j in 1:npar) {
        x1 = x2 = x3 = x4  = fit$par
        x1[i] = x1[i] + epsilon[i]; x1[j] = x1[j] + epsilon[j]
        x2[i] = x2[i] + epsilon[i]; x2[j] = x2[j] - epsilon[j]
        x3[i] = x3[i] - epsilon[i]; x3[j] = x3[j] + epsilon[j]
        x4[i] = x4[i] - epsilon[i]; x4[j] = x4[j] - epsilon[j]
        Hessian[i, j] = (weilk(x1)-weilk(x2)-weilk(x3)+weilk(x4))/
          (4*epsilon[i]*epsilon[j])
      }
    }
    cat("Maximized log-likehood: ",-weilk(fit$par),"\n")
    # Step 6: Create and Print Summary Report:
    se.coef = sqrt(diag(solve(Hessian)))
    tval = fit$par/se.coef
    matcoef = cbind(fit$par, se.coef, tval, 2*(1-pnorm(abs(tval))))
    dimnames(matcoef) = list(names(tval), c(" Estimate",
                                            " Std. Error", " t value", "Pr(>|t|)"))
    cat("\nCoefficient(s):\n")
    printCoefmat(matcoef, digits = 6, signif.stars = TRUE)
  }
  
  if(cond.dist=="gamma"){
    lowerB=c(omega=S,alpha=lowalpha,beta=lowbeta,power=S*1000,shape=S)
    upperB=c(omega=5*Mean,alpha=uppalpha,beta=uppbeta,power=5,shape=30)
    if(length(ini.est)==0){
      params=c(omega=Mean,alpha=inialpha,beta=inibeta,power=1,shape=1.2)
    }
    else{ 
      if(r > 0)inialpha=ini.est[2:(1+r)]
      if(s > 0)inibeta=ini.est[(1+r+1):(1+r+s)]
      params=c(omega=ini.est[1],alpha=inialpha,beta=inibeta,power=ini.est[2+r+s],shape=ini.est[3+r+s])
    }
    ##
    fit = nlminb(start = params, objective = gamlk,
                 lower = lowerB, upper = upperB)
    ##lower = lowerB, upper = upperB, control = list(trace=3))
    epsilon = 0.0001 * fit$par
    npar=length(params)
    Hessian = matrix(0, ncol = npar, nrow = npar)
    for (i in 1:npar) {
      for (j in 1:npar) {
        x1 = x2 = x3 = x4  = fit$par
        x1[i] = x1[i] + epsilon[i]; x1[j] = x1[j] + epsilon[j]
        x2[i] = x2[i] + epsilon[i]; x2[j] = x2[j] - epsilon[j]
        x3[i] = x3[i] - epsilon[i]; x3[j] = x3[j] + epsilon[j]
        x4[i] = x4[i] - epsilon[i]; x4[j] = x4[j] - epsilon[j]
        Hessian[i, j] = (gamlk(x1)-gamlk(x2)-gamlk(x3)+gamlk(x4))/
          (4*epsilon[i]*epsilon[j])
      }
    }
    cat("Maximized log-likehood: ",-gamlk(fit$par),"\n")
    # Step 6: Create and Print Summary Report:
    se.coef = sqrt(diag(solve(Hessian)))
    tval = fit$par/se.coef
    matcoef = cbind(fit$par, se.coef, tval, 2*(1-pnorm(abs(tval))))
    dimnames(matcoef) = list(names(tval), c(" Estimate",
                                            " Std. Error", " t value", "Pr(>|t|)"))
    cat("\nCoefficient(s):\n")
    printCoefmat(matcoef, digits = 6, signif.stars = TRUE)
  }
  
  # compute standardized residuals
  par=fit$par
  T=length(x)
  psit=rep(par[1],(T-ist+1))
  if(r > 0){
    for (i in 1:r){
      psit=psit+par[1+i]*x[(ist-i):(T-i)]
    }
  }
  psi=psit
  if(s > 0){
    ini=rep(Mean,s)
    beta=par[(1+r+1):(1+r+s)]
    psi=filter(psit,beta,"r",init=ini)
  }
  resi=x[ist:T]/psi
  
  acd <- list(estimates=par,Hessian=Hessian,epsilon=resi)
}

# 8. define loglikelyhood
# (1) based on Exponential distribution
explk <- function(par){
  x=Xacd
  Mean=mean(x)
  order=Acdorder
  T=length(x)
  r=order[1]
  s=order[2]
  ist=max(r,s)+1
  psit=rep(par[1],(T-ist+1))
  if(r > 0){
    for (i in 1:r){
      psit=psit+par[1+i]*x[(ist-i):(T-i)]
    }
  }
  if(s > 0){
    ini=rep(Mean,s)
    beta=par[(1+r+1):(1+r+s)]
    psi=filter(psit,beta,"r",init=ini)
    ept=x[ist:T]
    explk=-sum(log(dexp(ept/psi)/psi))
  }
}

# (2) based on Weibull distribution
weilk <- function(par){
  x=Xacd
  order=Acdorder
  Mean=mean(x)
  T=length(x)
  r=order[1]
  s=order[2]
  ist=max(r,s)+1
  psit=rep(par[1],(T-ist+1))
  if(r > 0){
    for (i in 1:r){
      psit=psit+par[1+i]*x[(ist-i):(T-i)]
    }
  }
  if(s > 0){
    ini=rep(Mean,s)
    beta=par[(1+r+1):(1+r+s)]
    psi=filter(psit,beta,"r",init=ini)
  }
  ept=x[ist:T]/psi
  shape=par[r+s+2]
  c1=gamma(1+(1/shape))
  tmp1=shape*c1^shape*ept^(shape-1)/psi
  tmp=tmp1*exp(-(c1*ept)^shape)
  weilk = -sum(log(tmp))
}

# (3) based on Gamma distribution
gamlk <- function(par){
  x=Xacd
  order=Acdorder
  Mean=mean(x)
  T=length(x)
  r=order[1]
  s=order[2]
  ist=max(r,s)+1
  power=par[2+r+s]
  shape=par[3+r+s]
  c1=gamma(shape)
  c2=gamma(shape+(1/power))
  lambda=c1/c2
  c3=shape*power
  psit=rep(par[1],(T-ist+1))
  if(r > 0){
    for (i in 1:r){
      psit=psit+par[1+i]*x[(ist-i):(T-i)]
    }
  }
  psi=psit
  if(s > 0){
    ini=rep(Mean,s)
    beta=par[(1+r+1):(1+r+s)]
    psi=filter(psit,beta,"r",init=ini)
  }
  ept=x[ist:T]
  tmp1=log(power/c1)+(c3-1)*log(ept)-c3*log(lambda*psi)
  tmp= tmp1-(ept/(lambda*psi))^power
  gamlk=-sum(tmp)
}


spotVol_1 <- function (pdata, dailyvol = "bipower", periodicvol = "TML", on = "minutes", 
          k = 5, dummies = FALSE, P1 = 4, P2 = 2, marketopen = "09:30:00", 
          marketclose = "16:00:00") 
{
  require(chron)
  require(timeDate)
  dates = unique(format(time(pdata), "%Y-%m-%d"))
  cDays = length(dates)
  rdata = mR = c()
  if (on == "minutes") {
    intraday = seq(from = times(marketopen), to = times(marketclose), by = times(paste("00:0", k, ":00", sep = "")))
  }
  if (tail(intraday, 1) != marketclose) {
    intraday = c(intraday, marketclose)
  }
  intraday = intraday[2:length(intraday)]
  for (d in 1:cDays) {
    pdatad = pdata[as.character(dates[d])]
    pdatad = aggregatePrice(pdatad, on = on, k = k, marketopen = marketopen, marketclose = marketclose)
    z = xts(rep(1, length(intraday)), order.by = timeDate(paste(dates[d], as.character(intraday), sep = ""), format = "%Y-%m-%d %H:%M:%S"))
    pdatad = merge.xts(z, pdatad)$pdatad
    pdatad = na.locf(pdatad)
    rdatad = makeReturns(pdatad)
    rdatad = rdatad[time(rdatad) > min(time(rdatad))]
    rdata = rbind(rdata, rdatad)
    mR = rbind(mR, as.numeric(rdatad))
  }
  mR[is.na(mR)] = 0
  M = ncol(mR)
  if (cDays == 1) {
    mR = as.numeric(rdata)
    estimdailyvol = switch(dailyvol, bipower = rBPCov(mR), medrv = medRV(mR), rv = RV(mR))
  }
  else {
    estimdailyvol = switch(dailyvol, bipower = apply(mR, 1, "rBPCov"), medrv = apply(mR, 1, "medRV"), rv = apply(mR, 1, "RV"))
  }
  if (cDays <= 50) {
    print("Periodicity estimation requires at least 50 observations. Periodic component set to unity")
    estimperiodicvol = rep(1, M)
  }
  else {
    mstdR = mR/sqrt(estimdailyvol * (1/M))
    selection = c(1:M)[(nrow(mR) - apply(mR, 2, "countzeroes")) >= 20]
    selection = c(min(selection):max(selection))
    mstdR = mstdR[, selection]
    estimperiodicvol_temp = diurnal(stddata = mstdR, method = periodicvol, dummies = dummies, P1 = P1, P2 = P2)[[1]]
    estimperiodicvol = rep(1, M)
    estimperiodicvol[selection] = estimperiodicvol_temp
    mfilteredR = mR/matrix(rep(estimperiodicvol, cDays), byrow = T, nrow = cDays)
    estimdailyvol = switch(dailyvol, bipower = apply(mfilteredR, 1, "rBPCov"), medrv = apply(mfilteredR, 1, "medRV"), 
                           rv = apply(mfilteredR, 1, "RV"))
  }
  out = cbind(rdata, rep(sqrt(estimdailyvol * (1/M)), each = M) * rep(estimperiodicvol, cDays), rep(sqrt(estimdailyvol * 
                                                         (1/M)), each = M), rep(estimperiodicvol, cDays))
  out = xts(out, order.by = time(rdata))
  names(out) = c("returns", "vol", "dailyvol", "periodicvol")
  return(out)
}


