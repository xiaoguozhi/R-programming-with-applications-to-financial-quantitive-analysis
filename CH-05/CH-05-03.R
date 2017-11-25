########################################################
# Description:
# 1.for Book 'R with applications to financial quantitive analysis'
# 2.Chapter: CH-05-03
# 3.Section: 5.3
# 4.Purpose: calculate endougeneous returns
# 5.Author: Qifa Xu, Yingying Zhou
# 6.Date: Jan 11, 2014.
# 7.Revised: Aug 20, 2014.
########################################################
# Contents:
# 1. calculate endougeneous returns
# 2. calculate expired returns
# 3. calculate effective annualized returns
# 4. calculate duration
# 5. calculate convexity
#########################################################
# 0. initialize
# (1) set work path
setwd('F:/programe/book/R with application to financial quantitive analysis/CH-05')
rm(list=ls())
# (2) source packages

# 1. calculate endougeneous returns
# (1) define a loss function
f <- function(r, p, Cs){
  n <- length(Cs)
  tt <- 1:n
  loss <- p - sum(Cs/((1+r)^tt))
  loss
}

# (2) find endougeneous returns
Cs <- c(20000, 20000, 25000, 40000)
p <- 77040
uniroot(f, c(0,1), p=p, Cs=Cs)          # find the zero root of f function

# 2. calculate expired returns
# (1) define a loss function
f_exp <- function(r, p, Cs, Cp){
  n <- length(Cs)
  tt <- 1:n
  loss <- p - sum(Cs/((1+r)^tt)) - Cp/(1+r)^n
  loss
}

# (2) find endougeneous returns for example 5-5
Cs <- rep(2000000, times=40)
Cp <- 25000000
p <- 23640000
uniroot(f_exp, c(0,1), p=p, Cs=Cs, Cp=Cp)          # find the zero root of f function


# (3) find endougeneous returns for example 5-6
Cs <- rep(950000, times=15)
Cp <- 15000000
p <- 22710000
r.expected <- 0.067
r.half <- uniroot(f_exp, c(0,1), p=p, Cs=Cs, Cp=Cp)$root          # find the zero root of f function
r.annulized <- r.half*2
if (r.annulized>=r.expected){
  cat('Do it, and the endougeneous return is', r.annulized, '\n')
}else{
  cat('Not do it, and the endougeneous return is', r.annulized, '\n')
}

# 3. calculate effective annualized returns
# (1) define a function
eff_rts <- function(pr,m){
  er<-(1+pr)^m-1  
}

# (2) do calculation
(da1 <- eff_rts(0.05,2))
(da2 <- eff_rts(0.025,4))


# 4. calculate duration
# (1) define function
jq <- function(y,cupon,period,p0){
  c2<-0
  tc2<-0
  for(n in 1:period){
    t=n
    if(n<period) c<-cupon else if(n==period) c<-cupon+p0
    c1=c/(1+y)^n
    tc1<-t*c1
    c2<-c2+c1
    tc2<-tc2+tc1
    if(n==period) {
      d<-tc2/(c2*2)            # duration
      md<-d/(1+y)              # adjusted duration
    }
  }
  list(d=d,md=md)
}

# (2) do calculation
(re<-jq(0.05,5,10,2000))


# 5. calculate convexity
# (1) define a function
td <- function(y, cupon, period, p0){
  c2<-0
  tc2<-0
  for(n in 1:period){
    t=n
    if(n<period) c<-cupon else if(n==period) c<-cupon+p0
    c1=c/(1+y)^n
    tc1<-t*(t+1)*c1
    c2<-c2+c1
    tc2<-tc2+tc1
    if(n==period){
      covex<-tc2/(c2*((1+y)^2))
      yearlycovex<-covex/4
    }
  }
  list(covex=covex,yearlycovex=yearlycovex)
}

# (2) do calculation
(re <- td(0.05,5,10,1500))

# (3) calculate the impact of convexity
delta.r <- 0.04
(effects <- 0.5*re$yearlycovex*delta.r^2)

