########################################################
# Description:
# 1.for Book 'R with applications to financial quantitive analysis'
# 2.Chapter: CH-06-02-01
# 3.Section: 6.2
# 4.Purpose: SV modeling through Kalman filter
# 5.Author: Qifa Xu
# 6.Founded: Dec 09, 2013.
# 7.Revised: Aug 06, 2014.
########################################################
# Contents:
# 1. read data
# 2. load filter function
# 3. SV modeling
# 4. compare distribution
# 5. volatility plot
#########################################################

# 0. initializing
setwd('F:/programe/book/R with application to financial quantitive analysis/CH-06')
rm(list=ls())

# 1. read data
# (1) get price
library(quantmod)
getSymbols('^SSEC', from='2008-01-01', to='2013-12-31')           # downloads daily price data of APPLE,please wait
dim(SSEC)                                                         # dimensions of the data
names(SSEC)                                                       # names of the data
detach(package:quantmod)

# (2) comput return of stocks
price.SSEC <- SSEC$SSEC.Adjusted
r.SSEC <- 100*diff(log(price.SSEC))                                # comput daily return
r <- as.numeric(r.SSEC[-1])                                        # remove NA code

# (3) logrithm square return
y <- log(r^2 + 1e-8)                                               # plus 1e-8 to grantee positive in log
sum(is.na(y))

# 2. load filter function
source('sub-06.R')

# 3. SV modeling
# (1) Initial Parameters
phi0<-0; phi1<-.95; sQ<-.2; alpha <- mean(y); mu1<- 0; sR1<-2; mu2<- 0; sR2<-2
init.par <- c(phi0, phi1, sQ, alpha, mu1, sR1, mu2, sR2)

# (2) Innovations Likelihood
# see sub-06.R

# (3) Estimation
est <- optim(init.par, Linn, y=y, method="BFGS", hessian=TRUE, control=list(trace=1,REPORT=1))
SE <- sqrt(diag(solve(est$hessian)))
t_test <- est$par/SE
prob <- 2*pt(abs(t_test), df=length(y)-1, lower.tail=FALSE)
u <- round(cbind(estimates=est$par, SE, t_test, prob), digits=4)
rownames(u)<-c("phi0","phi1","sQ","alpha","mu1","sigv1","mu2","sigv2")
print(u)

# 4. compare distribution
# (1) filters at the estimated parameters
phi0<-est$par[1]; phi1<-est$par[2]; sQ<-est$par[3]; alpha<-est$par[4];
mu1<-est$par[5]; sR1<-est$par[6]; mu2<-est$par[7]; sR2<-est$par[8]
sv <- SVfilter(y, phi0, phi1, sQ, alpha, mu1, sR1, mu2, sR2)

# (2) densities plot (f is chi-sq, fm is fitted mixture)
x <- seq(-15, 6, by=.01)
f <- exp(-.5*(exp(x)-x))/(sqrt(2*pi))
f1 <- exp(-.5*(x-mu1)^2/sR1^2)/(sR1*sqrt(2*pi))
f2 <- exp(-.5*(x-mu2)^2/sR2^2)/(sR2*sqrt(2*pi))
fm <- (f1+f2)/2

plot(density(y),  ylim=c(0, 0.25), main='', xlab='x', ylab='密度', lty=1, lwd=2, col='black')
lines(x, f, lty=2, lwd=2, col='blue')
lines(x, fm, lty=3,lwd=1, col='red')
legend('topleft', legend=c('y的核密度', '卡方自然对数', '拟合的混合正态'), 
       lty=c(1,2,3), lwd=c(2,1,1), col=c('black', 'blue','red'))

# 5. volatility plot
Time <- 1:100
par(mfrow=c(3,1))
plot(Time, y[Time], type='l', main='SSEC平方收益自然对数)', xlab='时间', ylab='y')
plot(Time, sv$xp[Time], type='l', main='预测波动的自然对数',
     ylim=c(-3.0, 2.5), xlab='时间', ylab='对数波动')
lines(Time, sv$xp[Time]+2*sqrt(sv$Pp[Time]), lty='dashed')
lines(Time, sv$xp[Time]-2*sqrt(sv$Pp[Time]), lty='dashed')
plot(Time, exp(sv$xp[Time]), type='l', main='预测波动', xlab='时间', ylab='波动率')
par(mfrow=c(1,1))
