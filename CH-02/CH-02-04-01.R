########################################################
# Description:
# 1.for Book 'R with applications to financial quantitive analysis'
# 2.Chapter: CH-02-04-01
# 3.Section: 2.4
# 4.Purpose: optimize theory
# 5.Author: Qifa Xu
# 6.Founded: Aug 03, 2014.
# 7.Revised: Sep 02, 2014.
########################################################
# Contents:
# 1. load packages
# 2. setup data set
# 3. select optimal lags
# 4. estimate VAR(1) model
# 5. comput IRF and FEVD
#########################################################
# 0. initializing
# (1) set path
setwd('F:/programe/book/R with application to financial quantitive analysis/CH-02')
rm(list = ls())                   

# (2) load packages
source('Sub-02.R')                 # our own functions


# 1. solve a nonlinear equation
# (1) define a loss function
f <- function(r, p, Cs){
  n <- length(Cs)
  tt <- 1:n
  loss <- p - sum(Cs/((1+r)^tt))
  loss
}

# (2) find the solution
Cs <- c(2000, 2000, 2500, 4000)
P <- 7704
uniroot(f, c(0,1), p=P, Cs=Cs)           # find the zero root of f function

# 2. optimize a nonlinear function
# (1) optimize
g <- function(r, p, Cs) {f(r, p, Cs)^2}   # define g function: g=f^2
optimize(g, c(0,1), p=P, Cs=Cs)           # optimize g function

# (2) compare tow approaches
rs <- seq(0, 1, length=100)
fval <- gval <- numeric(length(rs))
for (i in seq_along(rs)){
  fval[i] <- f(r=rs[i], p=P, Cs=Cs)
  gval[i] <- g(r=rs[i], p=P, Cs=Cs)
}

par(mfrow=c(2,1))
plot(rs, fval, type='l', xlab='r', ylab='f')
abline(h=0, lty=2)
plot(rs, gval, type='l', xlab='r', ylab='g')
abline(h=0, lty=2)
par(mfrow=c(1,1))

# 3. compare nonlinear optimize and L1 statistics
# (1) do L1 statistics
set.seed(1)
y <- rnorm(n=10, mean=0, sd=1)
xi <- seq(min(y), max(y), length=100)
TAU <- 0.5

(R <- objFun(tau=TAU, y, xi, plot.it=TRUE))                 # calculate objective function with different xi's
(xi_est <- xi[which.min(R)])                                # estimate xi which minimize objective function
(y_qua <- quantile(y, prob=TAU))                            # quantile of observations which is the same as minimization point

# (2) optimize the nonlinear objective function
optimize(objFun, c(-2,2), tau=TAU, y=y, plot.it=FALSE)           # optimize an objective function of L1 statistics


# 4. compare quadratic programming and OLS
# (1) generate data
beta <- c(5, 2)
sigma <- 1
n <- 100
set.seed(1)
eps <- rnorm(n, mean=0, sd=1)
x <- runif(n, min=-10, max=10)
y <- beta[1] + beta[2]*x + sigma*eps

# (2) do linear regression by OLS
model.lm <- lm(y~x)
(coef.OLS <- coef(model.lm))

# (3) solve quadratic programming by optim function
lossQuad <- function(betaHat, x, y){
  sum((y-betaHat[1]-betaHat[2]*x)^2)
}
optim(par=coef.OLS, fn=lossQuad, y=y, x=x)           # optimize a loss function



