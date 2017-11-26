########################################################
# Description:
# 1.for Book 'R with applications to financial quantitive analysis'
# 2.Chapter: CH-09-02
# 3.Section: 9.2
# 4.Purpose: multifactors model
# 5.Author: Qifa Xu
# 6.Date: Aug 20, 2014.
# 7.Revised: Aug 24, 2014.
########################################################
# Contents:
# 1. load data from last example
# 2. set variables
# 3. make multifactors model through matrix operation
# 4. make multifactors model through OLS
#########################################################

# 0. initialize
# (1) set work path and workspace
setwd('F:/programe/book/R with application to financial quantitive analysis/CH-09')
rm(list=ls())

# (2) load packages
library(xlsx)                         # read data from EXCEL file
library(vars)                         # model VAR

# 1. read data from EXCEL file
stocks <- read.xlsx('MultiFactorData.xlsx', sheetName='stocks')
factors <- read.xlsx('MultiFactorData.xlsx', sheetName='factors')

# 2. set variables
# (1) set response variables
rts <- stocks[,1:13]
n.assets <- ncol(rts)
obs.assets <- nrow(rts)

# (2) set predictors
ord.choice <- VARselect(factors, lag.max=20)
print(ord.choice)                             # recommand p=3
model.VAR <- VAR(factors, p=3)                # fit factors data using VAR(3)
factor.surprise <- residuals(model.VAR)

fcts <- cbind(rep(1, obs.assets), factor.surprise[(nrow(factor.surprise)-obs.assets+1):nrow(factor.surprise),])


# 3. make multifactors model through matrix operation
# (1) estimate model
coefs <- qr.solve(fcts, rts)     #  solve(t(fcts)%*%fcts, t(fcts)%*%as.matrix(rts))
beta.hat <- t(coefs[2:3,])
rownames(beta.hat) <- colnames(rts)
print(beta.hat)

# (2) compute r square
res <- rts - fcts %*% coefs
D.hat <- crossprod(as.matrix(res))/(nrow(fcts)-ncol(fcts))
R2 <- 1 - diag(D.hat)/diag(var(rts))
R2

# 4. make multifactors model through OLS
# (1) estimate model
beta.hat <- matrix(NA, nrow=ncol(rts), ncol=2)
R2 <- matrix(NA, nrow=ncol(rts), ncol=1)
for (j in 1:ncol(rts)){
  model.lm <- lm(rts[,j]~fcts-1)
  model.sum <- summary(model.lm)
  print(model.sum)
  beta.hat[j,] <- coef(model.lm)[2:3]
  R2[j,] <- model.sum$r.squared
}
colnames(beta.hat) <- names(factors)
rownames(beta.hat) <- colnames(rts)
rownames(R2) <- colnames(rts)

# (2) show results
par(mfrow=c(1,3))
barplot(beta.hat[,1], horiz=TRUE, main='CPI的beta值')
barplot(beta.hat[,2], horiz=TRUE, main='CEN的beta值')
barplot(t(R2), horiz=TRUE, main=expression(R^2))
