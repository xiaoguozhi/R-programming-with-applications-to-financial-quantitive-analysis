########################################################
# Description:
# 1.for Book 'R with applications to multivariable linear regression model'
# 2.Chapter: CH-02-01
# 3.Section: 2.1.1
# 4.Purpose: multivariate linear regression model
# 5.Author: Qifa Xu, Zhifeng Guo
# 6.Date: Dec 09, 2013.
# 7.Revised: Aug 30, 2014.
########################################################
# Contents:
# 1. read data from EXCEL file
# 2. do logrithm operation
# 3. make linear model using lm
# 4. check model
# 5. extract informaiton of model
# 6. do forecast
#########################################################
# 0. initializing
# (1) set path
setwd('F:/programe/book/R with application to financial quantitive analysis/CH-02')
rm(list = ls())                   

# (2) load packages
library(xlsx)          # for xlsx data

# 1. read data from EXCEL file
dat <- read.xlsx(file='MacEcoData.xlsx', sheetName='data1')
# names(dat) <- c('y', paste('x', 1:3))

# 2. do logrithm operation
dat.log <- log(dat)
names(dat)

# 3. make linear model using lm
model.lm <- lm(M2~., data=dat.log)
class(model.lm)
print(model.lm)
summary(model.lm)

# 4. check model
# (1) based on design matrix
kappa(dat.log)                       # calculate conditional number
DAAG::vif(model.lm)                  # calculate variance inflation factor

# (2) based on residuals
par(mfrow=c(2,2))
plot(model.lm)
par(mfrow=c(1,1))

# 5. extract informaiton of model
# (1) get coefs
names(model.lm)
(coefs.lm <- coef(model.lm))   # or use: coefs.lm <- model.lm$coefficients

# (2) get fitted values
(fit.lm <- predict(model.lm, se.fit=TRUE))   # or use: fit.lm <- model.lm$fitted.values

# (3) get residuals
(res.lm <- resid(model.lm))   # or use: res.lm <- model.lm$residuals

# (4) get R square and sigma
mode(summary(model.lm))
names(summary(model.lm))
(R2 <- summary(model.lm)$r.squared)
(sigma <- summary(model.lm)$sigma)

# 6. do forecast
# (1) set values
tt <- 1:5
CPI.hat <- dat$CPI[nrow(dat)]*1.05^tt
ir.hat <- dat$ir[nrow(dat)]*1.00^tt
GY.hat <- dat$ir[nrow(dat)]*1.05^tt
NewData <- data.frame(CPI=CPI.hat, ir=ir.hat, GY=GY.hat)

# (2) predict
M2.hat <- predict(model.lm, newdata=log(NewData), interval='prediction')
matplot(M2.hat, lty=c(1,2,2), type='l', xlab='时期', ylab = "M2的预测值")




