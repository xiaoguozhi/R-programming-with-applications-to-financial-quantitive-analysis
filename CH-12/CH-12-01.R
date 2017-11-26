########################################################
# Description:
# 1.for Book 'R with applications to financial quantitive analysis'
# 2.Chapter: CH-12-01
# 3.Section: 12.1
# 4.Purpose: predict bankruptcy
# 5.Author: Qifa Xu, Kexin Zhang
# 6.Date: Apr 03, 2014.
# 7.Revised: Sep 01, 2014.
########################################################
# Contents:
# 1. read data from EXCEL file
# 2. run SVM to perform the classification
# 3. run L1 logit to perform the classification
# 4. compare performances of two models
#############################################################
# 0. initializing
# (1) set path
setwd('F:/programe/book/R with application to financial quantitive analysis/CH-12')
rm(list=ls())

# (2) load packages
library(RODBC)                  # for reading EXCEL file
library(e1071)                  # for SVM
library(glmnet)                 # for Lasso and elastic-net regularized generalized linear models
library(ROCR)                   # for ROC curve

# 1. read data from EXCEL file
connExcel <- odbcConnectExcel("data.xls")
stocks <- sqlFetch(connExcel,"sheet1")
close(connExcel)
rm(connExcel)
X <- as.matrix(stocks[-c(1,2),3:10])
colnames(X) <- paste('x', 1:ncol(X), sep='')
y <- as.factor(stocks[-c(1,2),11])

# 2. run SVM to perform the classification
# (1) estimate SVM
svm1 <- svm(X,y, probability = TRUE, kernel="linear")
svmmodel.cv <- svm(X, y, probability = TRUE, kernel="linear", cross=10)

# (2) predict
pred_svm <- predict(svmmodel.cv, X, probability = TRUE)
pro_svm <- attr(pred_svm, "probabilities")[,1]
table(y, pred_svm)
(right <- sum(y==pred_svm)/length(y))

# (3) plot accuracy and ROC curve
pred_s <- prediction(pro_svm, y)
par(mfrow=c(1,2))
acc_s<-plot(performance(pred_s, 'acc'), main='精度曲线', xlab='截断值', ylab='精度')
roc_s<-plot(performance(pred_s, 'tpr', 'fpr'), main='ROC曲线', xlab='假阳性率', ylab='真阳性率')
abline(0, 1, lty=2)

# 3. run L1 logit to perform the classification
# (1) estimate model
set.seed(1)
logistic.l1 = cv.glmnet(as.matrix(X), y=y, family="binomial", nfolds=10, alpha = 1, standardize = TRUE)

# (2) plot cross validation
par(mfrow=c(1,1))
plot(logistic.l1, xlab='Lambda对数值', ylab='二元偏差')
coef(logistic.l1)
sum(coef(logistic.l1)!=0) 
rownames(coef(logistic.l1))[which(!(coef(logistic.l1)==0))]
log(logistic.l1$lambda.min) # the value of lambda that minimizes the cross-validation (CV) error

# (3) predict
pred_l1 <- predict(logistic.l1, newx=as.matrix(X), s="lambda.min", type="response")
table(round(pred_l1),  y)
(right <- sum(y==round(pred_l1))/length(y))

# (4) plot accuracy and ROC curve
pred_l <- prediction(pred_l1, y)
par(mfrow=c(1,2))
acc_l<-plot(performance(pred_l, 'acc'), main='精度曲线',lty=1, xlab='截断值', ylab='精度')
roc_l<-plot(performance(pred_l, 'tpr', 'fpr'), main='ROC曲线',lty=1, xlab='假阳性率', ylab='真阳性率')
abline(0, 1, lty=2)
par(mfrow=c(1,1))

# 4. compare performances of two models
par(mfrow=c(1,2))
# (1) plot accuracy curve
acc_svm <- performance(pred_s, 'acc')
acc_l1 <- performance(pred_l, 'acc')
plot(unlist(acc_svm@x.values), unlist(acc_svm@y.values), type='l', xlab='截断值', ylab='精度')
lines(unlist(acc_l1@x.values), unlist(acc_l1@y.values), lty=2)
legend('bottomright', legend=c('SVM', 'L1 Logit'), lty=c(1,2))

# (2) plot ROC curve
roc_svm <- performance(pred_s, 'tpr', 'fpr')
roc_l1 <- performance(pred_l, 'tpr', 'fpr')
plot(unlist(roc_svm@x.values), unlist(roc_svm@y.values), type='l', xlab='假阳性率', ylab='真阳性率')
lines(unlist(roc_l1@x.values), unlist(roc_l1@y.values), lty=2)
abline(0, 1, lty=2)
legend('bottomright', legend=c('SVM', 'L1 Logit'), lty=c(1,2))

par(mfrow=c(1,1))

