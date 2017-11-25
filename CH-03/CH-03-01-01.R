########################################################
# Description:
# 1.for Book 'R with applications to financial quantitive analysis'
# 2.Chapter: CH-03
# 3.Section: 3.1
# 4.Purpose: artificial neural network
# 5.Author: Qifa Xu, Zhifeng Guo
# 6.Date: Dec 09, 2013.
# 7.Revised: Aug 28, 2014.
########################################################
# Contents:
# 1. setup dataset
# 2. make BP neural network
# 3. make MLP neural network
# 4. make RBF neural network
# 5. make SVM model
#########################################################
# 0. Initializing
# (1) set path
setwd('F:/programe/book/R with application to financial quantitive analysis/CH-03')
rm(list = ls())

# (2) load packages
library(neuralnet)                # for neural network
library(monmlp)                   # for MLP
library(RSNNS)                    # for RBF
library(e1071)                    # for SVM
library(rminer)                   # for SVM
library(xlsx)                     # for excel data
library(ROCR)                     # for ROCR curve

# 1. setup dataset
# (1) read data from EXCEL file
dat <- read.xlsx(file='StockData.xlsx', sheetIndex='Sheet1')

# (2) name data
names(dat) <- c(paste('x',1:7,sep=''), 'y')
summary(as.factor(dat[,'y']))

# 2. make BP neural network
# (1) estimate model
set.seed(12345)
model.BP <- neuralnet(y~x1+x2+x3+x4+x5+x6+x7, dat, hidden=10, threshold=0.0001)

# (2) plot BPNN
plot(model.BP, rep="best")

# (3) do classification
class.BP <- round(compute(model.BP, dat[,-8])$net.result)
DAAG::confusion(class.BP, dat[,8])
plot(performance(prediction(class.BP, y), "tpr","fpr"))       # plot ROC
performance(prediction(class.BP, y), "auc")@y.values[[1]]     # accuracy


# 3. make MLP neural network
# (1) estimate model
set.seed(12345)
x <- as.matrix(dat[,1:7])
y <- as.matrix(dat[,8])
model.MLP <- monmlp.fit(x=x, y=y, hidden1=10, n.ensemble=15, monotone=1, bag=TRUE)
y.hat <- monmlp.predict(x=x, weights=model.MLP)

# (2) show results
DAAG::confusion(round(y.hat), y)             # calculate confusion matrix
plot(performance(prediction(y.hat, y), "tpr","fpr"))       # plot ROC
performance(prediction(y.hat, y), "auc")@y.values[[1]]     # accuracy


# 4. make RBF neural network
# (1) estimate model
set.seed(12345)
inputs <- x
colnames(inputs) <- NULL
outputs <- normalizeData(y, "0_1")
model.RBF <- rbf(inputs, outputs, size=10, maxit=1000)

# (2) show results
plotIterativeError(model.RBF)                              # show iterations
y.hat <- predict(model.RBF, inputs)
DAAG::confusion(round(y.hat), y)                           # calculate confusion matrix
plot(performance(prediction(y.hat, y), "tpr","fpr"))       # plot ROC
performance(prediction(y.hat, y), "auc")@y.values[[1]]     # accuracy


# 5. make SVM model
# (1) set index for K-fold CV
n <- nrow(dat)                                # sample size
ind.samp <- 1:n                               # index of sample
K <- 10                                       # K-fold
ind.K <- rep(1:K, ceiling(n/K))[1:n]          # keep the same length as sample size
set.seed(12345)
ind.CV <- sample(ind.K, n)                    # get the index of CV randomly

# (2) define accuracy function
Acurr <- function(y, yfit) {mean(y==yfit)}

# (3) estimate model and calculate accuracy of classification
Acurr.in <- Acurr.out <- numeric(K)
SVM.model <- list()
for (k in 1:K){
  ind.i <- ind.samp[ind.CV==k]
  SVM.model[[k]] <- fit(y~., data=dat[-ind.i,], model='svm', task='class')
  y.train <- predict(SVM.model[[k]], dat[-ind.i,])
  y.test <- predict(SVM.model[[k]], dat[ind.i,])
  Acurr.in[k] <- Acurr(y=dat[-ind.i,'y'], yfit=y.train)
  Acurr.out[k] <- Acurr(y=dat[ind.i,'y'], yfit=y.test)
}

# (4) evaluation
Acurr <- round(cbind(Acurr.in, Acurr.out), digits=4)
rownames(Acurr) <- paste('k=', 1:K, sep='')
print(Acurr)
boxplot(Acurr)



################### for further use################################
# # (1) sample data
# library(Rcpp)
# library(RSNNS)            # for all kinds of neural network


# #将样本随机分成训练集与验证集
# set.seed(12345)
# ind.samp <- sample(1:nrow(dat),nrow(dat))
# data <- dat[ind.samp, ]
# dataValues <- data[,1:7]
# #将类别标签编制为适合网络的格式
# dataTargets <- decodeClassLabels(data[,8])
# data <- splitForTrainingAndTest(dataValues, dataTargets, ratio=0.30)
# #数据标准化处理
# data <- normTrainingAndTestSet(data)
# # (2) estimate model
# model.MLP <- mlp(data$inputsTrain, data$targetsTrain, size=5, learnFuncParams=0.1,
#                  maxit=50,inputsTest=data$inputsTest,targetsTest=data$targetsTest)
# 
# # (3) do classification
# class.MLP = predict(model.MLP, data$inputsTest)
# #预测结果展示
# confusionMatrix(data$targetsTest, class.MLP)

# #读入数据
# data=read.delim("clipboard")
# data <- dat
# data= data[sample(1:nrow(data),length(1:nrow(data))),1:ncol(data)]
# dataValues= data[,1:7]
# dataTargets = decodeClassLabels(data[,8])
# data=splitForTrainingAndTest(dataValues, dataTargets, ratio=0.30)
# data=normTrainingAndTestSet(data)
# #建立rbf神经网络，并且训练
# model2=rbf(data$inputsTrain,data$targetsTrain)
# #预测
# predictions2 = predict(model2,data$inputsTest)
# #结果展示
# confusionMatrix(data$targetsTest,predictions2)




# # (1) estimate model
# x <- as.matrix(dat[,1:7])
# y <- as.matrix(dat[,8])
# model.SVM <- svm(y~., data=dat, scale=TRUE, kernel='radial')
# 
# M <- crossvaldata(y~., dat, fit, predict, ngroup=5, model="svm", task='class')
# 
# # (2) show results
# y.hat <- predict(model.SVM, x)
# DAAG::confusion(round(y.hat), y)                           # calculate confusion matrix
# plot(performance(prediction(y.hat, y), "tpr","fpr"))       # plot ROC
# performance(prediction(y.hat, y), "auc")@y.values[[1]]     # accuracy
