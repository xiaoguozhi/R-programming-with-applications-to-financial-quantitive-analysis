########################################################
# Description:
# 1.for Book 'R with applications to financial quantitive analysis'
# 2.Chapter: CH-12-04
# 3.Section: 12.4
# 4.Purpose: 
# 5.Author: Changjiu Wang, polished by Qifa Xu
# 6.Date: Apr 03, 2014.
# 7.Revised: Sep 01, 2014.
########################################################
# Contents:
# 1. define a function
# 2. implement option pricing
#############################################################
# 0. initializing
# (1) set path
setwd('F:/programe/book/R with application to financial quantitive analysis/CH-12')
rm(list=ls())

# (2) load packages
library(rmr2)                             #载入rmr2程序包
#载入需要的程辑包：Rcpp
#载入需要的程辑包：RJSONIO
#载入需要的程辑包：bitops
#载入需要的程辑包：digest
#载入需要的程辑包：functional
#载入需要的程辑包：reshape2
#载入需要的程辑包：stringr
#载入需要的程辑包：plyr
#载入需要的程辑包：caTools


# 1. define a function
#主函数：运用蒙特卡洛方法计算欧式期权价格，EurOptionPr为函数名，该函数有7个输入参数S0、K、T、r、sigma、M、N
#S0：当前标的资产价格
#K：期权合约的执行价格
#T：期权合约剩余有效期
#r：无风险利率
#sigma：年化波动率
#M：模拟资产价格路径的数目
#N：模拟的时间节点的数目
EurOptionPr <- function(S0,K,T,r,sigma,M,N){
  inp<-cbind(S0*rep(1,M),rep(0,M))
  inp<-to.dfs(inp)
  buildTraj<-function(k,v){
    deltaT<-T/N
    for(i in 1:N){
      dW<-sqrt(deltaT)*rnorm(length(v[,1]))
      v[,2]<-v[,1]+r*v[,1]*deltaT+sigma*v[,1]*dW
      v[,1]<-v[,2]
    }
    key<-ifelse(v[,1]-K>0,"call","put")
    value<-ifelse(v[,1]-K>0,exp(-r*T)*(v[,1]-K),exp(-r*T)*(K-v[,1]))
    keyval(key,value)
  }
  price.reduce.fn<-function(k,v){
    keyval(k,mean(v)*(length(v)/M))
  }
  OptionPr<-mapreduce(input=inp,map=buildTraj,reduce=price.reduce.fn)
  print(from.dfs(OptionPr))
}

# 2. implement option pricing
#调用主函数：此处假设S0=50，K=52，T=5/12，r=0.1，sigma=0.4，M=1000，N=100
EurOptionPr(50,52,5/12,0.1,0.4,1000,100)

