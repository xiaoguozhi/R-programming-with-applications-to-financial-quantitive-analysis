########################################################
# Description:
# 1.for Book 'R with applications to financial quantitive analysis'
# 2.Chapter: CH-06-01-01
# 3.Section: 6.1
# 4.Purpose: GARCH modeling
# 5.Author: Qifa Xu
# 6.Founded: Dec 09, 2013.
# 7.Revised: Aug 06, 2014.
########################################################
# Contents:
# 1. Load package
# 2. Simulate a univariate ARCH time series model
# 3. Simulate a univariate GARCH time series model
#########################################################

# 0. 初始化
setwd('F:/programe/book/R with application to financial quantitive analysis/CH-06')
rm(list=ls())

# 1. 加载包
library(fGarch)

# 2. 模拟一元ARCH时间序列模型
# (1) simulate ARCH(1) model
set.seed(12345)
spec_1 <- garchSpec(model=list(omega=0.01, alpha=0.85, beta=0))
simdata_1 <- garchSim(spec_1, n=200, extended=TRUE)
class(simdata_1)
plot(simdata_1)
par(mfrow=c(1,3))
acf(simdata_1$eps, main='(a) 残差序列', xlab='滞后期')
acf(simdata_1$garch, main='(b) 模拟数据', xlab='滞后期')
acf(simdata_1$garch^2, main='(c) 模拟数据平方', xlab='滞后期')
par(mfrow=c(1,1))

# (2) ARCH effect test
library(FinTS)
ArchTest(x=simdata_1$garch, lags=12)
ArchTest(x=simdata_1$eps, lags=12)

# 3. 模拟一元GARCH时间序列模型
# (1) simulate GARCH(1,1) model
set.seed(12345)
spec_2 <- garchSpec(model=list(omega=0.01, alpha=0.85, beta=0.1))
simdata_2 <- garchSim(spec_2, n=200, extended=TRUE)
class(simdata_2)
par(mfrow=c(1,3))
plot(c(simdata_2$eps), type='l', xlab='天', ylab='', main='(a) 残差序列')
plot(c(simdata_2$garch), type='l', xlab='天', ylab='', main='(b) 模拟数据')
plot(c(simdata_2$sigma), type='l', xlab='天', ylab='', main='(c) 条件标准差')
par(mfrow=c(1,1))

par(mfrow=c(1,3))
acf(simdata_2$eps, xlab='滞后期', main='(a) 残差序列')
acf(simdata_2$garch, xlab='滞后期', main='(b) 模拟数据')
acf(simdata_2$garch^2, xlab='滞后期', main='(c) 模拟数据平方')
par(mfrow=c(1,1))

# 4. 例6-4：案例分析-GARCH模型
# (1) read data
library(quantmod)                                           # 加载包
getSymbols('^HSI', from='1989-12-01', to='2013-11-30')      # 从Yahoo网站下载恒生指数日价格数据
dim(HSI)                                                    # 数据规模
names(HSI)                                                  # 数据变量名称
chartSeries(HSI, theme='white')                             # 画出价格与交易的时序图

HSI <- read.table('HSI.txt')                                # 或者从硬盘中读取恒生指数日价格数据
HSI <- as.xts(HSI)                                          # 将数据格式转化为xts格式

# (2) compute return series
ptd.HSI <- HSI$HSI.Adjusted                                 # 提取日收盘价信息
rtd.HSI <- diff(log(ptd.HSI))*100                           # 计算日对数收益
rtd.HSI <- rtd.HSI[-1,]                                     # 删除一期缺失值
plot(rtd.HSI)                                               # 画出日收益序列的时序图

ptm.HSI <- to.monthly(HSI)$HSI.Adjusted                     # 提取月收盘价信息
rtm.HSI <- diff(log(ptm.HSI))*100                           # 计算月对数收益
rtm.HSI <- rtm.HSI[-1,]                                     # 删除一期缺失值
plot(rtm.HSI)                                               # 画出月收益序列的时序图
detach(package:quantmod)

# (3) ARCH效应检验
# rtm.HSI <- as.numeric(rtm.HSI)
ind.outsample <- sub(' ','',substr(index(rtm.HSI), 4, 8)) %in% '2013'   # 设置样本外下标：2013年为样本外
ind.insample <- !ind.outsample                                          # 设置样本内下标：其余为样本内
rtm.insample <- rtm.HSI[ind.insample]
rtm.outsample <- rtm.HSI[ind.outsample]
Box.test(rtm.insample, lag=12, type='Ljung-Box')                        # 月收益序列不存在自相关
Box.test(rtm.insample^2, lag=12, type='Ljung-Box')                      # 平方月收益序列存在自相关

FinTS::ArchTest(x=rtm.insample, lags=12)                                # 存在显著的ARCH效应

# (4) 模型定阶
epst <- rtm.insample - mean(rtm.insample)                               # 均值调整对数收益
par(mfrow=c(1,2))
acf(as.numeric(epst)^2, lag.max=20, main='平方序列')
pacf(as.numeric(epst)^2, lag.max=20, main='平方序列')                               

# (5) 建立GARCH类模型
GARCH.model_1 <- garchFit(~garch(1,1), data=rtm.insample, trace=FALSE)                    # GARCH(1,1)-N模型
GARCH.model_2 <- garchFit(~garch(2,1), data=rtm.insample, trace=FALSE)                    # GARCH(1,2)-N模型
GARCH.model_3 <- garchFit(~garch(1,1), data=rtm.insample, cond.dist='std', trace=FALSE)   # GARCH(1,1)-t模型
GARCH.model_4 <- garchFit(~garch(1,1), data=rtm.insample, cond.dist='sstd', trace=FALSE)  # GARCH(1,1)-st模型
GARCH.model_5 <- garchFit(~garch(1,1), data=rtm.insample, cond.dist='ged', trace=FALSE)   # GARCH(1,1)-GED模型
GARCH.model_6 <- garchFit(~garch(1,1), data=rtm.insample, cond.dist='sged', trace=FALSE)  # GARCH(1,1)-SGED模型

summary(GARCH.model_1)
summary(GARCH.model_3)

plot(GARCH.model_1)                                                                       # 请键入相应数字获取信息

# (6) 提取GARCH类模型信息
vol_1 <- fBasics::volatility(GARCH.model_1)                   # 提取GARCH(1,1)-N模型得到的波动率估计
sres_1 <- residuals(GARCH.model_1, standardize=TRUE)          # 提取GARCH(1,1)-N模型得到的标准化残差
vol_1.ts <- ts(vol_1, frequency=12, start=c(1990, 1))
sres_1.ts <- ts(sres_1, frequency=12, start=c(1990, 1))
par(mfcol=c(2,1))
plot(vol_1.ts, xlab='年', ylab='波动率')
plot(sres_1.ts, xlab='年', ylab='标准化残差')

# (7) 模型检验
par(mfrow=c(2,2))
acf(sres_1, lag=24)
pacf(sres_1, lag=24)
acf(sres_1^2, lag=24)
pacf(sres_1^2, lag=24)

par(mfrow=c(1,1))
qqnorm(sres_1)
qqline(sres_1)

# (8) 模型预测
pred.model_1 <- predict(GARCH.model_1, n.ahead = 11, trace = FALSE, mse = 'cond', plot=FALSE)
pred.model_2 <- predict(GARCH.model_2, n.ahead = 11, trace = FALSE, mse = 'cond', plot=FALSE)
pred.model_3 <- predict(GARCH.model_3, n.ahead = 11, trace = FALSE, mse = 'cond', plot=FALSE)
pred.model_4 <- predict(GARCH.model_4, n.ahead = 11, trace = FALSE, mse = 'cond', plot=FALSE)
pred.model_5 <- predict(GARCH.model_5, n.ahead = 11, trace = FALSE, mse = 'cond', plot=FALSE)
pred.model_6 <- predict(GARCH.model_6, n.ahead = 11, trace = FALSE, mse = 'cond', plot=FALSE)

predVol_1 <- pred.model_1$standardDeviation
predVol_2 <- pred.model_2$standardDeviation
predVol_3 <- pred.model_3$standardDeviation
predVol_4 <- pred.model_4$standardDeviation
predVol_5 <- pred.model_5$standardDeviation
predVol_6 <- pred.model_6$standardDeviation
et <- abs(rtm.outsample - mean(rtm.outsample))
rtd.HSI.2013 <- rtd.HSI['2013']
rv <- sqrt(aggregate(rtd.HSI.2013^2, by=substr(index(rtd.HSI.2013), 1, 7), sum))

predVol <- round(rbind(predVol_1,predVol_2,predVol_3,predVol_4,predVol_5,predVol_6, 
                       as.numeric(et), as.numeric(rv)), digits=3)
colnames(predVol) <- 1:11
rownames(predVol) <- c('GARCH(1,1)-N模型','GARCH(1,2)-N模型','GARCH(1,1)-t模型','GARCH(1,1)-st模型',
                       'GARCH(1,1)-GED模型','GARCH(1,1)-SGED模型','残差绝对值', '已实现波动')
print(predVol)

# (9) 模型选择
cor(t(predVol))

# 5. 例6-5：案例分析, GARCH-M模型、TGARCH模型与APARCH模型
library(rugarch)
# (1) GARCH-M模型
GARCHM.spec <- ugarchspec(variance.model=list(model='fGARCH', garchOrder=c(1,1), submodel='GARCH'), 
                          mean.model=list(armaOrder=c(0,0), include.mean=TRUE, archm=TRUE),
                          distribution.model='norm')
GARCHM.fit <- ugarchfit(GARCHM.spec, data=rtm.insample)

# (2) TGARCH模型
TGARCH.spec <- ugarchspec(variance.model=list(model='fGARCH', garchOrder=c(1,1), submodel='TGARCH'), 
                          mean.model=list(armaOrder=c(0,0), include.mean=TRUE, archm=FALSE),
                          distribution.model='norm')
TGARCH.fit <- ugarchfit(TGARCH.spec, data=rtm.insample)

# (3) APARCH模型
APARCH.model_1 <- garchFit(~1+aparch(1,1), data=rtm.insample, trace=FALSE)                    # GARCH(1,1)-N模型
summary(APARCH.model_1)
APARCH.model_2 <- garchFit(~1+aparch(1,1), data=rtm.insample, delta=2, trace=FALSE)                    # GARCH(1,1)-N模型
summary(APARCH.model_2)

# 6. 例6-6：案例分析, MGARCH模型
library(rmgarch)
library(xts)
library(xlsx)

# (1) CCC-GARCH




# (2) DCC-GARCH模型
StockExchange <- read.xlsx(file='Data.xlsx', sheetIndex='StockExchange')
rstock <- diff(log(StockExchange$stock))            # 计算对数收益率
rexchange <- diff(log(StockExchange$exchange))      # 计算对数收益率
r.StockExchange <- data.frame(rstock=rstock, rexchange=rexchange, row.names=StockExchange$date[-1])
r.StockExchange <- as.xts(r.StockExchange)          # 转化为时间序列结构
par(mfrow=c(2,1))                                   # 时间序列图形
plot(r.StockExchange$rstock, ylab='收益率',main='沪深300',lty=1,cex.lab=0.8,cex.main=0.8)
plot(r.StockExchange$rexchange, ylab='收益率',main='汇率',lty=1,cex.lab=0.8,cex.main=0.8)

garch11.spec <- ugarchspec(mean.model = list(armaOrder = c(1,1),include.mean =TRUE),
                           variance.model = list(garchOrder = c(1,1),model = "eGARCH"),
                           distribution.model = "sstd")        # 设定GARCH和DCC过程
dcc.garch11.spec <- dccspec(uspec = multispec(replicate(2,garch11.spec)),VAR=FALSE,dccOrder = c(1,1),distribution="mvt")
dcc.fit = dccfit(dcc.garch11.spec, data=r.StockExchange)      # DCC估计
dcc.fit                                                       # 显示DCC估计结果

plot(dcc.fit)                                                 # DCC结果输出，按照要求键入一个数字（1-5）
dcc.fcst = dccforecast(dcc.fit, n.ahead=20)                   # 外推预测
plot(dcc.fcst)                                                # 显示外推预测，按照要求键入一个数字（1-5）



