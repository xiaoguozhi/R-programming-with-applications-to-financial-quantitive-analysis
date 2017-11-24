########################################################
# Description:
# 1.for Book 'R with applications to financial quantitive analysis'
# 2.Chapter: CH-01-04-01
# 3.Section: 1.4
# 4.Purpose: Drawing production
# 5.Author: Jinxiu Zhang
# 6.Date: Dec 27, 2013.
# 7.Revised: Jan 20, 2014.
########################################################
# Contents:
# 1. plot function demo
# 2. pairs and coplot function demo
# 3. Graphics case for The Shanghai composite index (SSEC)
#########################################################
# 0. Initializing
setwd('F:/programe/book/R with application to financial quantitive analysis/CH-01')
rm(list = ls())                   

# 1. plot function demo
set.seed(12345)                  
x <- sample(c(1:100),100) 
y <- sample(c(1:100),100) 
xt <- ts(x) 
xy <- cbind(x, y) 
f <- as.factor(c(rep('A',20),rep('B',30),rep('C',50)))
par(mfrow=c(2,3),mar=c(5,4,3,2))                                       # The graphic panel structure
plot(x)
plot(xt)
plot(xy)
plot(x,y)
plot(f)
plot(f,y)   

# 2. pairs and coplot function demo
data(morley)
pairs(morley)
coplot(uptake ~ conc | Plant, data = CO2,show.given = FALSE, type = "b")


# 3. Graphics case
# (1) reading SSEC index 
library(RODBC)                                                          # loading package
SSEC_Data <- odbcConnectExcel('CH-01-05.xls')                           # reading excel data
SSEC <-sqlFetch(SSEC_Data, 'SSEC')                                      # Extracting the data from SSEC workbook  
SSEC_shjc <- sqlFetch(SSEC_Data, 'SSEC-600009')                         # Extracting the data from SSEC-600009 workbook   
SSEC_scgf <- sqlFetch(SSEC_Data, 'SSEC-600008')                         # Extracting the data from SSEC-600008 workbook   
SSEC_zggm <- sqlFetch(SSEC_Data, 'SSEC-600007')                         # Extracting the data from SSEC-600007 workbook   
close(SSEC_Data)
detach(package:RODBC)

# (2) compute return series
Close.ptd.SSEC <- SSEC$SSEC_Close                                        # Extract the day the closing price information
Close.rtd.SSEC <- diff(log(Close.ptd.SSEC))*100                          # Calculation daily log returns

# (3) Close price of Sample stocks in SSEC  
Close.ptd.shjc <- SSEC_shjc$SSEC_600009_Close                       
Close.ptd.scgf <- SSEC_scgf$SSEC_600008_Close  
Close.ptd.zggm <- SSEC_zggm$SSEC_600007_Close

# (4) Drawing SSEC Return Series, Scatter, ACF and PACF Figure
# win.graph(width=7,height=6.5)                                          # The size of the Settings window
par(mfrow=c(2,2),mar=c(5,4,3,2))                                       # The graphic panel structure

Close.ptd.SSEC.ts<-ts(Close.ptd.SSEC,start=c(2006),freq=241)
plot(Close.ptd.SSEC.ts, type="l",main="(a) 上证指数日收盘价序列图",xlab="Date",ylab="Price",cex.main=0.95,las=1)     

plot(Close.ptd.shjc[1:20], type="p",pch=17,main="(b) 上证指数样本股散点图",
     xlab="Time",ylab="Price",cex.main=0.95,ylim=c(4,14),las=1)      # Drawing the scatter diagram of 20 Close Price  
points(Close.ptd.scgf[1:20],pch=15)                           
points(Close.ptd.zggm[1:20],pch=14)                           
legend("bottomright", legend=c("SHJC_600009","SCGF_600008","ZGGM_600007"),
       pch=c(17,15,14),cex=0.7,lty=c(-1,-1,-1))     

acf(Close.rtd.SSEC,main='',xlab='Lag',ylab='ACF',las=1)                 # The Autocorrelation  Test
title(main='(c) 上证指数收益率自相关检验',cex.main=0.95)

pacf(Close.rtd.SSEC,main='',xlab='Lag',ylab='PACF',las=1)               # The Partial autocorrelation Test
title(main='(d) 上证指数收益率偏自相关检验',cex.main=0.95)

# (5) Drawing  Q-Q, ecdf, density and hist figure 
# win.graph(width=7,height=6.5)                                           # The size of the Settings window
par(mfrow=c(2,2),mar=c(5,4,3,2)) 

qqnorm(Close.rtd.SSEC,main="(a) 上证指数收益率Q-Q图 ",cex.main=0.95,xlab='理论分位数',ylab='样本分位数')      # QQ picture of test of normality
qqline(Close.rtd.SSEC) 

ECD.SSEC <- ecdf(Close.rtd.SSEC[1:10])
plot(ECD.SSEC,lwd = 2,main="(b) 上证指数累积分布函数图",cex.main=0.95,las=1)  # Drawing empirical distributions of return 10 sample size
xx <- unique(sort(c(seq(-3, 2, length=24), knots(ECD.SSEC))))           # Finding knots 
lines(xx, ECD.SSEC (xx))                                                # Drawing lines with knot
abline(v=knots(ECD.SSEC),lty=2,col='gray70')                            # Drawing lines of perpendicular to the x axis
x1 <- c((-4):3)
lines(x1,pnorm(x1,mean(Close.rtd.SSEC[1:10]),sd(Close.rtd.SSEC[1:10])))   # Drawing the normal distribution curve

D <-density(Close.rtd.SSEC)
plot(D, main="(c) 上证指数核密度曲线图 ",xlab="收益", ylab='密度',
     xlim = c(-7,7), ylim=c(0,0.5),cex.main=0.95)                       # Drawing kernel density curve             
polygon(D, col="gray", border="black")                                  # Filling kernel density curve of closed section
curve(dnorm,lty = 2, add = TRUE)                                        # Drawing normal density curve
abline(v=0,lty = 3)                                                     # Drawing lines of perpendicular to the x=0 axis
legend("topright", legend=c("核密度","正态密度"),lty=c(1,2,3),cex=0.7)

hist(Close.rtd.SSEC[1:100],xaxt='n',main='(d) 上证指数收益率直方图',
     xlab='收益/100',ylab='密度', freq=F,cex.main=0.95,las=1)        # Drawing histogram of return 100 sample size and not displaying x axis labels 
x2 <- c(-6:4)
lines(x2,dnorm(x2,mean(Close.rtd.SSEC[1:100]),sd(Close.rtd.SSEC[1:100]))) # Drawing normal probability density curve
axis(1,at=axTicks(1),labels=as.integer(axTicks(1))/100)                   # Adding x axis labels and displaying smaller 100 times


