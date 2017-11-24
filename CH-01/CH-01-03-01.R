########################################################
# Description:
# 1.for Book 'R with applications to financial quantitive analysis'
# 2.Chapter: CH-01-03-01
# 3.Section: 1.3
# 4.Purpose: Common commands
# 5.Author: Jinxiu Zhang
# 6.Date: Feb 16, 2014.
# 7.Revised: Mar 02, 2014.
########################################################
# Contents:
# 1. Working directory and R memory
# 2. Save and load
# 3. Display commands
# 4. Link commands
#########################################################
# 0. Initializing
setwd('F:/programe/book/R with application to financial quantitive analysis/CH-01')
rm(list = ls())    

# 1. Working directory and R memory
# (1) Working directory
getwd()
setwd('F:/RCode/R-Book-fig')             # depend on your own directory
setwd('F:\\RCode\\R-Book')               # depend on your own directory

# (2) R memory
memory.size(NA)
memory.limit()
memory.size(FALSE)
memory.size(TRUE)
memory.size(4095)

# 2. Save and load
# (1) package
library(qrnn)
search()

# (2) object
save.image()
save(x,y,file="F:/Rcode/S.RData")
load(file="F:/Rcode/S.RData")

# (3) picture
jpeg(file="Saveplot1.jpeg")
plot(iris)
dev.off()

postscript(file="Saveplot2.eps")
plot(iris)
dev.off()

# 3. Display commands
data(iris)
head(iris)
tail(iris)

library(car)
some(iris)

D <- seq(1:3)
D
print(D)
(D <- seq(1:3))

E1 <-3.1415926535
E2 <-3.1415926535
(E1.r <- round(E1,digits=4))
E1.r*1000000
options(digits=4);E2
E2*1000000

# 4. Link commands
df <- data.frame(name=c("ZhangSan","XiaoHong","LiSi","XiaoLan"),
                 sex=c("M","F","M","F"),age =c(20,21,19,20),weight=c(110,90,128,102))
attach(df)
mean(age)

detach(df)

library(quantreg)
detach("package:quantreg")






