########################################################
# Description:
# 1.for Book 'R with applications to financial quantitive analysis'
# 2.Chapter: CH-01-02-02
# 3.Section: 1.2
# 4.Purpose: Common commands
# 5.Author: Jinxiu Zhang
# 6.Date: Feb 15, 2014.
# 7.Revised: Mar 02, 2014.
########################################################
# Contents:
# 1. Type conversion
# 2. The operator
# 3. Operation function
# 4. Family of apply functions
#########################################################
# 0. Initializing
setwd('F:/programe/book/R with application to financial quantitive analysis/CH-01')
rm(list = ls())  

# 1. Type conversion
M <- matrix(1:12,nrow=3,ncol=4)
class(M)
is.matrix(M)

M.vec <- as.vector(M)
M.vec

M.frame <- as.data.frame(M)
M.frame

ff1 <- factor(c(4,8))
ff1
as.numeric(ff1)
as.numeric(as.character(ff1))

ff2 <- factor(c("Shim","Taylor","Engel"))
ff2
as.numeric(ff2)

methods(as)
methods(is)

# 2. The operator
# (1) Mathematical operations
x <- c(3,4)%%10
x
y <- matrix(c(1:9),ncol=3)
z <- matrix(c(2:10),ncol=3)
z[lower.tri(z)] <- 0
y*z
y%*%z

# (2) Comparison operations
M <- c("A","B")
m <- c("a","b")
M==m
identical(M,m)
all.equal(M,m)

identical(y,z)
all.equal(y,z)

# 3. Operation function
# (1) Vector operations function
z <- c(1,5,4,7,9)
sum(z)
mean(z)
sd(z)
median(z)
sort(z)

append(z,2:3,after=2)

# (2) Matrix operations function
A <- matrix(1:9,ncol=3)
A
t(A)
aperm(A,c(2,1))

dim(A)
nrow(A)
ncol(A)

B <- matrix(1:8,ncol=4);B
C <- matrix(8:1,ncol=4);C
rbind(B,C)
cbind(B,C)

# 4. Family of apply functions
# (1)apply function
x <- runif(10,-1,1)
y <- rnorm(10,0.5,1)
xy <- cbind(x,y)
apply(xy,1,sum)
apply(xy,2,mean)

# (2)tapply function
t1 <- factor(rep(1:4,length=14),levels=1:5,labels=c("A","B","C","D","E"))
t2 <- c(1:14)
tapply(t2,t1,sum)
tapply(t2,t1,sum,simplify=FALSE)

# (3)lapply function
L1 <- list(a=1:20,b=runif(30,-2,5),d=matrix(c(1:10)))
lapply(L1,quantile)

# (4)sapply function
sapply(L1,quantile,simplify=FALSE,use.names=FALSE)
sapply(L1,quantile,simplify=TRUE,use.names=FALSE)
