########################################################
# Description:
# 1.for Book 'R with applications to financial quantitive analysis'
# 2.Chapter: CH-01-02-01
# 3.Section: 1.2
# 4.Purpose: Data Operation
# 5.Author: Jinxiu Zhang
# 6.Date: Feb 10, 2014.
# 7.Revised: Mar 02, 2014.
########################################################
# Contents:
# 1. Object
# 2. Basic types
# 3. Vector
# 4. Array and Matrix
# 5. List and Data.frame
# 6. Factor
# 7. Expression
#########################################################
# 0. Initializing
setwd('F:/programe/book/R with application to financial quantitive analysis/CH-01')                                    
rm(list = ls())                   

# 1. Object 
ls()
x <- 8
rm(x)
rm(list=ls())

# 2. Basic types
# (1) numeric 
x <- 12
length(x)
class(x)

# (2) logical 
y <- TRUE
class(y)
length(y)

# (3) character
z <- "Display \"a\" string "
w <- 'Display "b"  string '
cat(z)
cat(w)
class(z);class(w)

# (4) complex
m <- 2+3i
class(m)
length(m)

# 3. Vector
x <- c(1,3,5,7)
x
x[2]

y <- 1:10
y
1:10-1
1:(10-1)

seq(1,10,3)
seq(1,10)
seq(10,1,-2)
seq(1,by=3,length=4)

rep(1,3)
rep(c(2,4),3)
rep(c(2,4),each=3)
rep(2:4,rep(2,3))

# 4. Array and Matrix
# (1) Array 
X <- array(1:30,dim=c(5,6))
X

Z <- array(1:32,dim=c(4,4,2))
Z

Z[2,3,2]
Z[1:3,2,1]
Z[,,2]
dim(Z)

w <- 1:12
class(w)

dim(w) <- c(2,3,2)
w
class(w)

# (2) Matrix 
matrix(seq(1:20),nrow=5,ncol=4)
M <-matrix(seq(1:20),nrow=5,ncol=4,byrow=TRUE,
           dimnames=list(c("r1","r2","r3","r4","r5"),c("c1","c2","c3","c4")))
M
M[2,3]
M[1:3,3]
M[1:3,3,drop=FALSE]
M[-1,]
M[1:3,-c(1,3)]

diag(M)
diag(1:3)
diag(3)

Tri <- matrix(1:9,3,3)
Tri[upper.tri(Tri)] <- 0
Tri

# 5. List and Data.frame
# (1) List
Stu.Lt <- list(name="ZhangSan",stu.no="20140224",age=21,grade=c(90,85,96))
Stu.Lt 

Stu.Lt[[2]]
class(Stu.Lt[[2]])

Stu.Lt[2]
class(Stu.Lt[2])

Stu.Lt[1:2]

Stu.Lt[[4]][1]
Stu.Lt[[4]][1:2]

Stu.Lt["name"]
class(Stu.Lt["name"])

Stu.Lt$stu.no
Stu.Lt$grade

names(Stu.Lt)

Stu.Lt$Project <- c("programming","sport")
Stu.Lt$stu.no <- NULL
Stu.Lt$name <- "LiSi"  
Stu.Lt

unlist(Stu.Lt)

# (2) Data.frame
df <- data.frame(name=c("ZhangSan","XiaoHong","LiSi","XiaoLan"),
                 sex=c("M","F","M","F"),age =c(20,21,19,20),weight=c(110,90,128,102))
df

rownames(df) <- c("one","two","three","four")
df

df[1:2,3:4]

df[["age"]]
df$weight

# 6. Factor
factor(1:4)
factor(1:4,levels=1:2)
factor(c((1:4),(4:1)),labels=c("A","B","C","D"))
factor(1:4,exclude=2)
sex <- c("M","F","M","F")
sexf <- factor(sex)
sexf

levels(sexf)
table(sexf)

# 7. Expression
e1 <- 12; e2 <- 3.5
f <- expression(sin(e1)+e2^2)
f
eval(f)
D(f, "e1")
D(f, "e2")

set.seed(12345)
p1 <- rnorm(100)
p2 <- runif(100)
model.lm <- lm(p2~p1+I(p1^2))
summary(model.lm)








