setwd('F:/programe/package/R.matlab')
rm(list=ls())

# 1. Load package
library(R.matlab)

# 2. Start matlab
# (1) start matlab from R
Matlab$startServer()

# (2) start matlab externally
print(system.file('externals', package='R.matlab'))

# 3. Create a matlab client object used to communicate with matlab
matlab <- Matlab()
print(matlab)
isOpen <- open(matlab)
print(isOpen)
print(matlab)

# 4.  Example: lasso regression
# (1) solution 1
evaluate(matlab, 'X = randn(100,5)')
evaluate(matlab, 'r = [0;2;0;-3;0]')
evaluate(matlab, 'Y = X*r + randn(100,1)*.1')
data <- getVariable(matlab, c('X','r','Y'))

writeMat('ls1.mat', X=data$X, Y=data$Y)
evaluate(matlab, 'load ls1.mat;')

evaluate(matlab, '[B, FitInfo] = lasso(X, Y)')
result <- getVariable(matlab, c('B', 'FitInfo'))
print(result)

# (2) solution 2
X <- matrix(rnorm(500), nc=5)
r <- c(0,2,0,-3,0)
Y <- X%*%r + 0.1*rnorm(100)

setVariable(matlab, X=X)
setVariable(matlab, Y=Y)
evaluate(matlab, '[B, FitInfo] = lasso(X, Y)')
result <- getVariable(matlab, c('B', 'FitInfo'))
print(result)

# 5.  Done: close the matlab clinet
close(matlab)
print(matlab)
