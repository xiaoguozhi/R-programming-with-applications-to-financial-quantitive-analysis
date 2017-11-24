########################################################
# Description:
# 1.for Book 'R with applications to financial quantitive analysis'
# 2.Chapter: CH-01
# 3.Section: all
# 4.Purpose: sub function
# 5.Author: Qifa Xu
# 6.Founded: Aug 01, 2014.
# 7.Revised: Aug 01, 2014.
########################################################
# Contents:
# 1. define a function for descriptive statistics and normality test
#########################################################

# 1. define a function for descriptive statistics and normality test
stat <- function(x, plot.it=TRUE){
  if (!is.vector(x)) stop('argument x is not a vector, please check it.')
  if (!is.numeric(x)) stop('argument x is not a numeric, please check it.')
  
  browser()                                                        # set break point to debug the function
  mean <-  mean(x)
  median <- median(x)
  maximum <- max(x)
  minimum <- min(x)
  sd <- sd(x)
  skew <- moments::skewness(x)                                     # need package 'moments'
  kurt <- moments::kurtosis(x)                                     # need package 'moments'
  jbtst <- moments::jarque.test(x)                                 # need package 'moments'
  
  stats <- c(mean=mean, median=median, maximum=maximum, minimum=minimum, skew=skew, kurt=kurt)
  test <- c(JB=jbtst$statistic, p.value=jbtst$p.value)
  
  if (plot.it){
    par(mfrow=c(1,2))
    hist(x)
    qqnorm(x)
    qqline(x)
  }
  
  results<- list(stats=round(stats, digits=3), test=round(test, digits=3))   # set digits
  return(results)
}



