#####################
# load libraries
# set wd
# clear global .envir
#####################

# remove objects
rm(list=ls())
# detach all libraries
detachAllPackages <- function() {
  basic.packages <- c("package:stats", "package:graphics", "package:grDevices", "package:utils", "package:datasets", "package:methods", "package:base")
  package.list <- search()[ifelse(unlist(gregexpr("package:", search()))==1, TRUE, FALSE)]
  package.list <- setdiff(package.list, basic.packages)
  if (length(package.list)>0)  for (package in package.list) detach(package,  character.only=TRUE)
}
detachAllPackages()

# load libraries
pkgTest <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[,  "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg,  dependencies = TRUE)
  sapply(pkg,  require,  character.only = TRUE)
}
# install.packages("dgof")
# library(dgof)
# install.packages("codadiags")
# require(codadiags)
install.packages("CPAT")
# library(CPAT)

# here is where you load any necessary packages
# ex: stringr
# lapply(c("stringr"),  pkgTest)

lapply(c("CPAT"),  pkgTest)

# set wd for current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


#####################
# Problem 1
#####################

#by hand steps 
# 1. define a cdf for the theoretical distribution i.e. pnorm/cdf(x)
# 2. define a empirical distribution function for observed data i.e. empiricalCDF/edf(x)
# 3. create an empty object for the maximum distance between values of edf(x) and cdf(x). 
# 4. step through the cdf to check if the distance between value of edf(x) and of cdf(x) 
# is greater than that for the previous corresponding value of x. if so replace max dist 
# value with new distance
# 5. find maximum [done with hint, no need to step through]
# 6. identify critcal value for k-s distribution / cdf either by comparing to tables, or 
# by simulating the sampling distribution for D given the null hypothesis through generating 
# a large number of samples and finding the D for each https://blogs.sas.com/content/iml/2019/05/20/critical-values-kolmogorov-test.html
# 7. compare the D statistic to the critical value 
# OR calculate p-value via https://specbranch.com/posts/kolmogorov-smirnov/ equation

# I know what I have is wrong, this is as far as I got. What I couldn't find was a 
# way to simulate a sampling distribution for D, or alternatively to call on an existing KS distribution 
# (which I believe based on my research perhaps doesn't exist)

set.seed(123)
# c(alpha)=sqrt(-ln(alpha/2)*(1/2)) # https://www.aatbio.com/tools/kolmogorov-smirnov-k-s-test-calculator
crit.val.05 <- sqrt(-(log(0.05/2))*(1/2)) # for alpha = 0.05 

ks.norm.test <- function(dat){
  ECDF <- ecdf(dat) # create function for a value of the data
  empiricalCDF <- ECDF(dat) 
  
  D <- max(abs(empiricalCDF - pnorm(dat)))
  ## p <- CPAT:::pkolmogorov(D)
  
  if (D>crit.val.05)(
    print("Test Statistic D is greater than the critical value for which alpha = 0.05")
  )
  else (
    print("Test Statistic D is not greater than the critical value for which alpha = 0.05")
  )

  print(list(D))
}

data.test <- rcauchy(1000, location = 0, scale = 1)
ks.norm.test(data.test)
# D not greater, value for D = 0.1347281
# very different from ks.test() result 

ks.test(data.test, pnorm(data.test)) # D = 0.493, p < alpha

# ----------

# ECDF.test <- ecdf(data.test) # create function for a value of the data
# empiricalCDF.test <- ECDF.test(data.test) 

#n <- length(data.test)

#sample_Ds = rep(NA, n)

#fill empty vector with ts
# for(i in 1:n){
 # sample_Ds[i] = D(rnorm(20, mean=5.3, sd=9))
#}

# D.test <- max(abs(empiricalCDF.test - pnorm(data.test)))
# D.test
# p.test <- ecdf(D.test)
# p.test
#p.test <- CPAT:::pkolmogorov(D)

# print(list(D.test, p.test))
# ksecdf <- ecdf(D.test)


#hint 
# create empirical distribution of observed data
# ECDF <- ecdf(data) # create function for a value of the data
# empiricalCDF <- ECDF(data) # evaluates for every value of the data 
# generate test statistic
# D <- max(abs(empiricalCDF - pnorm(data)))

#####################
# Problem 2
#####################

set.seed(123)
data <- data.frame(x = runif(200, 1, 10)) 
data$y <- 0 + 2.75*data$x + rnorm(200, 0, 1.5)

# combining own sources with tutorial and lecture code
#x <- data$x
# fun2 <- function(b){
  # y_hat <-  (b[1]*x)
 # r = y_hat - b[1]*x
  # -sum(dnorm(r, mean = 0, y_hat, log=TRUE)) 
#}
# mle_est2 <- optim(fn = fun2, 
                #   par = 0:3,
                 #  hessian = T,
                #   method = "BFGS"
# )
# mle_est2$par

## answer
# derived from https://www.joshua-entrop.com/post/optim_linear_reg/ 
# y = interc + betai*xi + sigma / error 

fun <- function(par, yi, xi){
  # y_hat <-  (2.75*xi)
  
  alpha <- par[1] # assign parameters indices
  betai <- par[2]
  
  
  R = yi - alpha - (betai*xi) # residuals equation
  
  -sum(dnorm(R, mean = 0, sd(R), log=TRUE)) 
}


# estimate based on conditional parameters 
est_alpha <- mean(data$y)
est_betai <- mean(data$y[data$y >= 15.48787]) -  mean(data$y[data$y < 15.48787]) #median
#est_sigma <- sd(data$y)

mle_est <- optim(fn = fun, 
                 par = c(alpha = est_alpha, 
                         betai = est_betai),
                 yi = data$y,
                 xi = data$x,
                 hessian = T,
                 method = "BFGS"
                 )
mle_est$par
#alpha     betai 
#0.1391961 2.7266971 

lmm <- lm(data$y~data$x)
summary(lmm)
stargazer::stargazer(lmm, type = "latex") 




