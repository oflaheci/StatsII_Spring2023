#####################
# load libraries
# set wd
# setwd(getwd())
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

# here is where you load any necessary packages
# ex: stringr
# lapply(c("stringr"),  pkgTest)

lapply(c(),  pkgTest)

# set wd for current folder
# setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd("/Users/ciaraoflaherty/StatsII_Spring2023/problemSets/PS02/template")

#####################
# Problem 1
#####################

# load data
load(url("https://github.com/ASDS-TCD/StatsII_Spring2023/blob/main/datasets/climateSupport.RData?raw=true"))
dat <- climateSupport
head(dat)
summary(dat)
unique(dat$choice)

# clean data 
# change to boolean 
dat$choice <- sub("Not supported",0, dat$choice)
dat$choice <- sub("Supported", 1, dat$choice)
unique(dat$choice)
dat$choice <- as.logical(as.numeric(dat$choice))
typeof(dat$choice)

unique(dat$sanctions)
dat$sanctions <- sub("None", 0, dat$sanctions)
dat$sanctions <- sub("%","", dat$sanctions)
dat$sanctions <- as.factor(dat$sanctions)

unique(dat$countries)
dat$countries <- sub(" of 192", "", dat$countries)
dat$countries <- as.factor(dat$countries)

# part 1 - additive model 
modl <- glm(choice ~ countries + sanctions, data = dat,
            family = "binomial")
predict(modl, dat)
stargazer::stargazer(modl, type = "text")
summary(modl)

nmodl <- glm(choice ~ 1, data = dat, family = binomial(link = "logit"))
test2 <- anova(nmodl, modl, test = "LRT")
summary(test2)
test2 # reject the null 

# modl:
# Y_hat = Intercept + Beta1aX1 + Beta1bX1 +Beta1cX1 + Beta2aX2 + Beta2bX2 + Beta2cX2
# yhat = Intercept + (countries20 + countries80 + countries160)X1 _ (sanctions15 + sanctions20 + sanctions5)X2
# Y_hat = 0.376 + -0.648(0 or 1) - 0.312(0 or 1) - 0.133(0 or 1) - 0.304(0 or 1) + 0.192(0 or 1)
# Y_hat = 0.376 - 0.133(0 or 1) + 0.192(0 or 1)
# Y_hat_5% = 0.376 + 0.192(1) = 0.568
# Y_hat_5% = 0.376 - 0.133(1) = 0.243
# 0.325 difference in log odds
# prob = odds / (1 + odds)
#     = -0.325 / (1 - 0.325) = -0.4814815 probability 

# Y_hat = 0.376 - 0.312(1) = 0.064
# 0.064 / (1 + 0.064) = 0.06015038 probability 

# part 2c 
modl1 <- glm(choice ~ countries * sanctions, dat, family = "binomial")
compar <- anova(modl, modl1, test = "LRT")
# summary(compar)
compar # fail to reject H0 







