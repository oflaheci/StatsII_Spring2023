pkgTest <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[,  "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg,  dependencies = TRUE)
  sapply(pkg,  require,  character.only = TRUE)
}

lapply(c(),  pkgTest)

library(MASS)
library(nnet)
library(ggplot2)
library(foreign)
library(reshape2)
library(stargazer)
library(sandwich)
install.packages("AER")
library(AER)

gdpChange <- read.csv("gdpChange.csv")
orgdf <- gdpChange

## wrangling
gdpChange$GDPWdiff <- cut(orgdf$GDPWdiff, 
                          breaks = c(-10000, -1, 1, 10000), 
                          labels = c("negative","noChange","positive"))
# unique(gdpChange$GDPWdiff)

## descriptive stats / eda
summary(gdpChange)
with(gdpChange, table(OIL, GDPWdiff))
with(gdpChange, table(REG, GDPWdiff))

ftable(xtabs(~ REG + OIL + GDPWdiff, data = gdpChange))

#### UNORDERED MULTINOMIAL LOGIT

# set a reference level "no change"
gdpChange$GDPWdiff = relevel(gdpChange$GDPWdiff, ref = "noChange")

# run model
mult.log <- multinom(GDPWdiff ~ REG + OIL, data = gdpChange)
summary(mult.log)
stargazer(mult.log, type = "text")
stargazer(mult.log, type = "latex")
## relative risk rations
mult.log.rrr <- exp(coef(mult.log))
stargazer(mult.log, type="text", coef=list(mult.log.rrr), p.auto=FALSE)
stargazer(mult.log, type="latex", coef=list(mult.log.rrr), p.auto=FALSE)

# get p values
z <- summary(mult.log)$coefficients/summary(mult.log)$standard.errors
(p <- (1 - pnorm(abs(z), 0, 1)) * 2)

# predicted probabilities to help our interpretation
pp <- data.frame(fitted(mult.log))
head(data.frame(GDPWdiff = gdpChange$GDPWdiff,
                positive = pp$positive,
                noChange = pp$noChange,
                negative = pp$negative))


### ORDERED MULTINOMIAL LOGIT

ord.log <- polr(GDPWdiff ~ REG + OIL, data = gdpChange, Hess = TRUE)
summary(ord.log)

# Calculate a p value
ctable <- coef(summary(ord.log))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
(ctable <- cbind(ctable, "p value" = p))

# Calculate confidence intervals
(ci <- confint(ord.log))

# convert to odds ratio
exp(cbind(OR = coef(ord.log), ci))

### Question 2
# clear environment 
rm(list=ls())

# load data
mexMun <- read.csv("MexicoMuniData.csv")
summary(mexMun)
head(mexMun)
# negative values for marginality - interpret as increasing marginality as score rises
sum(is.na(c(mexMun$competitive.district, mexMun$marginality.06,
            mexMun$PAN.governor.06, mexMun$PAN.visits.06))) # =0

#run model
poisson.model <- glm(PAN.visits.06 ~ 
                        competitive.district + marginality.06 + PAN.governor.06, 
                        data = mexMun, family = poisson(link = "log"))
summary(poisson.model) # residual deviance - is less than degrees of freedom so dispersion <.9 (0.41)-> potentially under dispersed 
dispersiontest(poisson.model, trafo = 1) # p-value = 0.143 all good
stargazer(poisson.model, type = "latex")

# p-values 
cov.md1 <- vcovHC(poisson.model, type="HC0")
std.err <- sqrt(diag(cov.md1))
r.est <- cbind(Estimate= coef(poisson.model), "Robust SE" = std.err,
               "Pr(>|z|)" = 2 * pnorm(abs(coef(poisson.model)/std.err), lower.tail=FALSE),
               LowLim = coef(poisson.model) - 1.96 * std.err,
               UppLim = coef(poisson.model) + 1.96 * std.err)

r.est
stargazer(r.est, type = "latex")
