#####################################################################
### R-script to replicate Kueppers & Reiser 2022, Representation  ###
### "Ideological Extremism or Far-Right Attitudes?"               ###
#####################################################################

#### set up R ####
library(haven)
library(survey)
library(stargazer)
library(DescTools)
library(reghelper)
library(effects)
library(car)
library(lmtest)
library(sandwich)
library(tseries)
library(questionr)
library(ggplot2)
library(gridExtra)
library(wdm)
library(ltm)


setwd("")

options(scipen = 999)



#### get data ####
  ## Comment: We used a dataset containing the 2020 wave only; from the GESIS data, you will have to subset the dataset first
      ## tm <- read_spss("tm_kum_2021.sav") # read cumulative dataset
      ## tm20 <- subset(tm, tm$welle == 2020) # subset by 2020 wave

tm20 <- read_spss("tm2020.sav")

# check import:
dim(tm20)
names(tm20)
head(tm20[,24:28])
tail(tm20[,c("V12A", "V12B", "V12C", "V12D", "V12E")])

# surveydesign object:
tm20_w <- svydesign(ids=~0, weights=~persgew, data=tm20)
round(svytable(~alter, tm20_w), digits=0) # test





#### data manipulation ####

##### the DV #####
# skepticism dummies
tm20$core4_gr <- ifelse(tm20$COR_E4>2,1,0) # media alarmism
tm20$core4_gr <- as.factor(tm20$core4_gr)
table(tm20$core4_gr)

tm20$core8_gr <- ifelse(tm20$COR_E8>2,1,0) # main IV: not wore than flu
tm20$core8_gr <- as.factor(tm20$core8_gr)
table(tm20$core8_gr)

tm20$core9_gr <- ifelse(tm20$COR_E9>2,1,0) # trust own feelings
tm20$core9_gr <- as.factor(tm20$core9_gr)
table(tm20$core9_gr)


# Cor-skepticism scale (for OLS)
tm20$skepticism.scale <- tm20$COR_E4+tm20$COR_E8+tm20$COR_E9
tm20$skepticism.scale <- tm20$skepticism.scale-3
table(tm20$skepticism.scale) # higher values = skepticism

# dummy from 3 items
tm20$corskepsis3 <- as.numeric(tm20$core4_gr)+as.numeric(tm20$core8_gr)+as.numeric(tm20$core9_gr)
table(tm20$corskepsis3)
tm20$skeptiker3 <-  ifelse(tm20$corskepsis3 == 6, 1, 0)
tm20$skeptiker3 <- as.factor(tm20$skeptiker3)
table(tm20$skeptiker3)
tm20$corskepsis3 <- NULL # delete



##### the IVs #####

# far-right attitiudes
tm20$REX <- as.numeric(tm20$REX) # REX mean scale
table(tm20$REX, useNA = "ifany")

tm20$REXgr <- ifelse(tm20$REX>2.5,1,0) # right wing extremism (dummy, grouped)
tm20$REXgr <- factor(tm20$REXgr, 
                     levels = c(0,1),
                     labels = c("nonREX","REX")) 
table(tm20$REXgr, useNA = "ifany")



# REX dimensions: Neo-Nazi ideology and ethnocentrism
tm20$NSIdeo <- as.numeric(tm20$NSIdeo)
tm20$Ethno <- as.numeric(tm20$Ethno)



# L/R self-placement
tm20$V19 <- as.numeric(tm20$V19) # full lr scale (high values = right)
table(tm20$V19)

tm20$lr_gr <- recode(tm20$V19, recodes = "1 = 'far-left'; 2 = 'far-left'; 3 = 'left'; 4 = 'middle'; 5 = 'right'; 6 = 'far-right'; 7 = 'far-right'", as.factor = T)

tm20$lr_gr <- relevel(tm20$lr_gr, ref = "middle") # change reference category
table(tm20$lr_gr, useNA = "ifany")



# extremism
# a) distance from the center (squared) as measure for extremism
  # the L/R scale ranges from 1 to 7, so the  middle is 4 -> we need the absolute distance from 4
tm20$extrem <- recode(tm20$V19, recodes = "1=3; 2=2; 3=1; 4=0; 5=1; 6=2; 7=3") 
table(tm20$extrem, useNA = "ifany")
tm20$extrem.sq <- tm20$extrem^2 
table(tm20$extrem.sq, useNA = "ifany")

# b) only the most extreme category on the left/right end of the spectrum (Note: 22 + 4 cases only!)
tm20$extreme_lr <- recode(tm20$V19, recodes = "1 = 'extreme left'; 2 = 'not extreme'; 3 = 'not extreme'; 4 = 'not extreme'; 5 = 'not extreme'; 6 = 'not extreme'; 7 = 'extreme right'", as.factor = T)
tm20$extreme_lr <- relevel(tm20$extreme_lr, ref = 'not extreme')
table(tm20$extreme_lr, useNA = "ifany")



# party ID
table(tm20$V20)
tm20$V20 <- as.numeric(tm20$V20)
tm20$party.id <- recode(tm20$V20, recodes = '0 = "none"; 1 = "CDU"; 2 = "SPD"; 3 = "Left"; 4 = "FDP"; 5 = "Greens"; 6 = "other"; 7 = "other"; 8 = "AfD"; 10 = "other"', as.factor = T)
table(tm20$party.id)
tm20$party.id <- relevel(tm20$party.id, ref = "none") 



##### controls #####

# age
tm20$alter <- as.numeric(tm20$alter)

# gender
table(tm20$sex, useNA = "ifany")
tm20$sex <- factor(tm20$sex, 
                   levels = c(1,2),
                   labels = c("male", "female")) 

# educ
table(tm20$bildung, useNA = "ifany")
tm20$bildung_gr <-ifelse(tm20$bildung>2,1,0)
tm20$bildung_gr <- factor(tm20$bildung_gr, 
                          levels = c(0,1),
                          labels = c("less than A-levels", "A-levels or higher")) 
table(tm20$bildung_gr, useNA = "ifany")

# pol interest 
table(tm20$V06, useNA = "ifany")
tm20$V06 <- as.numeric(tm20$V06)

# affectedness by virus
tm20$COR_B <- factor(tm20$COR_B, 
                     levels = c(1,2,3),
                     labels = c("yes", "no", "no_riskgroup"))
table(tm20$COR_B, useNA = "ifany")

# risk perception
table(tm20$COR_G6)

tm20$COR_G6 <- as.numeric(tm20$COR_G6) # worry about own health, quasimetric version

tm20$corg6_gr <- ifelse(tm20$COR_G6>2,1,0)
tm20$corg6_gr <- factor(tm20$corg6_gr, 
                        levels = c(0,1),
                        labels = c("small threat", "big threat")) # worry about own health
table(tm20$corg6_gr, useNA = "ifany")


tm20$COR_G7 <- as.numeric(tm20$COR_G7)  # worry about pers. economic situation, quasimetric version

tm20$corg7_gr <- ifelse(tm20$COR_G7>2,1,0)
tm20$corg7_gr <- factor(tm20$corg7_gr, 
                        levels = c(0,1),
                        labels = c("small threat", "big threat")) # worry about pers. economic situation
table(tm20$corg7_gr, useNA = "ifany")

tm20$corp2_gr <- ifelse(tm20$COR_P2>2,1,0)
tm20$corp2_gr <- factor(tm20$corp2_gr, 
                        levels = c(0,1),
                        labels = c("disagree", "agree")) # personal economic situation has become worse during pandemic
table(tm20$corp2_gr, useNA = "ifany")


# trust
# trust in RKI
tm20$V09R <- as.numeric(tm20$V09R) 
tm20$v09r_gr <- recode(tm20$V09R, recodes = "1 = 'low'; 2 = 'low'; 3 = 'medium'; 4 = 'high'; 5 = 'high'", as.factor = T)
table(tm20$v09r_gr, useNA = "ifany")

# trust in fed. govt
tm20$V09A <- as.numeric(tm20$V09A) 
tm20$v09a_gr <- recode(tm20$V09A, recodes = "1 = 'low'; 2 = 'low'; 3 = 'medium'; 4 = 'high'; 5 = 'high'", as.factor = T)
table(tm20$v09a_gr, useNA = "ifany")

# reverse direction
tm20$V09A <- 6 - as.numeric(tm20$V09A) 
tm20$V09R <- 6 - as.numeric(tm20$V09R) 
  # => high numbers = low trust

# Covid-19 conspiracy belief
tm20$core5_gr <- ifelse(tm20$COR_E5>2,1,0)
tm20$core5_gr <- factor(tm20$core5_gr, 
                        levels = c(0,1),
                        labels = c("disagree", "agree"))
table(tm20$core5_gr, useNA = "ifany")


# check if everything looks fine:

str(tm20[,c("core8_gr", "skeptiker3", "skepticism.scale", "alter", "bildung_gr", "sex", "V06", "COR_B", "corg6_gr", "corg7_gr", "corp2_gr", "COR_G6", "COR_G7", "lr_gr", "V19", "party.id", "REXgr", "REX", "core5_gr", "v09a_gr",  "v09r_gr", "V09A", "V09R", "extrem", "extrem.sq", "extreme_lr", "NSIdeo", "Ethno")])



# add the new vars to the surveydesign item:

tm20_w <- svydesign(ids=~0, weights=~persgew, data=tm20)



# cronbach's alpha:

skepticism3 <- data.frame(tm20$COR_E4, tm20$COR_E8, tm20$COR_E9)
cronbach.alpha(skepticism3, na.rm = T)

all.rex <- data.frame(tm20$V12A, tm20$V12B, tm20$V12C, tm20$V12D, tm20$V12E, tm20$V12F, tm20$V12G, tm20$V12H, tm20$V12I, tm20$V12J)
cronbach.alpha(all.rex, na.rm = T)




#### descriptive stats ####
t <- svytable(~core8_gr, tm20_w)
round(prop.table(t)*100, digits=0)

t <- svytable(~core8_gr+REXgr, tm20_w)
round(prop.table(t,2)*100, digits=0)

t <- svytable(~REXgr+core8_gr, tm20_w)
round(prop.table(t,2)*100, digits=0)




##### plots #####
# distribution of sceptics along L/R scale (7 point scale):
t <- svytable(~core8_gr+V19, tm20_w)
t
t2 <- round(prop.table(t,2)*100, digits=0)
t2

d2 <- data.frame(t2)
d2
d2$scepticism <- ifelse(d2$core8_gr == 1, "Yes", "No")
d2$V19 <- recode(d2$V19, "1 = 'very far-left*'; 2 = 'far-left'; 3 = 'left'; 4 = 'middle'; 5 = 'right'; 6 = 'far-right*'; 7 = 'very far-right*'")
d2$lr_gr_ord <- factor(d2$V19, 
                       ordered = T,
                       levels = c("very far-left*", "far-left", "left", "middle", "right", "far-right*", "very far-right*"))


d2

ggplot(d2, 
       aes(x=lr_gr_ord, y=Freq, fill=scepticism)) + 
  geom_bar(position="fill", stat="identity") +                               
  scale_y_continuous(labels = scales::percent_format())+ # draw ggplot2 plot scaled to 100%
  labs(x = "self-placement on left-right dimension",
       y = "")+
  theme(axis.text = element_text(size = 11, color = "black"),
        axis.title.x = element_text(size = 14, color = "black"))


ggsave("fig. 1 - LR7_scepticism_col.tiff", units= "in", width=8.5, height=4.5, dpi=300)
ggsave("fig. 1 - LR7_scepticism_col.pdf", units= "in", width=8.5, height=4.5, dpi=300)




# distribution of sceptics according to party ID:
t <- svytable(~core8_gr+party.id, tm20_w)
t
t1 <- round(prop.table(t,2)*100, digits=0)
t1

d1 <- data.frame(t1)
d1
d1$scepticism <- ifelse(d1$core8_gr == 1, "Yes", "No")
d1$party.id_ord <- factor(d1$party.id, 
                          ordered = T,
                          levels = c("CDU", "SPD", "Left", "Greens", "FDP", "AfD", "other", "none"))


d1

ggplot(d1, 
       aes(x=party.id_ord, y=Freq, fill=scepticism)) + 
  geom_bar(position="fill", stat="identity") +                               
  scale_y_continuous(labels = scales::percent_format())+ # draw ggplot2 plot scaled to 100%
  labs(x = "party identification",
       y = "")+
  theme(axis.text = element_text(size = 11, color = "black"),
        axis.title.x = element_text(size = 14, color = "black"))


ggsave("fig. 2 - PartyID_scepticism_col.tiff", units= "in", width=8.5, height=4.5, dpi=300)
ggsave("fig. 2 - PartyID_scepticism_col.pdf", units= "in", width=8.5, height=4.5, dpi=300)





#### regression models ####

##### model 1 #####
# far-right attitiudes (REX) as only ideological predictor
glm.full1q <- glm(core8_gr~REX+alter+sex+bildung_gr+V06+COR_B+corg6_gr+corg7_gr+core5_gr+v09a_gr+v09r_gr,
                 family = quasibinomial(link = "logit"),
                 weights = persgew,
                 data = tm20)

summary(glm.full1q)
round(odds.ratio(glm.full1q), 3)


glm.full1 <- glm(core8_gr~REX+alter+sex+bildung_gr+V06+COR_B+corg6_gr+corg7_gr+core5_gr+v09a_gr+v09r_gr,
                 family = binomial(link = "logit"),
                 weights = persgew,
                 data = tm20)

summary(glm.full1)
round(odds.ratio(glm.full1), 3)

pchisq(glm.full1$null.deviance-glm.full1$deviance, glm.full1$df.null-glm.full1$df.residual, lower = F)

vif(glm.full1)>4

round(PseudoR2(glm.full1, "Nagel"),3)
round(AIC(glm.full1),3)
round(logLik(glm.full1),3)



##### model 2 #####
# extremism^2 (distance from center squared); no far-right attitiudes
glm.full2q <- glm(core8_gr~extrem.sq+alter+sex+bildung_gr+V06+COR_B+corg6_gr+corg7_gr+core5_gr+v09a_gr+v09r_gr,
                 family = quasibinomial(link = "logit"),
                 weights = persgew,
                 data = tm20)

summary(glm.full2q)
round(odds.ratio(glm.full2q), 3)


glm.full2 <- glm(core8_gr~extrem.sq+alter+sex+bildung_gr+V06+COR_B+corg6_gr+corg7_gr+core5_gr+v09a_gr+v09r_gr,
                   family = binomial(link = "logit"),
                   weights = persgew,
                   data = tm20)

summary(glm.full2)
round(odds.ratio(glm.full2), 3)

pchisq(glm.full2$null.deviance-glm.full2$deviance, glm.full2$df.null-glm.full2$df.residual, lower = F)

vif(glm.full1)>4

round(PseudoR2(glm.full2, "Nagel"),3)
round(AIC(glm.full2),3)
round(logLik(glm.full2),3)



##### model 3 #####
# party id as operationalization of ideology
glm.full3q <- glm(core8_gr~party.id+alter+sex+bildung_gr+V06+COR_B+corg6_gr+corg7_gr+core5_gr+v09a_gr+v09r_gr,
                 family = quasibinomial(link = "logit"),
                 weights = persgew,
                 data = tm20)

summary(glm.full3q)
round(odds.ratio(glm.full3q), 3)


glm.full3 <- glm(core8_gr~party.id+alter+sex+bildung_gr+V06+COR_B+corg6_gr+corg7_gr+core5_gr+v09a_gr+v09r_gr,
                 family = binomial(link = "logit"),
                 weights = persgew,
                 data = tm20)

summary(glm.full3)
round(odds.ratio(glm.full3), 3)

pchisq(glm.full3$null.deviance-glm.full3$deviance, glm.full3$df.null-glm.full3$df.residual, lower = F)

vif(glm.full3)>4

round(PseudoR2(glm.full3, "Nagel"),3)
round(AIC(glm.full3),3)
round(logLik(glm.full3),3)



##### model 4 #####
# all 3 operationalizations of ideology: far-right attitiudes, extremism squared and party is
glm.full4q <- glm(core8_gr~REX+extrem.sq+party.id+alter+sex+bildung_gr+V06+COR_B+corg6_gr+corg7_gr+core5_gr+v09a_gr+v09r_gr,
                 family = quasibinomial(link = "logit"),
                 weights = persgew,
                 data = tm20)

summary(glm.full4q)
round(odds.ratio(glm.full4q), 3)


glm.full4 <- glm(core8_gr~REX+extrem.sq+party.id+alter+sex+bildung_gr+V06+COR_B+corg6_gr+corg7_gr+core5_gr+v09a_gr+v09r_gr,
                 family = binomial(link = "logit"),
                 weights = persgew,
                 data = tm20)

summary(glm.full4)
round(odds.ratio(glm.full4), 3)

pchisq(glm.full4$null.deviance-glm.full4$deviance, glm.full4$df.null-glm.full4$df.residual, lower = F)

vif(glm.full4)>4

round(PseudoR2(glm.full4, "Nagel"),3)
round(AIC(glm.full4),3)
round(logLik(glm.full4),3)





#### regression models appendix ####

##### S1 #####

###### model S1.1 ######
lm.1 <- lm(skepticism.scale~REX+extrem.sq+party.id+alter+sex+bildung_gr+V06+COR_B+corg6_gr+corg7_gr+core5_gr+v09a_gr+v09r_gr,
                   weights = persgew,
                   data = tm20)

summary(lm.1)
round(lm.1$coefficients, 3)

par(mfrow = c(2,2))
plot(lm.1)
par(mfrow = c(1,1))

# heteroscedasticity?
bptest(lm.1, studentize = F) # we have a problem with heteroscedasticity -> why we will compute robust se

# normality?
jarque.bera.test(lm.1$residuals) # n.s.
shapiro.test(lm.1$residuals) # n.s.

# Cook's distance?
plot(lm.1, 4) # we're below 0.5

# multicollinearity?
vif(lm.1)>4

# hetroscedasticity robust standard errors
coeftest(lm.1 , vcov. = vcovHC(lm.1 , type = "HC3"))




###### model S1.2 ######
lm.2 <- lm(skepticism.scale~REX+extreme_lr+party.id+alter+sex+bildung_gr+V06+COR_B+corg6_gr+corg7_gr+core5_gr+v09a_gr+v09r_gr,
                    weights = persgew,
                    data = tm20)

summary(lm.2)
round(lm.2$coefficients, 3)


par(mfrow = c(2,2))
plot(lm.2)
par(mfrow = c(1,1))

# heteroscedasticity?
bptest(lm.2, studentize = F) # we have a problem with heteroscedasticity

# normality?
jarque.bera.test(lm.2$residuals) # n.s.
shapiro.test(lm.2$residuals) # n.s.

# Cook's distance?
plot(lm.2, 4) # all values below 0.5, that's fine

# multicollinearity?
vif(lm.2)>4

# hetroscedasticity robust standard errors
coeftest(lm.2 , vcov. = vcovHC(lm.2 , type = "HC3"))




##### S2 #####

###### model S2.1 ######
glm.s21 <- glm(skeptiker3~REX+extrem.sq+party.id+alter+sex+bildung_gr+V06+COR_B+corg6_gr+corg7_gr+core5_gr+v09a_gr+v09r_gr,
                      family = binomial(link = "logit"),
                      weights = persgew,
                      data = tm20)

summary(glm.s21)
round(odds.ratio(glm.s21), 3)

pchisq(glm.s21$null.deviance-glm.s21$deviance, glm.s21$df.null-glm.s21$df.residual, lower = F)

round(PseudoR2(glm.s21, "Nagel"),3)
round(AIC(glm.s21),3)
round(logLik(glm.s21), 3)
vif(glm.s21)>4



###### model S2.2 ######
glm.s22 <- glm(skeptiker3~REX+extreme_lr+party.id+alter+sex+bildung_gr+V06+COR_B+corg6_gr+corg7_gr+core5_gr+v09a_gr+v09r_gr,
                       family = binomial(link = "logit"),
                       weights = persgew,
                       data = tm20)

summary(glm.s22)
round(odds.ratio(glm.s22), 3)

pchisq(glm.s22$null.deviance-glm.s22$deviance, glm.s22$df.null-glm.s22$df.residual, lower = F)

round(PseudoR2(glm.s22, "Nagel"),3)
round(AIC(glm.s22),3)
round(logLik(glm.s22), 3)
vif(glm.s22)>4



##### S3 #####

###### model S3.1 ######
glm.s31<- glm(core8_gr~V19+alter+sex+bildung_gr+V06+COR_B+corg6_gr+corg7_gr+core5_gr+v09a_gr+v09r_gr, 
                 family=binomial(link="logit"), 
                 weights = persgew, 
                 data = tm20)

summary(glm.s31)
round(odds.ratio(glm.s31), 3)

pchisq(glm.s31$null.deviance-glm.s31$deviance, glm.s31$df.null-glm.s31$df.residual, lower = F)

round(PseudoR2(glm.s31, "Nagel"),3)
round(AIC(glm.s31),3)
round(logLik(glm.s31), 3)
vif(glm.s31)>4


###### model S3.2 ######
glm.s32<- glm(core8_gr~extreme_lr+alter+sex+bildung_gr+V06+COR_B+corg6_gr+corg7_gr+core5_gr+v09a_gr+v09r_gr, 
              family=binomial(link="logit"), 
              weights = persgew, 
              data = tm20)

summary(glm.s32)
round(odds.ratio(glm.s32), 3)

pchisq(glm.s32$null.deviance-glm.s32$deviance, glm.s32$df.null-glm.s32$df.residual, lower = F)

round(PseudoR2(glm.s32, "Nagel"),3)
round(AIC(glm.s32),3)
round(logLik(glm.s32), 3)
vif(glm.s32)>4



##### S4 #####
glm.s4 <- glm(core8_gr~REX+extrem.sq+party.id+alter+sex+bildung_gr+V06+COR_B+COR_G6+COR_G7+core5_gr+V09A+V09R, 
                   family=binomial(link="logit"), 
                   weights = persgew, 
                   data = tm20)

summary(glm.s4)
round(odds.ratio(glm.s4),3)

pchisq(glm.s4$null.deviance-glm.s4$deviance, glm.s4$df.null-glm.s4$df.residual, lower = F)

round(PseudoR2(glm.s4, "Nagel"),3)
round(AIC(glm.s4),3)
round(logLik(glm.s4), 3)
vif(glm.s4)>4



##### S5 #####
glm.s5 <- glm(core8_gr~REX+extrem.sq+party.id+alter+sex+bildung_gr+V06+COR_B+corg6_gr+corp2_gr+core5_gr+v09a_gr+v09r_gr,
                  family = binomial(link = "logit"),
                  weights = persgew,
                  data = tm20)

summary(glm.s5)
round(odds.ratio(glm.s5), 3)

pchisq(glm.s5$null.deviance-glm.s5$deviance, glm.s5$df.null-glm.s5$df.residual, lower = F)

round(PseudoR2(glm.s5, "Nagel"),3)
round(AIC(glm.s5),3)
round(logLik(glm.s5), 3)
vif(glm.s5)>4



##### S6 #####
glm.s6 <- glm(core8_gr~NSIdeo+Ethno+extrem.sq+party.id+alter+sex+bildung_gr+V06+COR_B+corg6_gr+corg7_gr+core5_gr+v09a_gr+v09r_gr,
                 family = binomial(link = "logit"),
                 weights = persgew,
                 data = tm20)

summary(glm.s6)
round(odds.ratio(glm.s6), 3)

pchisq(glm.s6$null.deviance-glm.s6$deviance, glm.s6$df.null-glm.s6$df.residual, lower = F)
round(PseudoR2(glm.s6, "Nagel"),3)
round(AIC(glm.s6),3)
round(logLik(glm.s6), 3)
vif(glm.s6)>4




#### Predicted probabilities ####
# plot REX
eff.REX <- effect(glm.full4, 
                  term = c("REX"), 
                  xlevels = list(REX = seq(1, 4, by = 0.1)))


p.rex <- plot(eff.REX,
              rescale.axis = F,
              rug = F,
              main = "",
              xlab = "Far-right attitudes (mean scale; 1 = low | 4 = high)",
              ylab = "Predicted probability for COVID-19 scepticism")
p.rex



# plot extremism
eff.ex <- effect(glm.full4,
                 term = c("extrem.sq"),
                 xlevels = list(extrem.sq = seq(1, 9, by = 0.1)))

p.ex <- plot(eff.ex,
             rescale.axis = F,
             rug = F,
             main = "",
             xlab = "Ideological extremism (distance from the middle of the left-right scale squared)",
             ylab = "Predicted probability for COVID-19 scepticism")
p.ex


# arrange plots on 1 canvas 
grid.arrange(p.rex, p.ex, ncol=1, nrow =2)






