#########################################################################
### Syntax for far-right (REX) attitudes scale and its subdimensions  ###
#########################################################################

library(car)
library(haven)

setwd("")

tm <- read_spss("tm_kum_2021_fixed.sav")
tm20 <- subset(tm, tm$welle == 2020)


# goal: replicate the mean.x command in SPSS to allow the mean scales of the REX dimensions Ethnocentrism (Ethno) and NS Ideology (NSIdeo) to be computed when one missing value is present

# the 10 items measuring far-right attitudes in the Thueringen-Monitor form 2 subdimensions: Ethnocentrism (V12A, V12B, V12C, V12E) and NSIdeology (V12D, V12F, V12G, V12H, V12I, V12J)




# Ethnocentrism scale:
# mean.3(v12a, v12b, v12c, v12e) in SPSS computes a mean scale with one missing value allowed
tm20$Ethno <- ifelse((is.na(tm20$V12A) + is.na(tm20$V12B) + is.na(tm20$V12C) + is.na(tm20$V12E)) > 1, NA,
                     rowMeans(tm20[,c("V12A", "V12B", "V12C", "V12E")], na.rm=TRUE))


table(tm20$Ethno)



# NS Ideology scale:
# NSIdeo = mean.5(v12d, v12f, v12g, v12h, v12i, v12j): the SPSS command computes a mean scale with one missing value allowed
tm20$NSIdeo <- ifelse((is.na(tm20$V12D) + is.na(tm20$V12F) + is.na(tm20$V12G) + is.na(tm20$V12H) + is.na(tm20$V12I) + is.na(tm20$V12J)) > 1, NA,
                      rowMeans(tm20[,c("V12D", "V12F", "V12G", "V12H", "V12I", "V12J")], na.rm=TRUE))

table(tm20$NSIdeo)
summary(tm20$NSIdeo)


# REX
tm20$REX <- (tm20$Ethno+tm20$NSIdeo)/2


summary(tm20$REX)