setwd("./files")

library(dplyr)
library(lubridate)

##picking specific stations to look at, over 2018-2022 period. also inserting a 'bike usage' variable

#vector of the station names of interest
stations <- c("CUSTOM HOUSE QUAY", "GRAND CANAL DOCK", "THE POINT")

##2018 Q3

data1 <- read.csv("../data/dublinbikes_20180701_20181001.csv")

# Subset the data based on the stations vector
data1_subset <- subset(data1, NAME %in% stations)
rm(data1)

#group the data by station and arrange by time
data1_subset <- data1_subset %>%
  group_by(`STATION.ID`) %>%
  arrange(`TIME`)

#calculate the difference in available bikes between each time point
data1_subset$BIKE.USAGE <- c(0, diff(data1_subset$`AVAILABLE.BIKES`))

data1_subset <- ungroup(data1_subset)

# Convert TIME column to POSIXct format
data1_subset$TIME <- as.POSIXct(data1_subset$TIME)

# Aggregate data by hour (this is across all days)
#data1_hourly <- data1_subset %>%
  #group_by(NAME, hour = lubridate::hour(TIME)) %>%
  #summarise(avg_bike_usage = mean(BIKE.USAGE),
            #avg_available_bikes = mean(AVAILABLE.BIKES),
            #avg_available_bike_stands = mean(AVAILABLE.BIKE.STANDS))

data1_hourly <- data1_subset %>%
  mutate(hour = format(TIME, "%Y-%m-%d %H:00:00"))

# save as CSV
write.csv(data1_hourly, "../data/18Q3.csv")
# read in and check
data1_hr_readin <- read.csv("../data/18Q3.csv")
# will need to delete obvs column when read in for each
data1_hr_readin <- subset(data1_hr_readin, select = -c(X))

## clear environment except stations
rm(list=setdiff(ls(), "stations"))

#2018 Q4

data2 <- read.csv("../data/dublinbikes_20181001_20190101.csv")
data2_subset <- subset(data2, NAME %in% stations)
rm(data2)

data2_subset <- data2_subset %>%
  group_by(`STATION.ID`) %>%
  arrange(`TIME`)

data2_subset$BIKE.USAGE <- c(0, diff(data2_subset$`AVAILABLE.BIKES`))

data2_subset <- ungroup(data2_subset)

# Convert TIME column to POSIXct format
data2_subset$TIME <- as.POSIXct(data2_subset$TIME)

data2_hourly <- data2_subset %>%
  mutate(hour = format(TIME, "%Y-%m-%d %H:00:00"))
rm(data2_subset)

# save as CSV
write.csv(data2_hourly, "../data/18Q4.csv")
# read in and check
data2_hr_readin <- read.csv("../data/18Q4.csv")

rm(list=setdiff(ls(), "stations"))

#2019 Q1
data3 <- read.csv("../data/dublinbikes_20190101_20190401.csv")
data3_subset <- subset(data3, NAME %in% stations)
rm(data3)

data3_subset <- data3_subset %>%
  group_by(`STATION.ID`) %>%
  arrange(`TIME`)

data3_subset$BIKE.USAGE <- c(0, diff(data3_subset$`AVAILABLE.BIKES`))

data3_subset <- ungroup(data3_subset)

# Convert TIME column to POSIXct format
data3_subset$TIME <- as.POSIXct(data3_subset$TIME)

data3_hourly <- data3_subset %>%
  mutate(hour = format(TIME, "%Y-%m-%d %H:00:00"))
rm(data3_subset)

# save as CSV
write.csv(data3_hourly, "../data/19Q1.csv")
# read in and check
data3_hr_readin <- read.csv("../data/19Q1.csv")

rm(list=setdiff(ls(), "stations"))

## 2019 Q2

data4 <- read.csv("../data/dublinbikes_20190401_20190701.csv")
data4_subset <- subset(data4, NAME %in% stations)
rm(data4)

data4_subset <- data4_subset %>%
  group_by(`STATION.ID`) %>%
  arrange(`TIME`)

data4_subset$BIKE.USAGE <- c(0, diff(data4_subset$`AVAILABLE.BIKES`))

data4_subset <- ungroup(data4_subset)

# Convert TIME column to POSIXct format
data4_subset$TIME <- as.POSIXct(data4_subset$TIME)

data4_hourly <- data4_subset %>%
  mutate(hour = format(TIME, "%Y-%m-%d %H:00:00"))
rm(data4_subset)

# save as CSV
write.csv(data4_hourly, "../data/19Q2.csv")
# read in and check
data4_hr_readin <- read.csv("../data/19Q2.csv")

rm(list=setdiff(ls(), "stations"))

## 2019 Q3
data5 <- read.csv("../data/dublinbikes_20190701_20191001.csv")
data5_subset <- subset(data5, NAME %in% stations)
rm(data5)

data5_subset <- data5_subset %>%
  group_by(`STATION.ID`) %>%
  arrange(`TIME`)

data5_subset$BIKE.USAGE <- c(0, diff(data5_subset$`AVAILABLE.BIKES`))

data5_subset <- ungroup(data5_subset)

# Convert TIME column to POSIXct format
data5_subset$TIME <- as.POSIXct(data5_subset$TIME)

data5_hourly <- data5_subset %>%
  mutate(hour = format(TIME, "%Y-%m-%d %H:00:00"))
rm(data5_subset)

# save as CSV
write.csv(data5_hourly, "../data/19Q3.csv")
# read in and check
data5_hr_readin <- read.csv("../data/19Q3.csv")

rm(list=setdiff(ls(), "stations"))

# 2019 Q4

data6 <- read.csv("../data/dublinbikes_20191001_20200101.csv")
data6_subset <- subset(data6, NAME %in% stations)
rm(data6)

data6_subset <- data6_subset %>%
  group_by(`STATION.ID`) %>%
  arrange(`TIME`)

data6_subset$BIKE.USAGE <- c(0, diff(data6_subset$`AVAILABLE.BIKES`))

data6_subset <- ungroup(data6_subset)

# Convert TIME column to POSIXct format
data6_subset$TIME <- as.POSIXct(data6_subset$TIME)

data6_hourly <- data6_subset %>%
  mutate(hour = format(TIME, "%Y-%m-%d %H:00:00"))
rm(data6_subset)

# save as CSV
write.csv(data6_hourly, "../data/19Q4.csv")
# read in and check
data6_hr_readin <- read.csv("../data/19Q4.csv")

rm(list=setdiff(ls(), "stations"))

# 2020 Q1
data7 <- read.csv("../data/dublinbikes_20200101_20200401.csv")
data7_subset <- subset(data7, NAME %in% stations)
rm(data7)

data7_subset <- data7_subset %>%
  group_by(`STATION.ID`) %>%
  arrange(`TIME`)

data7_subset$BIKE.USAGE <- c(0, diff(data7_subset$`AVAILABLE.BIKES`))

data7_subset <- ungroup(data7_subset)

# Convert TIME column to POSIXct format
data7_subset$TIME <- as.POSIXct(data7_subset$TIME)

data7_hourly <- data7_subset %>%
  mutate(hour = format(TIME, "%Y-%m-%d %H:00:00"))
rm(data7_subset)

# save as CSV
write.csv(data7_hourly, "../data/20Q1.csv")
# read in and check
data7_hr_readin <- read.csv("../data/20Q1.csv")

rm(list=setdiff(ls(), "stations"))

## 2020 Q2
data8 <- read.csv("../data/dublinbikes_20200401_20200701.csv")
data8_subset <- subset(data8, NAME %in% stations)
rm(data8)

data8_subset <- data8_subset %>%
  group_by(`STATION.ID`) %>%
  arrange(`TIME`)

data8_subset$BIKE.USAGE <- c(0, diff(data8_subset$`AVAILABLE.BIKES`))

data8_subset <- ungroup(data8_subset)

# Convert TIME column to POSIXct format
data8_subset$TIME <- as.POSIXct(data8_subset$TIME)

data8_hourly <- data8_subset %>%
  mutate(hour = format(TIME, "%Y-%m-%d %H:00:00"))
rm(data8_subset)

# save as CSV
write.csv(data8_hourly, "../data/20Q2.csv")
# read in and check
data8_hr_readin <- read.csv("../data/20Q2.csv")

rm(list=setdiff(ls(), "stations"))

## 2020 Q3

data9 <- read.csv("../data/dublinbikes_20200701_20201001.csv")
data9_subset <- subset(data9, NAME %in% stations)
rm(data9)

data9_subset <- data9_subset %>%
  group_by(`STATION.ID`) %>%
  arrange(`TIME`)

data9_subset$BIKE.USAGE <- c(0, diff(data9_subset$`AVAILABLE.BIKES`))

data9_subset <- ungroup(data9_subset)

# Convert TIME column to POSIXct format
data9_subset$TIME <- as.POSIXct(data9_subset$TIME)

data9_hourly <- data9_subset %>%
  mutate(hour = format(TIME, "%Y-%m-%d %H:00:00"))
rm(data9_subset)

# save as CSV
write.csv(data9_hourly, "../data/20Q3.csv")
# read in and check
data9_hr_readin <- read.csv("../data/20Q3.csv")

rm(list=setdiff(ls(), "stations"))

## 2020 Q4

data10 <- read.csv("../data/dublinbikes_20201001_20210101.csv")
data10_subset <- subset(data10, NAME %in% stations)
rm(data10)

data9_subset <- data9_subset %>%
  group_by(`STATION.ID`) %>%
  arrange(`TIME`)

data10_subset$BIKE.USAGE <- c(0, diff(data10_subset$`AVAILABLE.BIKES`))

data10_subset <- ungroup(data10_subset)

# Convert TIME column to POSIXct format
data10_subset$TIME <- as.POSIXct(data10_subset$TIME)

data10_hourly <- data10_subset %>%
  mutate(hour = format(TIME, "%Y-%m-%d %H:00:00"))
rm(data10_subset)

# save as CSV
write.csv(data10_hourly, "../data/20Q4.csv")
# read in and check
data10_hr_readin <- read.csv("../data/20Q4.csv")

rm(list=setdiff(ls(), "stations"))

## 2021 Q1

data11 <- read.csv("../data/dublinbikes_20210101_20210401.csv")
data11_subset <- subset(data11, NAME %in% stations)
rm(data11)

data11_subset <- data11_subset %>%
  group_by(`STATION.ID`) %>%
  arrange(`TIME`)

data11_subset$BIKE.USAGE <- c(0, diff(data11_subset$`AVAILABLE.BIKES`))

data11_subset <- ungroup(data11_subset)

# Convert TIME column to POSIXct format
data11_subset$TIME <- as.POSIXct(data11_subset$TIME)

data11_hourly <- data11_subset %>%
  mutate(hour = format(TIME, "%Y-%m-%d %H:00:00"))
rm(data11_subset)

# save as CSV
write.csv(data11_hourly, "../data/21Q1.csv")
# read in and check
data11_hr_readin <- read.csv("../data/21Q1.csv")

rm(list=setdiff(ls(), "stations"))

## 2021 Q2
data13 <- read.csv("../data/dublinbikes_20210401_20210701.csv")
data13_subset <- subset(data13, NAME %in% stations)
rm(data13)

data13_subset <- data13_subset %>%
  group_by(`STATION.ID`) %>%
  arrange(`TIME`)

data13_subset$BIKE.USAGE <- c(0, diff(data13_subset$`AVAILABLE.BIKES`))

data13_subset <- ungroup(data13_subset)

# Convert TIME column to POSIXct format
data13_subset$TIME <- as.POSIXct(data13_subset$TIME)

data13_hourly <- data13_subset %>%
  mutate(hour = format(TIME, "%Y-%m-%d %H:00:00"))
rm(data13_subset)

# save as CSV
write.csv(data13_hourly, "../data/21Q4.csv")
# read in and check
data13_hr_readin <- read.csv("../data/21Q4.csv")

rm(list=setdiff(ls(), "stations"))

## 2021 Q3
data14 <- read.csv("../data/dublinbikes_20210701_20211001.csv")
data14_subset <- subset(data14, NAME %in% stations)
rm(data14)

data14_subset <- data14_subset %>%
  group_by(`STATION.ID`) %>%
  arrange(`TIME`)

data14_subset$BIKE.USAGE <- c(0, diff(data14_subset$`AVAILABLE.BIKES`))

data14_subset <- ungroup(data14_subset)

# Convert TIME column to POSIXct format
data14_subset$TIME <- as.POSIXct(data14_subset$TIME)

data14_hourly <- data14_subset %>%
  mutate(hour = format(TIME, "%Y-%m-%d %H:00:00"))
rm(data14_subset)

# save as CSV
write.csv(data14_hourly, "../data/22Q3.csv")
# read in and check
data14_hr_readin <- read.csv("../data/22Q3.csv")

rm(list=setdiff(ls(), "stations"))

## 2021 Q4
data15 <- read.csv("../data/dublinbikes_20211001_20220101.csv")
data15_subset <- subset(data15, NAME %in% stations)
rm(data15)

data15_subset <- data15_subset %>%
  group_by(`STATION.ID`) %>%
  arrange(`TIME`)

data15_subset$BIKE.USAGE <- c(0, diff(data15_subset$`AVAILABLE.BIKES`))

data15_subset <- ungroup(data15_subset)

# Convert TIME column to POSIXct format
data15_subset$TIME <- as.POSIXct(data15_subset$TIME)

data15_hourly <- data15_subset %>%
  mutate(hour = format(TIME, "%Y-%m-%d %H:00:00"))
rm(data15_subset)

# save as CSV
write.csv(data15_hourly, "../data/22Q4.csv")
# read in and check
data15_hr_readin <- read.csv("../data/22Q4.csv")

rm(list=setdiff(ls(), "stations"))
