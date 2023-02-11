library(tidyverse)
library(stargazer)
library(ggplot2)
library(dplyr)

library(readr)
# datas <- read_csv("data/P_Data_Extract_From_World_Development_Indicators/f18d2765-f857-4bb5-8077-38f7aabeec6d_Data.csv", 
                                                     # col_types = cols(Time = col_skip(), `Time Code` = col_skip(), 
                                                                      # `Country Name` = col_skip(), `Ease of doing business rank (1=most business-friendly regulations) [IC.BUS.EASE.XQ]` = col_integer(), 
                                                                     #  `GDP per capita (current US$) [NY.GDP.PCAP.CD]` = col_double(), 
                                                                      # `Tax revenue (% of GDP) [GC.TAX.TOTL.GD.ZS]` = col_double()))
library(readr)
datas <- read_csv("data/P_Data_Extract_From_World_Development_Indicators/f18d2765-f857-4bb5-8077-38f7aabeec6d_Data.csv", 
                                                      col_types = cols(Time = col_skip(), `Time Code` = col_skip(), 
                                                                       `Country Name` = col_skip(), `Ease of doing business rank (1=most business-friendly regulations) [IC.BUS.EASE.XQ]` = col_number(), 
                                                                       `GDP per capita (current US$) [NY.GDP.PCAP.CD]` = col_number(), 
                                                                       `Tax revenue (% of GDP) [GC.TAX.TOTL.GD.ZS]` = col_number()))

View(datas) 

names(datas) <- sub("\\[.*","", names(datas)) # tells R to remove everything after the square bracket 
# names(datas) <- sub("\\(\\)","", names(datas))
names(datas) <- sub("Ease of doing business rank (1 = most business friendly regulations)","Ease of doing business score (0 = lowest performance to 100 = best performance", names(datas))

datas %>%
  ggplot(aes('Tax revenue (% of GDP)', 'GDP per capita (current US$)')) +
  geom_point() +
  geom_smooth(method = "lm")

datas %>%
  ggplot(aes('Ease of doing business score (0 = lowest performance to 100 = best performance', 'GDP per capita (current US$)'))+
  geom_point() +
  geom_smooth(method = "lm")


  