##Library Packages
library(ggplot2)
library(broom)
library(dplyr)

## Read in Data
ncGrandData<-read.csv("https://raw.githubusercontent.com/jasonh0509/SummerRA/main/OAP.csv")

##Take a look
summary(ncGrandData)
glimpse(ncGrandData)

##Testing NA
sum(is.na(ncGrandData))


