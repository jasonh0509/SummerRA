##Library Packages
library(ggplot2)
library(broom)
library(dplyr)

## Read in Data
ncGrandData<-read.csv("https://raw.githubusercontent.com/jasonh0509/SummerRA/main/ncBigdata.csv")

##Take a look
summary(ncGrandData)
glimpse(ncGrandData)

##Testing NA
sum(is.na(ncGrandData))

sum(ncGrandData$Year=="1999")

dataAlamance<-subset(ncGrandData,Place=="Alamance")

##Mutate to numeric
dataAlamance["rate_mult"][dataAlamance["rate_mult"]=="100,000"]<-"100000"
ncGrandData["rate_mult"][ncGrandData["rate_mult"]=="100,000"]<-"100000"

ncGrandData$rate_mult<-as.numeric(ncGrandData$rate_mult)
##Count rate numi
sum(dataAlamance$rate_mult=="	
100000")
