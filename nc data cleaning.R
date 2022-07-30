##Library Packages
library(ggplot2)
library(broom)
library(dplyr)
library(tidyverse)

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

ncfilter1<-ncGrandData%>%
  subset(ncGrandData$Place.Type=="Counties")
 

ncfilter1<-subset(ncfilter1,select=c(Measure,Place,Place.Type,Geoid,Value.Count))



valueC<-subset(ncfilter1,select = Value.Count)
valueC<-valueC%>%
  mutate(Value.Count=as.double(Value.Count))
ncfilter1

ncfilter2<-ncfilter1%>%
  pivot_wider(names_from = Measure,values_from = Value.Count)