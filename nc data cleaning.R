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


ncGrandData$rate_mult<-as.numeric(ncGrandData$rate_mult)
##Count rate numi


ncfilter1<-ncGrandData%>%
  subset(ncGrandData$Place.Type=="Counties")


ncfilter1<-ncfilter1%>%
  select(-Value.Rate)


ncNew<-ncfilter1%>%
  pivot_wider(names_from = Measure,values_from = Value.Count,values_fill = 0)

write.csv(ncNew,"C:/RGit/SummerRA/ncFilteredWith_Attribute.csv",row.names = FALSE)
