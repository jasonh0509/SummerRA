
library(ggplot2)
library(broom)
library(dplyr)
library(tidyverse)


NA2010<-read.csv("https://raw.githubusercontent.com/jasonh0509/SummerRA/main/Minority2010.csv")
NA2015<-read.csv("https://raw.githubusercontent.com/jasonh0509/SummerRA/main/Minority2015.csv")
NA2020<-read.csv("https://raw.githubusercontent.com/jasonh0509/SummerRA/main/Minority2020.csv")

totaldata<-rbind(NA2010,NA2015,NA2020)

sum(is.na(NA2010$Value.Count))

graph2010<-subset(NA2010,select = c(Place,Year,Geoid,Rate.Denom,Value.Count,Value.Rate))
graph2010$FIPS=graph2010$Geoid-37000

map=readOGR(dsn='Shapes',layer='cb_2014_us_county_500k')

ncMap=fortify(map[oh.map$STATEFP=='37',],region='COUNTYFP')
ncMap$id2=as.numeric(ncMap$id)
ncMap.merge=merge(ncMap,graph2010,by.x="id2",by.y="FIPS")

plot2010<-ggplot()+
  geom_polygon(data=ncMap.merge,aes(x=long,y=lat,group=group,fill=Value.Rate),color='black',alpha=.8,size=.3)+
  #scale_fill_gradient2(name="",limits=limits,low="navy", mid="white", high="red")+
  scale_fill_gradient(low="#FFFFFF",high = "#8b0000",)+
  coord_map()+
  theme_nothing(base_size=12, legend=T)+
  ggtitle("AI/AN Rate Map 2010")+theme(plot.title = element_text(hjust = 0.5,size = rel(2.25)),legend.text=element_text(size=rel(2)),legend.key.size=unit(2,"line"))
plot2010

plot2015<-ggplot()+
  geom_polygon(data=ncMap.merge,aes(x=long,y=lat,group=group,fill=Value.Rate),color='black',alpha=.8,size=.3)+
  #scale_fill_gradient2(name="",limits=limits,low="navy", mid="white", high="red")+
  scale_fill_gradient(low="#FFFFFF",high = "#8b0000",)+
  coord_map()+
  theme_nothing(base_size=12, legend=T)+
  ggtitle("AI/AN Rate Map 2010")+theme(plot.title = element_text(hjust = 0.5,size = rel(2.25)),legend.text=element_text(size=rel(2)),legend.key.size=unit(2,"line"))
plot2010

try<-DataForJason
