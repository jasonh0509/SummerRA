library('rgeos')
library('spdep')
library('rgdal')
library('maptools')
library('shapefiles')
library('ggmap')
library('ggpubr')
library('ggplot2')
library('gridExtra')
library('sf')
library('broom')
library(dplyr)

#Load big shape file
mapG=readOGR(dsn='Shapes',layer='cb_2014_us_county_500k')
ncMap=fortify(mapG[mapG$STATEFP=='37',],region='COUNTYFP')
ncMap$id2=as.numeric(ncMap$id)

#AI/AN Data
NA2010<-read.csv("https://raw.githubusercontent.com/jasonh0509/SummerRA/main/Minority2010.csv")
NA2015<-read.csv("https://raw.githubusercontent.com/jasonh0509/SummerRA/main/Minority2015.csv")
NA2020<-read.csv("https://raw.githubusercontent.com/jasonh0509/SummerRA/main/Minority2020.csv")

totaldataAIAN<-rbind(NA2010,NA2015,NA2020)
totaldataAIAN<-totaldataAIAN%>%
  mutate(Value.Rate=((Value.Rate)/10))


graphtotal<-subset(totaldataAIAN,select = c(Place,Year,Geoid,Rate.Denom,Value.Count,Value.Rate))
graphtotal$FIPS=graphtotal$Geoid-37000


ncMap=fortify(mapG[mapG$STATEFP=='37',],region='COUNTYFP')
ncMap$id2=as.numeric(ncMap$id)
ncMap.merge=merge(ncMap,graphtotal,by.x="id2",by.y="FIPS")

#2010 data 
mapping2010<-ncMap.merge[which(ncMap.merge$Year==2010),]


#2015 data
mapping2015<-ncMap.merge[which(ncMap.merge$Year==2015),]

#2020 data
mapping2020<-ncMap.merge[which(ncMap.merge$Year==2020),]

#limits = c(min(NA2010$Value.Rate),max(NA2010$Value.Rate))

RawAIANplot2010<-ggplot()+
  geom_polygon(data=mapping2010,aes(x=long,y=lat,group=group,fill=Value.Rate),color='black',alpha=.8,size=.3)+
  #scale_fill_gradient2(name="",limits=limits,low="navy", mid="white", high="red")+
  scale_fill_gradient(low="#FFFFFF",high = "#8b0000",limits=c(0,30),
                      breaks=c(0,15,30))+
  coord_map()+
  theme_nothing(base_size=12, legend=T)+
  ggtitle("AI/AN Raw Death Rate Map 2010")+theme(plot.title = element_text(hjust = 0.5,size = rel(2.25)),legend.text=element_text(size=rel(2)),legend.key.size=unit(2,"line"))


RawAIANplot2015<-ggplot()+
  geom_polygon(data=mapping2015,aes(x=long,y=lat,group=group,fill=Value.Rate),color='black',alpha=.8,size=.3)+
  #scale_fill_gradient2(name="",limits=limits,low="navy", mid="white", high="red")+
  scale_fill_gradient(low="#FFFFFF",high = "#8b0000",limits=c(0,30),
                      breaks=c(0,15,30))+
  coord_map()+
  theme_nothing(base_size=12, legend=T)+
  ggtitle("AI/AN Raw Death Rate Map 2015")+theme(plot.title = element_text(hjust = 0.5,size = rel(2.25)),legend.text=element_text(size=rel(2)),legend.key.size=unit(2,"line"))



RawAIANplot2020<-ggplot()+
  geom_polygon(data=mapping2020,aes(x=long,y=lat,group=group,fill=Value.Rate),color='black',alpha=.8,size=.3)+
  #scale_fill_gradient2(name="",limits=limits,low="navy", mid="white", high="red")+
  scale_fill_gradient(low="#FFFFFF",high = "#D70040",limits=c(0,30),
                      breaks=c(0,15,30))+
  coord_map()+
  theme_nothing(base_size=12, legend=T)+
  ggtitle("AI/AN Death Rate Map 2020")+theme(plot.title = element_text(hjust = 0.5,size = rel(2.25)),legend.text=element_text(size=rel(2)),legend.key.size=unit(2,"line"))


RawAIANplot2010
RawAIANplot2015
RawAIANplot2020
