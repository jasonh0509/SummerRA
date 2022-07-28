
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

#AI/AN Data

NA2010<-read.csv("https://raw.githubusercontent.com/jasonh0509/SummerRA/main/Minority2010.csv")
NA2015<-read.csv("https://raw.githubusercontent.com/jasonh0509/SummerRA/main/Minority2015.csv")
NA2020<-read.csv("https://raw.githubusercontent.com/jasonh0509/SummerRA/main/Minority2020.csv")

totaldataAIAN<-rbind(NA2010,NA2015,NA2020)

#sum(is.na(NA2010$Value.Count))

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

AIANplot2010<-ggplot()+
  geom_polygon(data=mapping2010,aes(x=long,y=lat,group=group,fill=Value.Rate),color='black',alpha=.8,size=.3)+
  #scale_fill_gradient2(name="",limits=limits,low="navy", mid="white", high="red")+
  scale_fill_gradient(low="#FFFFFF",high = "#8b0000",)+
  coord_map()+
  theme_nothing(base_size=12, legend=T)+
  ggtitle("AI/AN Rate Map 2010")+theme(plot.title = element_text(hjust = 0.5,size = rel(2.25)),legend.text=element_text(size=rel(2)),legend.key.size=unit(2,"line"))

AIANplot2010

AIANplot2015<-ggplot()+
  geom_polygon(data=mapping2015,aes(x=long,y=lat,group=group,fill=Value.Rate),color='black',alpha=.8,size=.3)+
  #scale_fill_gradient2(name="",limits=limits,low="navy", mid="white", high="red")+
  scale_fill_gradient(low="#FFFFFF",high = "#8b0000",)+
  coord_map()+
  theme_nothing(base_size=12, legend=T)+
  ggtitle("AI/AN Rate Map 2015")+theme(plot.title = element_text(hjust = 0.5,size = rel(2.25)),legend.text=element_text(size=rel(2)),legend.key.size=unit(2,"line"))
#AIANplot2015


AIANplot2020<-ggplot()+
  geom_polygon(data=mapping2020,aes(x=long,y=lat,group=group,fill=Value.Rate),color='black',alpha=.8,size=.3)+
  #scale_fill_gradient2(name="",limits=limits,low="navy", mid="white", high="red")+
  scale_fill_gradient(low="#FFFFFF",high = "#8b0000",)+
  coord_map()+
  theme_nothing(base_size=12, legend=T)+
  ggtitle("AI/AN Rate Map 2020")+theme(plot.title = element_text(hjust = 0.5,size = rel(2.25)),legend.text=element_text(size=rel(2)),legend.key.size=unit(2,"line"))
#AIANplot2020

AIANplot2010

AIANgraph<-grid.arrange(AIANplot2010,AIANplot2015,AIANplot2020,nrow=3)

##Asian Data
Asian2010<-read.csv("https://raw.githubusercontent.com/jasonh0509/SummerRA/main/Asian2010.csv")
Asian2015<-read.csv("https://raw.githubusercontent.com/jasonh0509/SummerRA/main/Asian2015.csv")
Asian2020<-read.csv("https://raw.githubusercontent.com/jasonh0509/SummerRA/main/Asian2020.csv")

totaldataAsian<-rbind(Asian2010,Asian2015,Asian2020)
#Rate.de<-subset(totaldataAsian,select = Rate.Denom)
totaldataAsian<-totaldataAsian%>%
  mutate(Rate.Denom=gsub(",","",Rate.Denom),
         Rate.Denom=as.numeric(Rate.Denom),
         Value.Rate=((Value.Count)/(Rate.Denom))*1000)

#Asian Merge and map file

graphtotalAsian<-subset(totaldataAsian,select = c(Place,Year,Geoid,Rate.Denom,Value.Count,Value.Rate))
graphtotalAsian$FIPS=graphtotalAsian$Geoid-37000

ncMap.mergeAsian<-merge(ncMap,graphtotalAsian,by.x="id2",by.y="FIPS")

Asianmapping2010<-ncMap.mergeAsian[which(ncMap.mergeAsian$Year==2010),]
Asianmapping2015<-ncMap.mergeAsian[which(ncMap.mergeAsian$Year==2015),]
Asianmapping2020<-ncMap.mergeAsian[which(ncMap.mergeAsian$Year==2020),]


b<-c(0,1.5,3)
limits = c(min(Asian2010$Value.Rate),max(Asian2010$Value.Rate))


#Asian Graph
Asianplot2010<-ggplot()+
  geom_polygon(data=Asianmapping2010,aes(x=long,y=lat,group=group,fill=Value.Rate),color='black',alpha=.8,size=.3)+
  #scale_fill_gradient2(name="",limits=limits,low="navy", mid="white", high="red")+
  scale_fill_gradient(low="#FFFFFF",high = "#8b0000")+
  coord_map()+
  theme_nothing(base_size=12, legend=T)+
  ggtitle("Asian Rate Map 2010")+theme(plot.title = element_text(hjust = 0.5,size = rel(2.25)),legend.text=element_text(size=rel(2)),legend.key.size=unit(2,"line"))
Asianplot2010

Asianplot2015<-ggplot()+
  geom_polygon(data=Asianmapping2015,aes(x=long,y=lat,group=group,fill=Value.Rate),color='black',alpha=.8,size=.3)+
  #scale_fill_gradient2(name="",limits=limits,low="navy", mid="white", high="red")+
  scale_fill_gradient(low="#FFFFFF",high = "#8b0000")+
  coord_map()+
  theme_nothing(base_size=12, legend=T)+
  ggtitle("Asian Rate Map 2015")+theme(plot.title = element_text(hjust = 0.5,size = rel(2.25)),legend.text=element_text(size=rel(2)),legend.key.size=unit(2,"line"))
Asianplot2015

Asianplot2020<-ggplot()+
  geom_polygon(data=Asianmapping2020,aes(x=long,y=lat,group=group,fill=Value.Rate),color='black',alpha=.8,size=.3)+
  #scale_fill_gradient2(name="",limits=limits,low="navy", mid="white", high="red")+
  scale_fill_gradient(low="#FFFFFF",high = "#8b0000")+
  coord_map()+
  theme_nothing(base_size=12, legend=T)+
  ggtitle("Asian Rate Map 2020")+theme(plot.title = element_text(hjust = 0.5,size = rel(2.5)),legend.text=element_text(size=rel(2)),legend.key.size=unit(2,"line"))
Asianplot2020

AsianGraph<-grid.arrange(Asianplot2010,Asianplot2015,Asianplot2020,nrow=3)

##Hispanic Data

Hisp2010<-read.csv("https://raw.githubusercontent.com/jasonh0509/SummerRA/main/HISP2010.csv")
Hisp2015<-read.csv("https://raw.githubusercontent.com/jasonh0509/SummerRA/main/HISP2015.csv")
Hisp2020<-read.csv("https://raw.githubusercontent.com/jasonh0509/SummerRA/main/HISP2020.csv")


##Black Data

Black2010<-read.csv("https://raw.githubusercontent.com/jasonh0509/SummerRA/main/AA2010.csv")
Black2015<-read.csv("https://raw.githubusercontent.com/jasonh0509/SummerRA/main/AA2015.csv")
Black2020<-read.csv("https://raw.githubusercontent.com/jasonh0509/SummerRA/main/AA2020.csv")

##White Data

White2010<-read.csv("https://raw.githubusercontent.com/jasonh0509/SummerRA/main/White2010.csv")
White2015<-read.csv("https://raw.githubusercontent.com/jasonh0509/SummerRA/main/White2015.csv")
White2020<-read.csv("https://raw.githubusercontent.com/jasonh0509/SummerRA/main/White2020.csv")

