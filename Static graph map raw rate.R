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
#totaldataAIAN<-totaldataAIAN%>%
#  mutate(Value.Rate=((Value.Rate)))


graphtotal<-subset(totaldataAIAN,select = c(Place,Year,Geoid,Rate.Denom,Value.Count,Value.Rate))
graphtotal$FIPS=graphtotal$Geoid-37000


ncMap=fortify(mapG[mapG$STATEFP=='37',],region='COUNTYFP')
ncMap$id2=as.numeric(ncMap$id)
ncMap.merge=merge(ncMap,graphtotal,by.x="id2",by.y="FIPS")

#2010 data 
Rawmapping2010<-ncMap.merge[which(ncMap.merge$Year==2010),]


#2015 data
Rawmapping2015<-ncMap.merge[which(ncMap.merge$Year==2015),]

#2020 data
Rawmapping2020<-ncMap.merge[which(ncMap.merge$Year==2020),]

#limits = c(min(NA2010$Value.Rate),max(NA2010$Value.Rate))

RawAIANplot2010<-ggplot()+
  geom_polygon(data=Rawmapping2010,aes(x=long,y=lat,group=group,fill=Value.Rate),color='black',alpha=.8,size=.3)+
  #scale_fill_gradient2(name="",limits=limits,low="navy", mid="white", high="red")+
  scale_fill_gradient(low="#FFFFFF",high = "#8b0000",limits=c(0,340),
                      breaks=c(0,170,340))+
  coord_map()+
  theme_nothing(base_size=12, legend=T)+
  ggtitle("AI/AN Raw Death Rate Map 2010")+theme(plot.title = element_text(hjust = 0.5,size = rel(2.25)),legend.text=element_text(size=rel(2)),legend.key.size=unit(2,"line"))
ggsave(path = "C:/RGit/SummerRA/Raw Map","RawAIANplot2010.png",width = 7, height = 7)

RawAIANplot2015<-ggplot()+
  geom_polygon(data=Rawmapping2015,aes(x=long,y=lat,group=group,fill=Value.Rate),color='black',alpha=.8,size=.3)+
  #scale_fill_gradient2(name="",limits=limits,low="navy", mid="white", high="red")+
  scale_fill_gradient(low="#FFFFFF",high = "#8b0000",limits=c(0,300),
                      breaks=c(0,150,300))+
  coord_map()+
  theme_nothing(base_size=12, legend=T)+
  ggtitle("AI/AN Raw Death Rate Map 2015")+theme(plot.title = element_text(hjust = 0.5,size = rel(2.25)),legend.text=element_text(size=rel(2)),legend.key.size=unit(2,"line"))
ggsave(path = "C:/RGit/SummerRA/Raw Map","RawAIANplot2015.png",width = 7, height = 7)



RawAIANplot2020<-ggplot()+
  geom_polygon(data=Rawmapping2020,aes(x=long,y=lat,group=group,fill=Value.Rate),color='black',alpha=.8,size=.3)+
  #scale_fill_gradient2(name="",limits=limits,low="navy", mid="white", high="red")+
  scale_fill_gradient(low="#FFFFFF",high = "#8b0000",limits=c(0,300),
                      breaks=c(0,150,300))+
  coord_map()+
  theme_nothing(base_size=12, legend=T)+
  ggtitle("AI/AN Death Rate Map 2020")+theme(plot.title = element_text(hjust = 0.5,size = rel(2.25)),legend.text=element_text(size=rel(2)),legend.key.size=unit(2,"line"))
ggsave(path = "C:/RGit/SummerRA/Raw Map","RawAIANplot2020.png",width = 7, height =7)


RawAIANplot2010
RawAIANplot2015
RawAIANplot2020

##Asian Map

###Asian Data
Asian2010Raw<-read.csv("https://raw.githubusercontent.com/jasonh0509/SummerRA/main/Asian2010.csv")
Asian2015Raw<-read.csv("https://raw.githubusercontent.com/jasonh0509/SummerRA/main/Asian2015.csv")
Asian2020Raw<-read.csv("https://raw.githubusercontent.com/jasonh0509/SummerRA/main/Asian2020.csv")

RawtotaldataAsian<-rbind(Asian2010Raw,Asian2015Raw,Asian2020Raw)
#Rate.de<-subset(totaldataAsian,select = Rate.Denom)


#Asian Merge and map file

RawgraphtotalAsian<-subset(RawtotaldataAsian,select = c(Place,Year,Geoid,Rate.Denom,Value.Count,Value.Rate))
RawgraphtotalAsian$FIPS=RawgraphtotalAsian$Geoid-37000

ncMap.mergeAsianRaw<-merge(ncMap,RawgraphtotalAsian,by.x="id2",by.y="FIPS")

Asianmapping2010.Raw<-ncMap.mergeAsianRaw[which(ncMap.mergeAsianRaw$Year==2010),]
Asianmapping2015.Raw<-ncMap.mergeAsianRaw[which(ncMap.mergeAsianRaw$Year==2015),]
Asianmapping2020.Raw<-ncMap.mergeAsianRaw[which(ncMap.mergeAsianRaw$Year==2020),]

Asianplot2010.Raw<-ggplot()+
  geom_polygon(data=Asianmapping2010.Raw,aes(x=long,y=lat,group=group,fill=Value.Rate),color='black',alpha=.8,size=.3)+
  #scale_fill_gradient2(name="",limits=limits,low="navy", mid="white", high="red")+
  scale_fill_gradient(low="#FFFFFF",high = "#8b0000",limits=c(0,340),
                      breaks=c(0,170,340))+
  coord_map()+
  theme_nothing(base_size=12, legend=T)+
  ggtitle("Asian Raw Death Rate Map 2010")+theme(plot.title = element_text(hjust = 0.5,size = rel(2.25)),legend.text=element_text(size=rel(2)),legend.key.size=unit(2,"line"))
ggsave(path = "C:/RGit/SummerRA/Raw Map","RawAsianplot2010.png",width = 7, height =7)

#Asianplot2010.Raw

Asianplot2015.Raw<-ggplot()+
  geom_polygon(data=Asianmapping2015.Raw,aes(x=long,y=lat,group=group,fill=Value.Rate),color='black',alpha=.8,size=.3)+
  #scale_fill_gradient2(name="",limits=limits,low="navy", mid="white", high="red")+
  scale_fill_gradient(low="#FFFFFF",high = "#8b0000",limits=c(0,300),
                      breaks=c(0,150,300))+
  coord_map()+
  theme_nothing(base_size=12, legend=T)+
  ggtitle("Asian Raw Death Rate Map 2015")+theme(plot.title = element_text(hjust = 0.5,size = rel(2.25)),legend.text=element_text(size=rel(2)),legend.key.size=unit(2,"line"))
Asianplot2015.Raw

Asianplot2020.Raw<-ggplot()+
  geom_polygon(data=Asianmapping2020.Raw,aes(x=long,y=lat,group=group,fill=Value.Rate),color='black',alpha=.8,size=.3)+
  #scale_fill_gradient2(name="",limits=limits,low="navy", mid="white", high="red")+
  scale_fill_gradient(low="#FFFFFF",high = "#8b0000",limits=c(0,300),
                      breaks=c(0,150,300))+
  coord_map()+
  theme_nothing(base_size=12, legend=T)+
  ggtitle("Asian Raw Death Rate Map 2020")+theme(plot.title = element_text(hjust = 0.5,size = rel(2.25)),legend.text=element_text(size=rel(2)),legend.key.size=unit(2,"line"))
Asianplot2020.Raw#This has a maxium cell over 3000

##Hispanic

Hisp2010.Raw<-read.csv("https://raw.githubusercontent.com/jasonh0509/SummerRA/main/HISP2010.csv")
Hisp2015.Raw<-read.csv("https://raw.githubusercontent.com/jasonh0509/SummerRA/main/HISP2015.csv")
Hisp2020.Raw<-read.csv("https://raw.githubusercontent.com/jasonh0509/SummerRA/main/HISP2020.csv")

totaldataHisp.Raw<-rbind(Hisp2010.Raw,Hisp2015.Raw,Hisp2020.Raw)
#Rate.de<-subset(totaldataAsian,select = Rate.Denom)

graphtotalHisp.Raw<-subset(totaldataHisp.Raw,select = c(Place,Year,Geoid,Rate.Denom,Value.Count,Value.Rate))
graphtotalHisp.Raw$FIPS=graphtotalHisp.Raw$Geoid-37000

ncMap.mergeHisp.raw<-merge(ncMap,graphtotalHisp.Raw,by.x="id2",by.y="FIPS")

Hispmapping2010.Raw<-ncMap.mergeHisp.raw[which(ncMap.mergeHisp.raw$Year==2010),]
Hispmapping2015.Raw<-ncMap.mergeHisp.raw[which(ncMap.mergeHisp.raw$Year==2015),]
Hispmapping2020.Raw<-ncMap.mergeHisp.raw[which(ncMap.mergeHisp.raw$Year==2020),]

Hispplot2010.Raw<-ggplot()+
  geom_polygon(data=Hispmapping2010.Raw,aes(x=long,y=lat,group=group,fill=Value.Rate),color='black',alpha=.8,size=.3)+
  #scale_fill_gradient2(name="",limits=limits,low="navy", mid="white", high="red")+
  scale_fill_gradient(low="#FFFFFF",high = "#8b0000",limits=c(0,340),
                      breaks=c(0,170,340))+
  coord_map()+
  theme_nothing(base_size=12, legend=T)+
  ggtitle("Hispanic Death Rate Map 2010")+theme(plot.title = element_text(hjust = 0.5,size = rel(2.25)),legend.text=element_text(size=rel(2)),legend.key.size=unit(2,"line"))
ggsave(path = "C:/RGit/SummerRA/Raw Map","RawHispanicplot2010.png",width = 7, height =7)

#Hispplot2010.Raw

Hispplot2015.Raw<-ggplot()+
  geom_polygon(data=Hispmapping2015.Raw,aes(x=long,y=lat,group=group,fill=Value.Rate),color='black',alpha=.8,size=.3)+
  #scale_fill_gradient2(name="",limits=limits,low="navy", mid="white", high="red")+
  scale_fill_gradient(low="#FFFFFF",high = "#8b0000",limits=c(0,340),
                      breaks=c(0,170,340))+
  coord_map()+
  theme_nothing(base_size=12, legend=T)+
  ggtitle("Hispanic Death Rate Map 2015")+theme(plot.title = element_text(hjust = 0.5,size = rel(2.25)),legend.text=element_text(size=rel(2)),legend.key.size=unit(2,"line"))
Hispplot2015.Raw

Hispplot2020.Raw<-ggplot()+
  geom_polygon(data=Hispmapping2020.Raw,aes(x=long,y=lat,group=group,fill=Value.Rate),color='black',alpha=.8,size=.3)+
  #scale_fill_gradient2(name="",limits=limits,low="navy", mid="white", high="red")+
  scale_fill_gradient(low="#FFFFFF",high = "#8b0000",limits=c(0,340),
                      breaks=c(0,170,340))+
  coord_map()+
  theme_nothing(base_size=12, legend=T)+
  ggtitle("Hispanic Death Rate Map 2020")+theme(plot.title = element_text(hjust = 0.5,size = rel(2.25)),legend.text=element_text(size=rel(2)),legend.key.size=unit(2,"line"))
Hispplot2020.Raw


##Black Data

Black2010<-read.csv("https://raw.githubusercontent.com/jasonh0509/SummerRA/main/AA2010.csv")
Black2015<-read.csv("https://raw.githubusercontent.com/jasonh0509/SummerRA/main/AA2015.csv")
Black2020<-read.csv("https://raw.githubusercontent.com/jasonh0509/SummerRA/main/AA2020.csv")

totaldatablk<-rbind(Black2010,Black2015,Black2020)


##White Data

White2010<-read.csv("https://raw.githubusercontent.com/jasonh0509/SummerRA/main/White2010.csv")
White2015<-read.csv("https://raw.githubusercontent.com/jasonh0509/SummerRA/main/White2015.csv")
White2020<-read.csv("https://raw.githubusercontent.com/jasonh0509/SummerRA/main/White2020.csv")

totoalDataWhite<-rbind(White2010,White2015,White2020)

