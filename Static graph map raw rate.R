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

#AI/AN

##AI/AN Data


##Aggregate 3 AI/AN data sets into one 
totaldataAIAN.Raw<-read.csv("https://raw.githubusercontent.com/jasonh0509/SummerRA/main/NC%20AIAN.csv")

##Select columns needed and change FIPS code into desired form for creating map
graphtotalAIAN.Raw<-subset(totaldataAIAN.Raw,select = c(Place,Year,Geoid,Rate.Denom,Value.Count,Value.Rate))
graphtotalAIAN.Raw$FIPS=graphtotalAIAN.Raw$Geoid-37000

##Processing map file and merge it with the data file
ncMap=fortify(mapG[mapG$STATEFP=='37',],region='COUNTYFP')
ncMap$id2=as.numeric(ncMap$id)
ncMap.merge=merge(ncMap,graphtotalAIAN.Raw,by.x="id2",by.y="FIPS")

##AI/AN 2010, 2015,2020 data  for mapping 
AIAN.Mapping2010.Raw<-ncMap.merge[which(ncMap.merge$Year==2010),]
AIAN.Mapping2015.Raw<-ncMap.merge[which(ncMap.merge$Year==2015),]
AIAN.Mapping2020.Raw<-ncMap.merge[which(ncMap.merge$Year==2020),]

##Creating maps of AI/AN opioid related death rate of 2010,2015,2020
RawAIANplot2010<-ggplot()+
  geom_polygon(data=AIAN.Mapping2010.Raw,aes(x=long,y=lat,group=group,fill=Value.Rate),color='black',alpha=.8,size=.3)+
  #scale_fill_gradient2(name="",limits=limits,low="navy", mid="white", high="red")+
  scale_fill_gradient(low="#FFFFFF",high = "#8b0000",limits=c(0,340),
                      breaks=c(0,170,340))+
  coord_map()+
  theme_nothing(base_size=12, legend=T)+
  ggtitle("AI/AN Raw Death Rate Map 2010")+theme(plot.title = element_text(hjust = 0.5,size = rel(2.25)),legend.text=element_text(size=rel(2)),legend.key.size=unit(2,"line"))
#RawAIANplot2010
#ggsave(path = "C:/RGit/SummerRA/Raw Map/AIAN","RawAIANplot2010.png",width = 7, height = 7)

RawAIANplot2015<-ggplot()+
  geom_polygon(data=AIAN.Mapping2015.Raw,aes(x=long,y=lat,group=group,fill=Value.Rate),color='black',alpha=.8,size=.3)+
  #scale_fill_gradient2(name="",limits=limits,low="navy", mid="white", high="red")+
  scale_fill_gradient(low="#FFFFFF",high = "#8b0000",limits=c(0,340),
                      breaks=c(0,170,340))+
  coord_map()+
  theme_nothing(base_size=12, legend=T)+
  ggtitle("AI/AN Raw Death Rate Map 2015")+theme(plot.title = element_text(hjust = 0.5,size = rel(2.25)),legend.text=element_text(size=rel(2)),legend.key.size=unit(2,"line"))
#ggsave(path = "C:/RGit/SummerRA/Raw Map/AIAN","RawAIANplot2015.png",width = 7, height = 7)

RawAIANplot2020<-ggplot()+
  geom_polygon(data=AIAN.Mapping2020.Raw,aes(x=long,y=lat,group=group,fill=Value.Rate),color='black',alpha=.8,size=.3)+
  #scale_fill_gradient2(name="",limits=limits,low="navy", mid="white", high="red")+
  scale_fill_gradient(low="#FFFFFF",high = "#8b0000",limits=c(0,340),
                      breaks=c(0,170,340))+
  coord_map()+
  theme_nothing(base_size=12, legend=T)+
  ggtitle("AI/AN Raw Death Rate Map 2020")+theme(plot.title = element_text(hjust = 0.5,size = rel(2.25)),legend.text=element_text(size=rel(2)),legend.key.size=unit(2,"line"))
#ggsave(path = "C:/RGit/SummerRA/Raw Map/AIAN","RawAIANplot2020.png",width = 7, height =7)


##Asian

##Asian Data
Asian2010Raw<-read.csv("https://raw.githubusercontent.com/jasonh0509/SummerRA/main/Asian2010.csv")
Asian2015Raw<-read.csv("https://raw.githubusercontent.com/jasonh0509/SummerRA/main/Asian2015.csv")
Asian2020Raw<-read.csv("https://raw.githubusercontent.com/jasonh0509/SummerRA/main/Asian2020.csv")

totaldataAsian.Raw<-rbind(Asian2010Raw,Asian2015Raw,Asian2020Raw)#bind 3 dataset into one


#Asian map and data merge

graphtotalAsian.Raw<-subset(totaldataAsian.Raw,select = c(Place,Year,Geoid,Rate.Denom,Value.Count,Value.Rate))
graphtotalAsian.Raw$FIPS=graphtotalAsian.Raw$Geoid-37000

ncMap.mergeAsianRaw<-merge(ncMap,graphtotalAsian.Raw,by.x="id2",by.y="FIPS")

##Asian 2010, 2015, 2020 data for mapping 
Asian.mapping2010.Raw<-ncMap.mergeAsianRaw[which(ncMap.mergeAsianRaw$Year==2010),]
Asian.mapping2015.Raw<-ncMap.mergeAsianRaw[which(ncMap.mergeAsianRaw$Year==2015),]
Asian.mapping2020.Raw<-ncMap.mergeAsianRaw[which(ncMap.mergeAsianRaw$Year==2020),]

##Creating Asian's map of opioid related death rates
Asianplot2010.Raw<-ggplot()+
  geom_polygon(data=Asian.mapping2010.Raw,aes(x=long,y=lat,group=group,fill=Value.Rate),color='black',alpha=.8,size=.3)+
  scale_fill_gradient(low="#FFFFFF",high = "#8b0000",limits=c(0,340),
                      breaks=c(0,170,340))+
  coord_map()+
  theme_nothing(base_size=12, legend=T)+
  ggtitle("Asian Raw Death Rate Map 2010")+theme(plot.title = element_text(hjust = 0.5,size = rel(2.25)),legend.text=element_text(size=rel(2)),legend.key.size=unit(2,"line"))
#ggsave(path = "C:/RGit/SummerRA/Raw Map","RawAsianplot2010.png",width = 7, height =7)
#Asianplot2010.Raw

Asianplot2015.Raw<-ggplot()+
  geom_polygon(data=Asianmapping2015.Raw,aes(x=long,y=lat,group=group,fill=Value.Rate),color='black',alpha=.8,size=.3)+
  scale_fill_gradient(low="#FFFFFF",high = "#8b0000",limits=c(0,340),
                      breaks=c(0,170,340))+
  coord_map()+
  theme_nothing(base_size=12, legend=T)+
  ggtitle("Asian Raw Death Rate Map 2015")+theme(plot.title = element_text(hjust = 0.5,size = rel(2.25)),legend.text=element_text(size=rel(2)),legend.key.size=unit(2,"line"))
#ggsave(path = "C:/RGit/SummerRA/Raw Map","RawAsianplot2015.png",width = 7, height =7)

Asianplot2020.Raw<-ggplot()+
  geom_polygon(data=Asianmapping2020.Raw,aes(x=long,y=lat,group=group,fill=Value.Rate),color='black',alpha=.8,size=.3)+
  scale_fill_gradient(low="#FFFFFF",high = "#8b0000",limits=c(0,340),
                      breaks=c(0,170,340))+
  coord_map()+
  theme_nothing(base_size=12, legend=T)+
  ggtitle("Asian Raw Death Rate Map 2020")+theme(plot.title = element_text(hjust = 0.5,size = rel(2.25)),legend.text=element_text(size=rel(2)),legend.key.size=unit(2,"line"))
#ggsave(path = "C:/RGit/SummerRA/Raw Map","RawAsianplot2020.png",width = 7, height =7)

#Hispanic

## Hispanic 2010, 2015 and 2020 data
Hisp2010.Raw<-read.csv("https://raw.githubusercontent.com/jasonh0509/SummerRA/main/HISP2010.csv")
Hisp2015.Raw<-read.csv("https://raw.githubusercontent.com/jasonh0509/SummerRA/main/HISP2015.csv")
Hisp2020.Raw<-read.csv("https://raw.githubusercontent.com/jasonh0509/SummerRA/main/HISP2020.csv")

totaldataHisp.Raw<-rbind(Hisp2010.Raw,Hisp2015.Raw,Hisp2020.Raw)#Combine 3 data set into 1

##Select columns needed and pre-treat Geoid/FIPS into desired form
graphtotalHisp.Raw<-subset(totaldataHisp.Raw,select = c(Place,Year,Geoid,Rate.Denom,Value.Count,Value.Rate))
graphtotalHisp.Raw$FIPS=graphtotalHisp.Raw$Geoid-37000

ncMap.mergeHisp.raw<-merge(ncMap,graphtotalHisp.Raw,by.x="id2",by.y="FIPS")

##Mapping data of hispanic in 2010, 2015 and 2020
Hispmapping2010.Raw<-ncMap.mergeHisp.raw[which(ncMap.mergeHisp.raw$Year==2010),]
Hispmapping2015.Raw<-ncMap.mergeHisp.raw[which(ncMap.mergeHisp.raw$Year==2015),]
Hispmapping2020.Raw<-ncMap.mergeHisp.raw[which(ncMap.mergeHisp.raw$Year==2020),]

##Creating maps of opioid related death of hispanic population in NC(2010, 2015, 2020)
Hispplot2010.Raw<-ggplot()+
  geom_polygon(data=Hispmapping2010.Raw,aes(x=long,y=lat,group=group,fill=Value.Rate),color='black',alpha=.8,size=.3)+
  #scale_fill_gradient2(name="",limits=limits,low="navy", mid="white", high="red")+
  scale_fill_gradient(low="#FFFFFF",high = "#8b0000",limits=c(0,340),
                      breaks=c(0,170,340))+
  coord_map()+
  theme_nothing(base_size=12, legend=T)+
  ggtitle("Hispanic Raw Death Rate Map 2010")+theme(plot.title = element_text(hjust = 0.5,size = rel(2.25)),legend.text=element_text(size=rel(2)),legend.key.size=unit(2,"line"))
ggsave(path = "C:/RGit/SummerRA/Raw Map","RawHispanicplot2010.png",width = 7, height =7)

Hispplot2015.Raw<-ggplot()+
  geom_polygon(data=Hispmapping2015.Raw,aes(x=long,y=lat,group=group,fill=Value.Rate),color='black',alpha=.8,size=.3)+
  #scale_fill_gradient2(name="",limits=limits,low="navy", mid="white", high="red")+
  scale_fill_gradient(low="#FFFFFF",high = "#8b0000",limits=c(0,340),
                      breaks=c(0,170,340))+
  coord_map()+
  theme_nothing(base_size=12, legend=T)+
  ggtitle("Hispanic Raw Death Rate Map 2015")+theme(plot.title = element_text(hjust = 0.5,size = rel(2.25)),legend.text=element_text(size=rel(2)),legend.key.size=unit(2,"line"))
ggsave(path = "C:/RGit/SummerRA/Raw Map","RawHispanicplot2015.png",width = 7, height =7)

Hispplot2020.Raw<-ggplot()+
  geom_polygon(data=Hispmapping2020.Raw,aes(x=long,y=lat,group=group,fill=Value.Rate),color='black',alpha=.8,size=.3)+
  #scale_fill_gradient2(name="",limits=limits,low="navy", mid="white", high="red")+
  scale_fill_gradient(low="#FFFFFF",high = "#8b0000",limits=c(0,340),
                      breaks=c(0,170,340))+
  coord_map()+
  theme_nothing(base_size=12, legend=T)+
  ggtitle("Hispanic Raw Death Rate Map 2020")+theme(plot.title = element_text(hjust = 0.5,size = rel(2.25)),legend.text=element_text(size=rel(2)),legend.key.size=unit(2,"line"))
ggsave(path = "C:/RGit/SummerRA/Raw Map","RawHispanicplot2020.png",width = 7, height =7)


#Black

##Black data
Black2010<-read.csv("https://raw.githubusercontent.com/jasonh0509/SummerRA/main/AA2010.csv")
Black2015<-read.csv("https://raw.githubusercontent.com/jasonh0509/SummerRA/main/AA2015.csv")
Black2020<-read.csv("https://raw.githubusercontent.com/jasonh0509/SummerRA/main/AA2020.csv")

totaldatablk.Raw<-rbind(Black2010,Black2015,Black2020)#Combine 3 data set into one 

##Selected required column modify the Geoid into needed form 
graphtotalBlack.Raw<-subset(totaldatablk.Raw,select = c(Place,Year,Geoid,Rate.Denom,Value.Count,Value.Rate))
graphtotalBlack.Raw$FIPS=graphtotalBlack.Raw$Geoid-37000

##Merge map file with data file 
ncMap.mergeBlack.raw<-merge(ncMap,graphtotalBlack.Raw,by.x="id2",by.y="FIPS")

##Data frame for mapping (Black)
Blackmapping2010.Raw<-ncMap.mergeBlack.raw[which(ncMap.mergeBlack.raw$Year==2010),]
Blackmapping2015.Raw<-ncMap.mergeBlack.raw[which(ncMap.mergeBlack.raw$Year==2015),]
Blackmapping2020.Raw<-ncMap.mergeBlack.raw[which(ncMap.mergeBlack.raw$Year==2020),]

##Creating maps for black population (2010,2015, 2020)
Blackplot2010.Raw<-ggplot()+
  geom_polygon(data=Blackmapping2010.Raw,aes(x=long,y=lat,group=group,fill=Value.Rate),color='black',alpha=.8,size=.3)+
  scale_fill_gradient(low="#FFFFFF",high = "#8b0000",limits=c(0,340),
                      breaks=c(0,170,340))+
  coord_map()+
  theme_nothing(base_size=12, legend=T)+
  ggtitle("Black Raw Death Rate Map 2010")+theme(plot.title = element_text(hjust = 0.5,size = rel(2.25)),legend.text=element_text(size=rel(2)),legend.key.size=unit(2,"line"))
#ggsave(path = "C:/RGit/SummerRA/Raw Map","RawBlackplot2010.png",width = 7, height =7)

Blackplot2015.Raw<-ggplot()+
  geom_polygon(data=Blackmapping2015.Raw,aes(x=long,y=lat,group=group,fill=Value.Rate),color='black',alpha=.8,size=.3)+
  scale_fill_gradient(low="#FFFFFF",high = "#8b0000",limits=c(0,340),
                      breaks=c(0,170,340))+
  coord_map()+
  theme_nothing(base_size=12, legend=T)+
  ggtitle("Black Raw Death Rate Map 2015")+theme(plot.title = element_text(hjust = 0.5,size = rel(2.25)),legend.text=element_text(size=rel(2)),legend.key.size=unit(2,"line"))
#ggsave(path = "C:/RGit/SummerRA/Raw Map","RawBlackplot2015.png",width = 7, height =7)
Blackplot2015.Raw
Blackplot2020.Raw<-ggplot()+
  geom_polygon(data=Blackmapping2020.Raw,aes(x=long,y=lat,group=group,fill=Value.Rate),color='black',alpha=.8,size=.3)+
  scale_fill_gradient(low="#FFFFFF",high = "#8b0000",limits=c(0,340),
                      breaks=c(0,170,340))+
  coord_map()+
  theme_nothing(base_size=12, legend=T)+
  ggtitle("Black Raw Death Rate Map 2020")+theme(plot.title = element_text(hjust = 0.5,size = rel(2.25)),legend.text=element_text(size=rel(2)),legend.key.size=unit(2,"line"))
#ggsave(path = "C:/RGit/SummerRA/Raw Map","RawBlackplot2020.png",width = 7, height =7)

#White Data
##White data from 2020,2015,2020
White2010<-read.csv("https://raw.githubusercontent.com/jasonh0509/SummerRA/main/White2010.csv")
White2015<-read.csv("https://raw.githubusercontent.com/jasonh0509/SummerRA/main/White2015.csv")
White2020<-read.csv("https://raw.githubusercontent.com/jasonh0509/SummerRA/main/White2020.csv")

totalDataWhite.Raw<-rbind(White2010,White2015,White2020)#Row bind data

graphtotalWhite.Raw<-subset(totalDataWhite.Raw,select = c(Place,Year,Geoid,Rate.Denom,Value.Count,Value.Rate))
graphtotalWhite.Raw$FIPS=graphtotalWhite.Raw$Geoid-37000

ncMap.mergeWhite.raw<-merge(ncMap,graphtotalWhite.Raw,by.x="id2",by.y="FIPS")

Whitemapping2010.Raw<-ncMap.mergeWhite.raw[which(ncMap.mergeWhite.raw$Year==2010),]
Whitemapping2015.Raw<-ncMap.mergeWhite.raw[which(ncMap.mergeWhite.raw$Year==2015),]
Whitemapping2020.Raw<-ncMap.mergeWhite.raw[which(ncMap.mergeWhite.raw$Year==2020),]

Whiteplot2010.Raw<-ggplot()+
  geom_polygon(data=Whitemapping2010.Raw,aes(x=long,y=lat,group=group,fill=Value.Rate),color='black',alpha=.8,size=.3)+
  scale_fill_gradient(low="#FFFFFF",high = "#8b0000",limits=c(0,340),
                      breaks=c(0,170,340))+
  coord_map()+
  theme_nothing(base_size=12, legend=T)+
  ggtitle("White Raw Death Rate Map 2010")+theme(plot.title = element_text(hjust = 0.5,size = rel(2.25)),legend.text=element_text(size=rel(2)),legend.key.size=unit(2,"line"))
#ggsave(path = "C:/RGit/SummerRA/Raw Map","RawWhiteplot2010.png",width = 7, height =7)

Whiteplot2015.Raw<-ggplot()+
  geom_polygon(data=Whitemapping2015.Raw,aes(x=long,y=lat,group=group,fill=Value.Rate),color='black',alpha=.8,size=.3)+
  scale_fill_gradient(low="#FFFFFF",high = "#8b0000",limits=c(0,340),
                      breaks=c(0,170,340))+
  coord_map()+
  theme_nothing(base_size=12, legend=T)+
  ggtitle("White Raw Death Rate Map 2015")+theme(plot.title = element_text(hjust = 0.5,size = rel(2.25)),legend.text=element_text(size=rel(2)),legend.key.size=unit(2,"line"))
#ggsave(path = "C:/RGit/SummerRA/Raw Map","RawWhiteplot2015.png",width = 7, height =7)

Whiteplot2020.Raw<-ggplot()+
  geom_polygon(data=Whitemapping2020.Raw,aes(x=long,y=lat,group=group,fill=Value.Rate),color='black',alpha=.8,size=.3)+
  scale_fill_gradient(low="#FFFFFF",high = "#8b0000",limits=c(0,340),
                      breaks=c(0,170,340))+
  coord_map()+
  theme_nothing(base_size=12, legend=T)+
  ggtitle("White Raw Death Rate Map 2020")+theme(plot.title = element_text(hjust = 0.5,size = rel(2.25)),legend.text=element_text(size=rel(2)),legend.key.size=unit(2,"line"))
#ggsave(path = "C:/RGit/SummerRA/Raw Map","RawWhiteplot2020.png",width = 7, height =7)




