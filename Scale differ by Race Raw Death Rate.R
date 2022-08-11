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
totaldataAIAN<-read.csv("https://raw.githubusercontent.com/jasonh0509/SummerRA/main/NC%20AIAN.csv")


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


RawAIANplot2010<-ggplot()+
  geom_polygon(data=Rawmapping2010,aes(x=long,y=lat,group=group,fill=Value.Rate),color='black',alpha=.8,size=.3)+
  #scale_fill_gradient2(name="",limits=limits,low="navy", mid="white", high="red")+
  scale_fill_gradient(low="#FFFFFF",high = "#8b0000",limits=c(0,204),
                      breaks=c(0,102,204))+
  coord_map()+
  theme_nothing(base_size=12, legend=T)+
  ggtitle("AI/AN Raw Death Rate Map 2010")+theme(plot.title = element_text(hjust = 0.5,size = rel(2.25)),legend.text=element_text(size=rel(2)),legend.key.size=unit(2,"line"))
#ggsave(path = "C:/RGit/SummerRA/Raw Map Scale Different by Race","AIANplotRaw2010.png",width = 7, height =7)

RawAIANplot2015<-ggplot()+
  geom_polygon(data=Rawmapping2015,aes(x=long,y=lat,group=group,fill=Value.Rate),color='black',alpha=.8,size=.3)+
  #scale_fill_gradient2(name="",limits=limits,low="navy", mid="white", high="red")+
  scale_fill_gradient(low="#FFFFFF",high = "#8b0000",limits=c(0,204),
                      breaks=c(0,102,204))+
  coord_map()+
  theme_nothing(base_size=12, legend=T)+
  ggtitle("AI/AN Raw Death Rate Map 2015")+theme(plot.title = element_text(hjust = 0.5,size = rel(2.25)),legend.text=element_text(size=rel(2)),legend.key.size=unit(2,"line"))
#ggsave(path = "C:/RGit/SummerRA/Raw Map Scale Different by Race","AIANplotRaw2015.png",width = 7, height =7)



RawAIANplot2020<-ggplot()+
  geom_polygon(data=Rawmapping2020,aes(x=long,y=lat,group=group,fill=Value.Rate),color='black',alpha=.8,size=.3)+
  #scale_fill_gradient2(name="",limits=limits,low="navy", mid="white", high="red")+
  scale_fill_gradient(low="#FFFFFF",high = "#8b0000",limits=c(0,204),
                      breaks=c(0,102,204))+
  coord_map()+
  theme_nothing(base_size=12, legend=T)+
  ggtitle("AI/AN Raw Death Rate Map 2020")+theme(plot.title = element_text(hjust = 0.5,size = rel(2.25)),legend.text=element_text(size=rel(2)),legend.key.size=unit(2,"line"))
#ggsave(path = "C:/RGit/SummerRA/Raw Map Scale Different by Race","AIANplotRaw2020.png",width = 7, height =7)



##Asian Map

###Asian Data
Asian2010Raw<-read.csv("https://raw.githubusercontent.com/jasonh0509/SummerRA/main/Asian2010.csv")
Asian2015Raw<-read.csv("https://raw.githubusercontent.com/jasonh0509/SummerRA/main/Asian2015.csv")
Asian2020Raw<-read.csv("https://raw.githubusercontent.com/jasonh0509/SummerRA/main/Asian2020.csv")

RawtotaldataAsian<-rbind(Asian2010Raw,Asian2015Raw,Asian2020Raw)


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
  scale_fill_gradient(low="#FFFFFF",high = "#8b0000",limits=c(0,334),
                      breaks=c(0,172,334))+
  coord_map()+
  theme_nothing(base_size=12, legend=T)+
  ggtitle("Asian Raw Death Rate Map 2010")+theme(plot.title = element_text(hjust = 0.5,size = rel(2.25)),legend.text=element_text(size=rel(2)),legend.key.size=unit(2,"line"))
#ggsave(path = "C:/RGit/SummerRA/Raw Map Scale Different by Race","RawAsianplot2010.png",width = 7, height =7)



Asianplot2015.Raw<-ggplot()+
  geom_polygon(data=Asianmapping2015.Raw,aes(x=long,y=lat,group=group,fill=Value.Rate),color='black',alpha=.8,size=.3)+
  #scale_fill_gradient2(name="",limits=limits,low="navy", mid="white", high="red")+
  scale_fill_gradient(low="#FFFFFF",high = "#8b0000",limits=c(0,334),
                      breaks=c(0,172,334))+
  coord_map()+
  theme_nothing(base_size=12, legend=T)+
  ggtitle("Asian Raw Death Rate Map 2015")+theme(plot.title = element_text(hjust = 0.5,size = rel(2.25)),legend.text=element_text(size=rel(2)),legend.key.size=unit(2,"line"))
#ggsave(path = "C:/RGit/SummerRA/Raw Map Scale Different by Race","RawAsianplot2015.png",width = 7, height =7)

Asianplot2020.Raw<-ggplot()+
  geom_polygon(data=Asianmapping2020.Raw,aes(x=long,y=lat,group=group,fill=Value.Rate),color='black',alpha=.8,size=.3)+
  #scale_fill_gradient2(name="",limits=limits,low="navy", mid="white", high="red")+
  scale_fill_gradient(low="#FFFFFF",high = "#8b0000",limits=c(0,334),
                      breaks=c(0,172,334))+
  coord_map()+
  theme_nothing(base_size=12, legend=T)+
  ggtitle("Asian Raw Death Rate Map 2020")+theme(plot.title = element_text(hjust = 0.5,size = rel(2.25)),legend.text=element_text(size=rel(2)),legend.key.size=unit(2,"line"))
#ggsave(path = "C:/RGit/SummerRA/Raw Map Scale Different by Race","RawAsianplot2020.png",width = 7, height =7)

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
  scale_fill_gradient(low="#FFFFFF",high = "#8b0000",limits=c(0,78),
                      breaks=c(0,39,78))+
  coord_map()+
  theme_nothing(base_size=12, legend=T)+
  ggtitle("Hispanic Raw Death Rate Map 2010")+theme(plot.title = element_text(hjust = 0.5,size = rel(2.25)),legend.text=element_text(size=rel(2)),legend.key.size=unit(2,"line"))
#ggsave(path = "C:/RGit/SummerRA/Raw Map Scale Different by Race","RawHispanicplot2010.png",width = 7, height =7)



Hispplot2015.Raw<-ggplot()+
  geom_polygon(data=Hispmapping2015.Raw,aes(x=long,y=lat,group=group,fill=Value.Rate),color='black',alpha=.8,size=.3)+
  #scale_fill_gradient2(name="",limits=limits,low="navy", mid="white", high="red")+
  scale_fill_gradient(low="#FFFFFF",high = "#8b0000",limits=c(0,78),
                      breaks=c(0,39,78))+
  coord_map()+
  theme_nothing(base_size=12, legend=T)+
  ggtitle("Hispanic Raw Death Rate Map 2015")+theme(plot.title = element_text(hjust = 0.5,size = rel(2.25)),legend.text=element_text(size=rel(2)),legend.key.size=unit(2,"line"))
#ggsave(path = "C:/RGit/SummerRA/Raw Map Scale Different by Race","RawHispanicplot2015.png",width = 7, height =7)


Hispplot2020.Raw<-ggplot()+
  geom_polygon(data=Hispmapping2020.Raw,aes(x=long,y=lat,group=group,fill=Value.Rate),color='black',alpha=.8,size=.3)+
  #scale_fill_gradient2(name="",limits=limits,low="navy", mid="white", high="red")+
  scale_fill_gradient(low="#FFFFFF",high = "#8b0000",limits=c(0,78),
                      breaks=c(0,39,78))+
  coord_map()+
  theme_nothing(base_size=12, legend=T)+
  ggtitle("Hispanic Raw Death Rate Map 2020")+theme(plot.title = element_text(hjust = 0.5,size = rel(2.25)),legend.text=element_text(size=rel(2)),legend.key.size=unit(2,"line"))
#ggsave(path = "C:/RGit/SummerRA/Raw Map Scale Different by Race","RawHispanicplot2020.png",width = 7, height =7)


##Black Data

Black2010<-read.csv("https://raw.githubusercontent.com/jasonh0509/SummerRA/main/AA2010.csv")
Black2015<-read.csv("https://raw.githubusercontent.com/jasonh0509/SummerRA/main/AA2015.csv")
Black2020<-read.csv("https://raw.githubusercontent.com/jasonh0509/SummerRA/main/AA2020.csv")

totaldatablk<-rbind(Black2010,Black2015,Black2020)


graphtotalBlack.Raw<-subset(totaldatablk,select = c(Place,Year,Geoid,Rate.Denom,Value.Count,Value.Rate))
graphtotalBlack.Raw$FIPS=graphtotalBlack.Raw$Geoid-37000

ncMap.mergeBlack.raw<-merge(ncMap,graphtotalBlack.Raw,by.x="id2",by.y="FIPS")

Blackmapping2010.Raw<-ncMap.mergeBlack.raw[which(ncMap.mergeBlack.raw$Year==2010),]
Blackmapping2015.Raw<-ncMap.mergeBlack.raw[which(ncMap.mergeBlack.raw$Year==2015),]
Blackmapping2020.Raw<-ncMap.mergeBlack.raw[which(ncMap.mergeBlack.raw$Year==2020),]

Blackplot2010.Raw<-ggplot()+
  geom_polygon(data=Blackmapping2010.Raw,aes(x=long,y=lat,group=group,fill=Value.Rate),color='black',alpha=.8,size=.3)+
  #scale_fill_gradient2(name="",limits=limits,low="navy", mid="white", high="red")+
  scale_fill_gradient(low="#FFFFFF",high = "#8b0000",limits=c(0,112),
                      breaks=c(0,56,112))+
  coord_map()+
  theme_nothing(base_size=12, legend=T)+
  ggtitle("Black Raw Death Rate Map 2010")+theme(plot.title = element_text(hjust = 0.5,size = rel(2.25)),legend.text=element_text(size=rel(2)),legend.key.size=unit(2,"line"))
#ggsave(path = "C:/RGit/SummerRA/Raw Map Scale Different by Race","RawBlackplot2010.png",width = 7, height =7)

Blackplot2015.Raw<-ggplot()+
  geom_polygon(data=Blackmapping2015.Raw,aes(x=long,y=lat,group=group,fill=Value.Rate),color='black',alpha=.8,size=.3)+
  #scale_fill_gradient2(name="",limits=limits,low="navy", mid="white", high="red")+
  scale_fill_gradient(low="#FFFFFF",high = "#8b0000",limits=c(0,112),
                      breaks=c(0,56,112))+
  coord_map()+
  theme_nothing(base_size=12, legend=T)+
  ggtitle("Black Raw Death Rate Map 2015")+theme(plot.title = element_text(hjust = 0.5,size = rel(2.25)),legend.text=element_text(size=rel(2)),legend.key.size=unit(2,"line"))
#ggsave(path = "C:/RGit/SummerRA/Raw Map Scale Different by Race","RawBlackplot2015.png",width = 7, height =7)
##
Blackplot2020.Raw<-ggplot()+
  geom_polygon(data=Blackmapping2020.Raw,aes(x=long,y=lat,group=group,fill=Value.Rate),color='black',alpha=.8,size=.3)+
  #scale_fill_gradient2(name="",limits=limits,low="navy", mid="white", high="red")+
  scale_fill_gradient(low="#FFFFFF",high = "#8b0000",limits=c(0,112),
                      breaks=c(0,56,112))+
  coord_map()+
  theme_nothing(base_size=12, legend=T)+
  ggtitle("Black Raw Death Rate Map 2020")+theme(plot.title = element_text(hjust = 0.5,size = rel(2.25)),legend.text=element_text(size=rel(2)),legend.key.size=unit(2,"line"))
#ggsave(path = "C:/RGit/SummerRA/Raw Map Scale Different by Race","RawBlackplot2020.png",width = 7, height =7)

##White Data

White2010<-read.csv("https://raw.githubusercontent.com/jasonh0509/SummerRA/main/White2010.csv")
White2015<-read.csv("https://raw.githubusercontent.com/jasonh0509/SummerRA/main/White2015.csv")
White2020<-read.csv("https://raw.githubusercontent.com/jasonh0509/SummerRA/main/White2020.csv")

totalDataWhite<-rbind(White2010,White2015,White2020)



graphtotalWhite.Raw<-subset(totalDataWhite,select = c(Place,Year,Geoid,Rate.Denom,Value.Count,Value.Rate))
graphtotalWhite.Raw$FIPS=graphtotalWhite.Raw$Geoid-37000

ncMap.mergeWhite.raw<-merge(ncMap,graphtotalWhite.Raw,by.x="id2",by.y="FIPS")

Whitemapping2010.Raw<-ncMap.mergeWhite.raw[which(ncMap.mergeWhite.raw$Year==2010),]
Whitemapping2015.Raw<-ncMap.mergeWhite.raw[which(ncMap.mergeWhite.raw$Year==2015),]
Whitemapping2020.Raw<-ncMap.mergeWhite.raw[which(ncMap.mergeWhite.raw$Year==2020),]

Whiteplot2010.Raw<-ggplot()+
  geom_polygon(data=Whitemapping2010.Raw,aes(x=long,y=lat,group=group,fill=Value.Rate),color='black',alpha=.8,size=.3)+
  #scale_fill_gradient2(name="",limits=limits,low="navy", mid="white", high="red")+
  scale_fill_gradient(low="#FFFFFF",high = "#8b0000",limits=c(0,70),
                      breaks=c(0,35,70))+
  coord_map()+
  theme_nothing(base_size=12, legend=T)+
  ggtitle("White Raw Death Rate Map 2010")+theme(plot.title = element_text(hjust = 0.5,size = rel(2.25)),legend.text=element_text(size=rel(2)),legend.key.size=unit(2,"line"))
#ggsave(path = "C:/RGit/SummerRA/Raw Map Scale Different by Race","RawWhiteplot2010.png",width = 7, height =7)

Whiteplot2015.Raw<-ggplot()+
  geom_polygon(data=Whitemapping2015.Raw,aes(x=long,y=lat,group=group,fill=Value.Rate),color='black',alpha=.8,size=.3)+
  #scale_fill_gradient2(name="",limits=limits,low="navy", mid="white", high="red")+
  scale_fill_gradient(low="#FFFFFF",high = "#8b0000",limits=c(0,70),
                      breaks=c(0,35,70))+
  coord_map()+
  theme_nothing(base_size=12, legend=T)+
  ggtitle("White Raw Death Rate Map 2015")+theme(plot.title = element_text(hjust = 0.5,size = rel(2.25)),legend.text=element_text(size=rel(2)),legend.key.size=unit(2,"line"))
#ggsave(path = "C:/RGit/SummerRA/Raw Map Scale Different by Race","RawWhiteplot2015.png",width = 7, height =7)

Whiteplot2020.Raw<-ggplot()+
  geom_polygon(data=Whitemapping2020.Raw,aes(x=long,y=lat,group=group,fill=Value.Rate),color='black',alpha=.8,size=.3)+
  #scale_fill_gradient2(name="",limits=limits,low="navy", mid="white", high="red")+
  scale_fill_gradient(low="#FFFFFF",high = "#8b0000",limits=c(0,70),
                      breaks=c(0,35,70))+
  coord_map()+
  theme_nothing(base_size=12, legend=T)+
  ggtitle("White Raw Death Rate Map 2020")+theme(plot.title = element_text(hjust = 0.5,size = rel(2.25)),legend.text=element_text(size=rel(2)),legend.key.size=unit(2,"line"))
#ggsave(path = "C:/RGit/SummerRA/Raw Map Scale Different by Race","RawWhiteplot2020.png",width = 7, height =7)

NameList <- list(
  AIAN_data=totaldataAIAN,
  Asian_data=RawtotaldataAsian,
  Black_data=totaldatablk,
  Hispanic_data=totaldatablk,
  White_data=totalDataWhite
)
for(i in names(NameList)){
  write.csv(NameList[[i]], paste0(i,".csv"))
}

SelectedFileList<-list(
  AIAN_selected=graphtotal,
  Asian_selected=RawtotaldataAsian,
  Black_selected=graphtotalBlack.Raw,
  Hispanic_selected=totaldataHisp.Raw,
  White_selected=graphtotalWhite.Raw
)

for(i in names(SelectedFileList)){
  write.csv(SelectedFileList[[i]], paste0(i,".csv"))
}

CombinedFileList<-list(
  AIAN_mapCombined=ncMap.merge,
  Asian_mapCombined=ncMap.mergeAsianRaw,
  Black_mapCombined=ncMap.mergeBlack.raw,
  Hispanic_mapCombined=ncMap.mergeHisp.raw,
  White_mapCombined=ncMap.mergeWhite.raw
)

for(i in names(CombinedFileList)){
  write.csv(CombinedFileList[[i]], paste0(i,".csv"))
}

FinalData_used_for_drawing<-list(
  AIAN_mapping2010=Rawmapping2010,
  AIAN_mapping2015=Rawmapping2015,
  AIAN_mapping2020=Rawmapping2020,
  Whitemapping2010.Raw=Whitemapping2010.Raw,
  Whitemapping2015.Raw=Whitemapping2015.Raw,
  Whitemapping2020.Raw=Whitemapping2020.Raw,
  Asianmapping2010.Raw=Asianmapping2010.Raw,
  Asianmapping2015.Raw=Asianmapping2015.Raw,
  Asianmapping2020.Raw=Asianmapping2020.Raw,
  Hispmapping2010.Raw=Hispmapping2010.Raw,
  Hispmapping2015.Raw=Hispmapping2015.Raw,
  Hispmapping2020.Raw=Hispmapping2020.Raw,
  Blackmapping2010.Raw=Blackmapping2010.Raw,
  Blackmapping2015.Raw=Blackmapping2015.Raw,
  Blackmapping2020.Raw=Blackmapping2020.Raw
  
  
)
for(i in names(FinalData_used_for_drawing)){
  write.csv(FinalData_used_for_drawing[[i]], paste0(i,".csv"))
}

