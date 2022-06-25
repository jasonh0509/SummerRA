#### load the packages required
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


data=read.csv("https://raw.githubusercontent.com/jasonh0509/SummerRA/main/DataForJason.csv?token=GHSAT0AAAAAABVDRYPINSL5AVYH7HFCNTCEYVFEODA",header = TRUE)
nationaldata<-read.csv("https://raw.githubusercontent.com/jasonh0509/SummerRA/main/Death%20Rate%20All%20counties.csv")


#national_rate$national_rate=nationaldata$Crude.Rate
#data$FIPS = data$countyFIPS-39000


#Read in Ohio county shape file
#get shapes
mapfile=readOGR(dsn='Shapes',layer='cb_2014_us_county_500k')

#convert to data frame
usa.map.data=fortify(mapfile,region='COUNTYFP')
usa.map.data$id2=as.numeric(usa.map.data$id)
#usa.map.data.merge=merge(usa.map.data,nationaldata)


##subseting year 2018
#mappingdata = oh.map.data.merge[which(oh.map.data.merge$Year==2018),]
##which command identify what rows have year 2018

limits = c(min(nationalRate$X2019.Age.adjusted.Rate),max(nationalRate$X2019.Age.adjusted.Rate))

plot2018<-ggplot()+
  geom_polygon(data=usa.map.data.merge,aes(x=long,y=lat,group=group,fill=death.rate),color='black',alpha=.8,size=.3)+
  scale_fill_gradient2(name="",limits=limits,low='red',high='blue')+
  coord_map()+
  theme_nothing(base_size=12, legend=T)+
  ggtitle("2018")+theme(plot.title = element_text(hjust = 0.5,size = rel(2.25)),legend.text=element_text(size=rel(2)),legend.key.size=unit(2,"line"))

##Subset Year 2007
mapping2007=oh.map.data.merge[which(oh.map.data.merge$Year==2007),]
limits2=c(min(data$death.rate),max(data$death.rate))

plot2007<-ggplot()+
  geom_polygon(data=mapping2007,aes(x=long,y=lat,group=group,fill=death.rate),color='black',alpha=.8,size=.3)+
  scale_fill_gradient2(name="",limits=limits,low='',high='red')+
  coord_map()+
  theme_nothing(legend=T)+
  ggtitle("2007")+theme(plot.title = element_text(hjust = 0.5,size = rel(2.25)),legend.text=element_text(size=rel(2)),legend.key.size=unit(2,"line"))


##Subset Hamilton county in OH
HamiltonMap<-subset(mappingdata,County=="Hamilton")

plot2018+
  geom_point(aes(x=-84.5120,y=39.1031))+
  geom_point(aes(x=-81.6944,y=41.4993))+
  geom_point(aes(x=-82.9988,y=39.9612))+
  geom_text(aes(-84.5120,39),label="Cincinnati",color="black")+
  geom_text(aes(-81.6944,41.4),label="Cleveland",color="black")+
  geom_text(aes(-82.9988,39.85),label="Columbus",color="black")

min(oh.map.data.merge$Year)