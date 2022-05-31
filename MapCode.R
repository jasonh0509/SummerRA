
#### load the packages required
library('rgeos')
library('spdep')
library('rgdal')
library('maptools')
library('shapefiles')
library('ggmap')
library('ggpubr')
library('ggplot2')


data = read.csv('DataForJason.csv',header=TRUE)

data$death.rate = data$Deaths/data$Population*10000
data$FIPS = data$countyFIPS-39000


#Read in Ohio county shape file
#get shapes
oh.map=readOGR(dsn='Shapes',layer='cb_2014_us_county_500k')

#convert to data frame
oh.map.data=fortify(oh.map[oh.map$STATEFP=='39',],region='COUNTYFP')
oh.map.data$id2=as.numeric(oh.map.data$id)
oh.map.data.merge=merge(oh.map.data,data,by.x="id2",by.y="FIPS")

mappingdata = oh.map.data.merge[which(oh.map.data.merge$Year==2018),]

limits = c(min(data$death.rate),max(data$death.rate))

ggplot()+
  geom_polygon(data=mappingdata,aes(x=long,y=lat,group=group,fill=death.rate),color='black',alpha=.8,size=.3)+
  scale_fill_gradient2(name="",limits=limits,low='blue',high='red')+
  coord_map()+
  theme_nothing(legend=T)+
  ggtitle("2018")+theme(plot.title = element_text(hjust = 0.5,size = rel(2.25)),legend.text=element_text(size=rel(2)),legend.key.size=unit(2,"line"))


