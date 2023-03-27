#### load the packages required
library('rgeos')
library('spdep')
library('rgdal')
library('maptools')
library('shapefiles')
library('ggmap')
library('ggpubr')
library('stringr')
library(mapproj)
#### make county and census tract level maps of some variables


load("NC_ACS .Rda")
yrs = 2009:2021


#### plot variables over time to look for discrepancies
par(mfrow=c(2,1))
p=31
matplot(yrs,t(matrix(countydata5[,p],100,13)),type="l")
matplot(yrs,t(matrix(countydata1[,p],100,13)),type="l")
plot(yrs,statedata1[,p],type="l",ylab=colnames(statedata1)[p])

ggplot(data = countydata5,aes(x=Year,y=TotPop,group=geoid))+
  geom_line()




ggplot(data=tractdata5[which(tractdata5$Year>2009 & tractdata5$Year<2020),], aes(x=Year, y=poor/TotPop, group=geoid)) +
  geom_line()


nc.map10=readOGR(dsn='Shapes',layer='tl_2015_37_tract')

#convert to data frame
nc.map.data=fortify(nc.map10,region='GEOID')
nc.map.data$id2=as.numeric(nc.map.data$id)

ggplot()+
  geom_polygon(data=nc.map.data,aes(x=long,y=lat,group=group),color='black',alpha=.8,size=.3)+
  #scale_fill_gradient2(name="",limits=limits,midpoint=0,low='blue',high='red')+
  coord_map()+
  theme_nothing(legend=T)+
  ggtitle(paste(2013))+theme(plot.title = element_text(hjust = 0.5,size = rel(2.25),vjust=0.01),legend.text=element_text(size=rel(2)),legend.key.size=unit(4,"line"),legend.key.width=unit(1,"cm"))

tractdata5$County = as.numeric(substr(tractdata5$geoid,8,12))
#tractdata5$tractID = as.numeric(substr(tractdata5$geoid,13,18)) doesn't give unique tracts...
tractdata5$id2 = as.numeric(substr(tractdata5$geoid,8,23))

tract2015 = tractdata5[which(tractdata5$Year==2015),]

nc.map.data = merge(nc.map.data,tract2015[,c("id2","TotPop")],by="id2")
nc.map.data = merge(nc.map.data,tract2015[,c("id2","White1_moe")],by="id2")

ggplot()+
  geom_polygon(data=nc.map.data,aes(x=long,y=lat,group=group,fill=White1_moe),color="black",alpha=.8,size=.3)+
  scale_fill_gradient2(name="",limits=c(0,1300),midpoint=650,low='blue',high='red')+
  coord_map()+
  theme_nothing(legend=T)+
  ggtitle(paste(2015))+theme(plot.title = element_text(hjust = 0.5,size = rel(2.25),vjust=0.01),legend.text=element_text(size=rel(2)),legend.key.size=unit(4,"line"),legend.key.width=unit(1,"cm"))

tract2009 = tractdata5[which(tractdata5$Year==2009),]
nc.map.2009 = merge(nc.map.data,tract2009[,c("id2","Over65")],by="id2")
ggplot()+
  geom_polygon(data=nc.map.2009,aes(x=long,y=lat,group=group,fill=Over65),color='black',alpha=.8,size=.3)+
  #scale_fill_gradient2(name="",limits=limits,midpoint=0,low='blue',high='red')+
  coord_map()+
  theme_nothing(legend=T)+
  ggtitle(paste(2009))+theme(plot.title = element_text(hjust = 0.5,size = rel(2.25),vjust=0.01),legend.text=element_text(size=rel(2)),legend.key.size=unit(4,"line"),legend.key.width=unit(1,"cm"))

#### do count values of tracts sum to county values?

II = which(tractdata5$Year==2009 & tractdata5$County==37001)
sum(tractdata5$Over65[II])
countydata5$Over65[which(countydata5$Year==2009 & countydata5$geocode==37001)]



