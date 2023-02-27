#### load the packages required
library('rgeos')
library('spdep')
library('rgdal')
library('maptools')
library('shapefiles')
library('ggmap')
library('ggpubr')
library('stringr')
library('dplyr')
library('data.table')
library(ggplot2)

#### output Rda files for the counties and census tracts

data = fread('NC5yr2015.csv',header=TRUE)
### keep county,state,tract data
### columns for id and variables we want - use Nethery variables

yrs = 2009:2021

### note insurance doesn't start in 2009 so ignore that?
varlist = c("TotPop","TotPop_moe","Over65","Over65_moe","White1","pctWhite1","White1_moe","Over25","Over25_moe","Bachelors","pctBachelors","Bachelors_moe","Married","Married_moe","Over15","Over15_moe","EmployedCLF","EmployedCLF_moe","pctEmployedCLF","Over16","Over16_moe","MedianHHInc","MedianHHInc_moe","MedianGrossRent","MedianGrossRent_moe","TotHHS","TotHHS_moe","NotInsuredUnder65","NotInsuredUnder65_moe","FamHHS","FamHHS_moe","poor","poor_moe","UMPartnerHHsPerK","UMPartnerHHsPerK_moe" )
countydata5.0 = data[which(data$SumLev=="050"),c(3,6)]
n.cty = nrow(countydata5.0)
countydata5 = data.frame("Year" = kronecker(yrs,rep(1,n.cty)),"geoid" = rep(countydata5.0$geoid,length(yrs)),"geocode" = rep(countydata5.0$esriid,length(yrs)))

countydata1.0 = data[which(data$SumLev=="050"),c(2,4)] ### include all counties just leave blanks
countydata1 = data.frame("Year" = kronecker(yrs,rep(1,n.cty)),"geoid" = rep(countydata1.0$geoid,length(yrs)),"geocode" = rep(countydata1.0$geocode,length(yrs)))

statedata1 = data.frame("Year"=yrs,"geoid"=rep( data[which(data$geoid=="04000US37"),2],length(yrs)),"geocode" = rep( data[which(data$geoid=="04000US37"),4],length(yrs)))


### need a list of all unique tracts from any year?
tractdata5 = matrix(NA,ncol=2)
for(t in yrs){
  filename = paste("NC/NC_5year_",t,".csv",sep="")
  data = as.data.frame(fread(filename))
  colnames(data) = tolower(colnames(data))
  data = data[which(data$sumlev=="140"),]
  tractdata5 = rbind(tractdata5,matrix(c(rep(t,nrow(data)),data$geoid),nrow(data),2))
}
tractdata5 = tractdata5[-1,]
tractdata5 = as.data.frame(tractdata5)
colnames(tractdata5) = c("Year","geoid")

#tractdata5.0 = data[which(data$SumLev=="140"),c(2,4)]
#n.trct = nrow(tractdata5.0) ### caution because number of tracts changes over time
#tractdata5 = data.frame("Year" = kronecker(yrs,rep(1,n.trct)),"geoid" = rep(tractdata5.0$geoid,length(yrs)),"geocode" = rep(tractdata5.0$geocode,length(yrs)))

countydata5 = cbind(countydata5,matrix(NA,nrow(countydata5),length(varlist)))
colnames(countydata5) = c(colnames(countydata5)[1:3],varlist)
countydata1 = cbind(countydata1,matrix(NA,nrow(countydata1),length(varlist)))
colnames(countydata1) = c(colnames(countydata1)[1:3],varlist)
tractdata5 = cbind(tractdata5,matrix(NA,nrow(tractdata5),length(varlist)))
colnames(tractdata5) = c(colnames(tractdata5)[1:2],varlist)
statedata1 = cbind(statedata1,matrix(NA,nrow(statedata1),length(varlist)))
colnames(statedata1) = c(colnames(statedata1)[1:3],varlist)



################################################################
##### County 5-year data
###############################################################
for(t in 2009:2021){
    filename = paste("NC/NC_5year_",t,".csv",sep="")
    data = as.data.frame(fread(filename))
    colnames(data) = tolower(colnames(data))
    data = data[which(data$sumlev=="050"),]
    ### merge the variables in varlist
    II = which(tolower(varlist) %in% colnames(data))
    temp = full_join(countydata5[which(countydata5$Year==t),c(1,2)],data[,c("geoid",tolower(varlist[II]))],by=c("geoid"))
    countydata5[which(countydata5$Year==t),varlist[II]] = temp[,tolower(varlist[II])]
}



################################################################
##### County 1-year data
###############################################################
for(t in 2009:2021){
  if(t!=2020){ ### 2020 only has 5-year data
    filename = paste("NC/NC_1year_",t,".csv",sep="")
    data = as.data.frame(fread(filename))
    colnames(data) = tolower(colnames(data))
    data = data[which(data$sumlev=="050"),]
    ### merge the variables in varlist
    II = which(tolower(varlist) %in% colnames(data))
    temp = full_join(countydata1[which(countydata1$Year==t),c(1,2)],data[,c("geoid",tolower(varlist[II]))],by=c("geoid"))
    countydata1[which(countydata1$Year==t),varlist[II]] = temp[,tolower(varlist[II])]
  }
}

################################################################
##### Census Tract 5-year data
##### note tracts might change in decennial years
##### might need to add rows or even delete rows over time
###############################################################
for(t in 2009:2021){
  filename = paste("NC/NC_5year_",t,".csv",sep="")
  data = as.data.frame(fread(filename))
  colnames(data) = tolower(colnames(data))
  data = data[which(data$sumlev=="140"),]
  ### merge the variables in varlist
  II = which(tolower(varlist) %in% colnames(data))
  temp = full_join(tractdata5[which(tractdata5$Year==t),c(1,2)],data[,c("geoid",tolower(varlist[II]))],by=c("geoid"))
  tractdata5[which(tractdata5$Year==t),varlist[II]] = temp[,tolower(varlist[II])]
}


################################################################
##### State 1-year data
###############################################################
for(t in 2009:2021){
  if(t!=2020){ ### 2020 only has 5-year data
    filename = paste("NC/NC_1year_",t,".csv",sep="")
    data = as.data.frame(fread(filename))
    colnames(data) = tolower(colnames(data))
    data = data[which(data$geoid=="04000US37"),]
    ### merge the variables in varlist
    II = which(tolower(varlist) %in% colnames(data))
    temp = full_join(statedata1[which(statedata1$Year==t),c(1,2)],data[,c("geoid",tolower(varlist[II]))],by=c("geoid"))
    statedata1[which(statedata1$Year==t),varlist[II]] = temp[,tolower(varlist[II])]
  }
}

#### need to remove commas and $ and make numeric
for(p in 4:ncol(countydata5)){
  countydata5[,p] = as.numeric(gsub("[^0-9.-]", "", countydata5[,p]))
  countydata1[,p] = as.numeric(gsub("[^0-9.-]", "", countydata1[,p]))
  tractdata5[,(p-1)] = as.numeric(gsub("[^0-9.-]", "", tractdata5[,(p-1)]))
  statedata1[,p] = as.numeric(gsub("[^0-9.-]", "", statedata1[,p]))
}


save(countydata5,countydata1,tractdata5,statedata1,file="NC_ACS.Rda")


#### make county and census tract level maps and plots of some variables


load("NC_ACS.Rda")
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



