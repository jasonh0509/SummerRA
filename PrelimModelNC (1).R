######################################################################
##### Preliminary Model for Dynamic Social Vulnerability Index #######
######################################################################

library(nimble)
library(coda)

#### Only use data from 2010 - 2019 so that no changes of tracts
#### Use all count variables - start with just two variables, say poor and EmployedCLF
#### Only model 2014 - 2019 so can use 1-year data from 2010 - 2014 to help inform the later years

### Assume population of all tracts is known and equal to the 5-year values...


load("NC_ACS.Rda")
yrs = 2014:2019
yrs1 = 2010:2019

countydata1 = countydata1[which(countydata1$Year %in% yrs1),]
statedata1 = statedata1[which(statedata1$Year %in% yrs1),]
countydata5 = countydata5[which(countydata5$Year %in% yrs),]
tractdata5 = tractdata5[which(tractdata5$Year %in% yrs),]

#### note there are tracts that have no people in them??? Let's remove those from our data when we map results
#tractdata5 = tractdata5[-which(tractdata5$TotPop==0),]

### we should use unemployment instead of employment
countydata1$UnemployedCLF = countydata1$TotPop-countydata1$EmployedCLF
countydata5$UnemployedCLF = countydata5$TotPop-countydata5$EmployedCLF
tractdata5$UnemployedCLF = tractdata5$TotPop-tractdata5$EmployedCLF
statedata1$UnemployedCLF = statedata1$TotPop-statedata1$EmployedCLF



tractPop = tractdata5$TotPop
y.var = c("poor","UnemployedCLF")
K = length(y.var)

#### will need indices to map tracts to the counties they are in
tractdata5$County = as.factor(substr(tractdata5$geoid,8,12))
tractdata5$tractID = as.numeric(substr(tractdata5$geoid,8,23))


countydata5$CountyID = as.numeric(as.factor(countydata5$geocode))
countydata1 = merge(countydata1,countydata5[,c("geocode","CountyID","Year")],by=c("geocode","Year"),all=TRUE)
countydata1 = countydata1[order(countydata1$Year,countydata1$CountyID),]
tractdata5 = merge(tractdata5,countydata5[,c("geocode","CountyID","Year")],by.x=c("County","Year"),by.y=c("geocode","Year"))
tractdata5 = tractdata5[order(tractdata5$Year,tractdata5$CountyID),]
tractdata5$tractnum = tractdata5$tractID
tractdata5$tractID = as.numeric(as.factor(tractdata5$tractnum))
tractdata5 = tractdata5[order(tractdata5$Year,tractdata5$CountyID,tractdata5$tractID),]


#### make a matrix one row per county one column for tract within county
n.cty = max(countydata5$CountyID)
tractdata5.2010 = tractdata5[which(tractdata5$Year=="2014"),]
trctpercty = aggregate(tractdata5$tractID[which(tractdata5$Year==2014)],by=list(tractdata5$CountyID[which(tractdata5$Year==2014)]),FUN=length)
trctpercty = trctpercty$x

ctytract = matrix(NA,n.cty,max(trctpercty))
for(i in 1:n.cty){
  ctytract[i,1:trctpercty[i]]=unique(tractdata5$tractID[which(tractdata5$CountyID==i)])
}

### make the tract level data
y.T5 <- array(NA,c(n.cty,max(trctpercty),length(yrs),K))
Pop <- array(NA,c(n.cty,max(trctpercty),length(yrs)))
for(t in 1:length(yrs)){
  for(i in 1:n.cty){
    for(j in 1:trctpercty[i]){
      Ind = which(tractdata5$Year==yrs[t] & tractdata5$CountyID==i & tractdata5$tractID==ctytract[i,j])
      for(k in 1:K){
        y.T5[i,j,t,k] <- tractdata5[Ind,y.var[k]]
      }
      Pop[i,j,t] <- tractdata5[Ind,'TotPop']
    }
  }
}
### make the county-level 5-year data
y.C5 <- array(NA,c(n.cty,length(yrs),K))
for(t in 1:length(yrs)){
  for(i in 1:n.cty){
      Ind = which(countydata5$Year==yrs[t] & countydata5$CountyID==i)
      for(k in 1:K){
        y.C5[i,t,k] <- countydata5[Ind,y.var[k]]
      }
  }
}

# 
# NC.map=readOGR(dsn='../../../Shapes',layer='cb_2014_us_county_500k')
# NC.map = NC.map[NC.map$STATEFP=='37',]
# NC.map = NC.map[order(NC.map$COUNTYFP),]
# 
# nbmat = poly2nb(NC.map)
# A = matrix(0,n.cty,n.cty)
# for(i in 1:n.cty){
#   A[i,unlist(nbmat[[i]])]=1
# }

num<-colSums(A)
adj<-NULL
for(j in 1:n.cty){
  adj<-c(adj,which(A[j,]==1))
}
adj<-as.vector(adj)
num<-as.vector(num)
weights<-1+0*adj


model_code=nimbleCode({
  
  for(t in 1:T){ ### year
    for(k in 1:K){ ### model for each outcome variable
      for(i in 1:n.cty){
        for(j in 1:n.trct[i]){
          log(lambda5[i,j,t,k]) <- mu[k] + alpha[k]*f[i,j,t]
          mp[i,j,t,k] <- lambda5[i,j,t,k]/sum(lambda5[i,1:n.trct[i],t,k])
        }
        y.T5[i,1:n.trct[i],t,k] ~ dmulti(mp[i,1:n.trct[i],t,k], y.C5[i,t,k])
        y.C5[i,t,k] ~ dpois( sum(Pop[i,1:n.trct[i],t]*lambda5[i,1:n.trct[i],t,k]  )  )
      }
    }
  }
  
  alpha[1] <- 1
  for(k in 2:K){
    alpha[k] ~ dnorm(1,sd=5)
  }
  ### model for tract-level factor, centered at county-level factor
  for(t in 1:T){
    for(i in 1:n.cty){
      for(j in 1:n.trct[i]){
        f[i,j,t] ~ dnorm(f.cty[i,t]+mu.f[i,t],tau=psi[i])
       # f[i,j,t] ~ dnorm(0,tau=psi[i])
      }
     # f.cty[i,t] ~ dnorm(0,sd=10)
    } 
    f.cty[1:n.cty,t] ~ dcar_normal(adj[], weights[], num[], tau.fcty,zero_mean=1)
  }
  
 for(t in 1:1){
 for(i in 1:n.cty){
   mu.f[i,t] <- 0
 }
 }
 for(t in 2:T){
   for(i in 1:n.cty){
     mu.f[i,t] <- f.cty[i,t-1] 
   }
 }


  tau.fcty ~ dgamma(.5,.5)
  for(i in 1:n.cty){
    psi[i] ~ dgamma(.5,.5)
  }
  
  for(k in 1:K){
    mu[k] ~ dflat()
  }
  
  
  
})


mod_constants=list(K=dim(y.T5)[4],n.cty=n.cty,n.trct=trctpercty,adj=adj,weights=weights,num=num,T=length(yrs))
mod_data=list(y.T5=y.T5,y.C5=y.C5,Pop=Pop)

# Set initial values.

mod_inits=list(mu = c(-2,-1),f = array(0,c(n.cty,max(trctpercty),length(yrs))),f.cty = matrix(0,n.cty,length(yrs)),alpha=rep(1,K))
 
nimble_model <- nimbleModel(model_code, mod_constants,mod_data,mod_inits)
compiled_model <- compileNimble(nimble_model,resetFunctions = TRUE)

mcmc_conf <- configureMCMC(nimble_model,monitors=c('mu','f','alpha','f.cty'),control=list(adaptive=TRUE,adaptInterval=100,sliceWidths=5,sliceMaxSteps=1000,maxContractions=10000000,sliceAdaptWidthMaxIter=0,sliceAdaptFactorMaxIter=0),useConjugacy = TRUE)

nimble_mcmc<-buildMCMC(mcmc_conf)
compiled_mcmc<-compileNimble(nimble_mcmc, project = nimble_model,resetFunctions = TRUE)

MCS=100000
MCB=50000
MCT = 50
st = Sys.time()
samples=runMCMC(compiled_mcmc,inits=mod_inits,
                nchains = 1, nburnin=MCB,niter = MCS,samplesAsCodaMCMC = TRUE,thin=MCT,
                summary = FALSE, WAIC = FALSE,progressBar=TRUE) 
Sys.time()-st


save(samples,file='output.Rda')



if(0){
  
  
  load('output.Rda')
  M = dim(samples)[1]
  
  al = which(names(samples[1,])=="alpha[1]")
  au = al+K-1
  alpha.hat = colMeans(samples[,al:au])
  mul = which(names(samples[1,])=="mu[1]")
  muu = mul+K-1
  mu.hat = colMeans(samples[,mul:muu])
  
  fl = which(names(samples[1,])=="f[1, 1, 1]")
  fu = fl + n.cty*max(trctpercty)*length(yrs)-1
  f.hat = colMeans(samples[,fl:fu])
  f.hat = array(f.hat,c(n.cty,max(trctpercty),length(yrs)))
  
  f.ctyl = which(names(samples[1,])=="f.cty[1, 1]")
  f.ctyu = f.ctyl + n.cty*length(yrs)-1
  f.ctyhat = colMeans(samples[,f.ctyl:f.ctyu])
  f.ctyhat = matrix(f.ctyhat,n.cty,length(yrs))
  
  log.lambdahat = array(NA,c(n.cty,max(trctpercty),length(yrs),K))
  for(t in 1:length(yrs)){
    for(i in 1:n.cty){
      for(j in 1:trctpercty[i]){
        for(k in 1:K){
          log.lambdahat[i,j,t,k] <- mu.hat[k]+alpha.hat[k]*f.hat[i,j,t]
        }
      }
    }
  }
  
  
  nc.maptrct=readOGR(dsn='NC/Shapes',layer='tl_2015_37_tract')
  
  #convert to data frame
  nc.map.trct.data=fortify(nc.maptrct,region='GEOID')
  nc.map.trct.data$id2=as.numeric(nc.map.trct.data$id)
  
  log.lambda.mat = matrix(0,nrow(tractdata5),2)
  for(t in 1:length(yrs)){
    for(i in 1:n.cty){
      for(j in 1:trctpercty[i]){
        for(k in 1:K){
         log.lambda.mat[((t-1)*2195+ctytract[i,j]),k]=log.lambdahat[i,j,t,k] 
        }
      }
    }
  }
  
  f.mat = rep(0,nrow(tractdata5))
  for(t in 1:length(yrs)){
    for(i in 1:n.cty){
      for(j in 1:trctpercty[i]){
        f.mat[((t-1)*2195+ctytract[i,j])]=f.hat[i,j,t]
      }
    }
  }
  
  tractdata5$loglambda1 = log.lambda.mat[,1]
  tractdata5$loglambda2 = log.lambda.mat[,2]
  tractdata5$fhat = f.mat
  tractdata5$id2 = as.numeric(substr(tractdata5$geoid,8,23))
  
  ### remove the tracts that have 0 population
  tract2map = tractdata5[-which(tractdata5$TotPop==0),]
  
  nc.map.trct.data = merge(nc.map.trct.data,tractdata5,by="id2")
  nc.map.trct.data = nc.map.trct.data[-which(nc.map.trct.data$TotPop==0),]
  
  ll1map <- list()
  ll2map <- list()
  varmap1 <- list()
  varmap2 <- list()
  for(t in 2014:2019){
    data2map = nc.map.trct.data[which(nc.map.trct.data$Year==t),]
   # data2map = data2map[which(data2map$County==37067),]
    data2map = data2map[which(data2map$County==37183),]
    
    ll1map[[t-2013]]<- ggplot()+
      geom_polygon(data=data2map,aes(x=long,y=lat,group=group,fill=loglambda1),color='black',alpha=.8,size=.3)+
      scale_fill_gradient2(name="",low='blue',high='red')+
      coord_map()+
      theme_nothing(legend=T)+
      ggtitle(paste(t))+theme(plot.title = element_text(hjust = 0.5,size = rel(2.25),vjust=0.01),legend.text=element_text(size=rel(2)),legend.key.size=unit(4,"line"),legend.key.width=unit(1,"cm"))
    
    ll2map[[t-2013]]<- ggplot()+
      geom_polygon(data=data2map,aes(x=long,y=lat,group=group,fill=loglambda2),color='black',alpha=.8,size=.3)+
      scale_fill_gradient2(name="",low='blue',high='red')+
      coord_map()+
      theme_nothing(legend=T)+
      ggtitle(paste(t))+theme(plot.title = element_text(hjust = 0.5,size = rel(2.25),vjust=0.01),legend.text=element_text(size=rel(2)),legend.key.size=unit(4,"line"),legend.key.width=unit(1,"cm"))
    
    
    varmap1[[t-2013]]<- ggplot()+
      geom_polygon(data=data2map,aes(x=long,y=lat,group=group,fill=log(poor/TotPop+.1)),color='black',alpha=.8,size=.3)+
      scale_fill_gradient2(name="",low='blue',high='red')+
      coord_map()+
      theme_nothing(legend=T)+
      ggtitle(paste(t))+theme(plot.title = element_text(hjust = 0.5,size = rel(2.25),vjust=0.01),legend.text=element_text(size=rel(2)),legend.key.size=unit(4,"line"),legend.key.width=unit(1,"cm"))
    
    varmap2[[t-2013]]<- ggplot()+
      geom_polygon(data=data2map,aes(x=long,y=lat,group=group,fill=log(UnemployedCLF/TotPop)),color='black',alpha=.8,size=.3)+
      scale_fill_gradient2(name="",low='blue',high='red')+
      coord_map()+
      theme_nothing(legend=T)+
      ggtitle(paste(t))+theme(plot.title = element_text(hjust = 0.5,size = rel(2.25),vjust=0.01),legend.text=element_text(size=rel(2)),legend.key.size=unit(4,"line"),legend.key.width=unit(1,"cm"))
    
  }
  
  ggarrange(ll1map[[1]],ll1map[[2]],ll1map[[3]],ll1map[[4]],ll1map[[5]],varmap1[[1]],varmap1[[2]],varmap1[[3]],varmap1[[4]],varmap1[[5]],nrow=2,ncol=5)
  
  ggarrange(ll2map[[1]],ll2map[[2]],ll2map[[3]],ll2map[[4]],ll2map[[5]],varmap2[[1]],varmap2[[2]],varmap2[[3]],varmap2[[4]],varmap2[[5]],nrow=2,ncol=5)
  
  
  #### map the tract level factor
  fmap <- list()
  for(t in 2014:2019){
    data2map = nc.map.trct.data[which(nc.map.trct.data$Year==t),]
    #data2map = data2map[which(data2map$County==37067),]
    data2map = data2map[which(data2map$County==37183),]
    
    fmap[[t-2013]]<- ggplot()+
      geom_polygon(data=data2map,aes(x=long,y=lat,group=group,fill=fhat),color='black',alpha=.8,size=.3)+
      scale_fill_gradient2(name="",low='blue',high='red')+
      coord_map()+
      theme_nothing(legend=T)+
      ggtitle(paste(t))+theme(plot.title = element_text(hjust = 0.5,size = rel(2.25),vjust=0.01),legend.text=element_text(size=rel(2)),legend.key.size=unit(4,"line"),legend.key.width=unit(1,"cm"))
  }
  
  ggarrange(varmap1[[1]],varmap2[[1]],fmap[[1]],varmap1[[2]],varmap2[[2]],fmap[[2]],varmap1[[3]],varmap2[[3]],fmap[[3]],varmap1[[4]],varmap2[[4]],fmap[[4]],varmap1[[5]],varmap2[[5]],fmap[[5]],ncol=3,nrow=5)
  
  
  ##### map county level factor
  countydata5$fcty.hat = as.vector(f.ctyhat)
  countydata5$id2 = countydata5$geocode
  
  NCcty.map=readOGR(dsn='../../../Shapes',layer='cb_2014_us_county_500k')
  NCcty.map = NCcty.map[NCcty.map$STATEFP=='37',]
  NCcty.map = NCcty.map[order(NCcty.map$COUNTYFP),]
  
  nc.map.cty.data=fortify(NCcty.map,region='GEOID')
  nc.map.cty.data$id2=as.numeric(nc.map.cty.data$id)  
  
  nc.map.cty.data = merge(nc.map.cty.data,countydata5,by="id2")
  
  fmap <- list()
  for(t in 2014:2019){
    data2map = nc.map.cty.data[which(nc.map.cty.data$Year==t),]
    fmap[[t-2013]]<- ggplot()+
      geom_polygon(data=data2map,aes(x=long,y=lat,group=group,fill=fcty.hat),color='black',alpha=.8,size=.3)+
      scale_fill_gradient2(name="",low='blue',high='red')+
      coord_map()+
      theme_nothing(legend=T)+
      ggtitle(paste(t))+theme(plot.title = element_text(hjust = 0.5,size = rel(2.25),vjust=0.01),legend.text=element_text(size=rel(2)),legend.key.size=unit(4,"line"),legend.key.width=unit(1,"cm"))
  }
  
  varmap1<-list()
  varmap2 <- list()
  for(t in 2014:2019){
    data2map = nc.map.cty.data[which(nc.map.cty.data$Year==t),]
    varmap1[[t-2013]]<- ggplot()+
      geom_polygon(data=data2map,aes(x=long,y=lat,group=group,fill=log(poor/TotPop)),color='black',alpha=.8,size=.3)+
       scale_fill_gradient2(low='blue',high='red')+
      coord_map()+
      theme_nothing(legend=T)+
      ggtitle(paste(t))+theme(plot.title = element_text(hjust = 0.5,size = rel(2.25),vjust=0.01),legend.text=element_text(size=rel(2)),legend.key.size=unit(4,"line"),legend.key.width=unit(1,"cm"))
 
    varmap2[[t-2013]]<- ggplot()+
      geom_polygon(data=data2map,aes(x=long,y=lat,group=group,fill=log(EmployedCLF/TotPop)),color='black',alpha=.8,size=.3)+
      scale_fill_gradient2(low='blue',high='red')+
      coord_map()+
      theme_nothing(legend=T)+
      ggtitle(paste(t))+theme(plot.title = element_text(hjust = 0.5,size = rel(2.25),vjust=0.01),legend.text=element_text(size=rel(2)),legend.key.size=unit(4,"line"),legend.key.width=unit(1,"cm"))
    
   }
  
  ggarrange(varmap1[[1]],varmap2[[1]],fmap[[1]],varmap1[[2]],varmap2[[2]],fmap[[2]],varmap1[[3]],varmap2[[3]],fmap[[3]],varmap1[[4]],varmap2[[4]],fmap[[4]],varmap1[[5]],varmap2[[5]],fmap[[5]],ncol=3,nrow=5)
  
  
}

