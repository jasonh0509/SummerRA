install.packages("faraway")
install.packages()
library(faraway)
library(ggplot2)
##Permcol
permcol<-
  function (X,y,Permute,n=1000,hist=TRUE,lm=FALSE)
  {
    X2<-X
    if (is.null(colnames(X2))){cn<-1:ncol(X)} else {cn<-colnames(X2)}
    cat("\n")
    cat("===========\n")
    cat(noquote(paste(c("Response (y): ",paste(cn[y],collapse=", "),"\n"),collapse="")))
    cat(noquote(paste(c("All: ",paste(cn,collapse=", "),"\n"),collapse="")))
    cat(noquote(paste(c("Keep (X2): ",paste(cn[-c(Permute,y)],collapse=", "),"\n"),collapse="")))
    cat(noquote(paste(c("Permute: ",paste(cn[Permute],collapse=", "),"\n"),collapse="")))
    X<-X2[,-c(Permute,y)];V<-X2[,Permute];y<-X2[,y]
    cat("-----------\n")
    if (is.null(X)) {X<-rep(1,length(y))}
    V<-as.matrix(cbind(V)); X<-as.matrix(cbind(X));
    if(dim(X)[2]==0) {X<-cbind(X,1)}
    D<-NULL
    for (i in 1:n)
    {
      v2<-(apply(V,2,sample)); ylm2<-lm(y~X+v2);
      D<-c(D,deviance(ylm2))
    }
    ylm<-lm(y~X+V); Dr<-deviance(ylm)
    D<-round(D,8);Dr<-round(Dr,8)
    if(hist){
      hist(D,main="Histogram of deviances")
      if (min(D)<Dr) rug(Dr,col=4,lwd=4)}
    cat(noquote(paste(c("Target Deviance: ",round(Dr,3),"\n"),collapse="")))
    cat("-----------\n")
    a<-length(D[D<Dr])+length(D[D==Dr])/2
    if ((a/n)==0) {
      cat(paste(c("Smaller deviances: ","None","\n"),collapse=""))
      cat(noquote(paste(c("Best/smallest: ", head(round(min(D),3)),"\n"),
                        collapse="")))
    } else {
      U1<-paste(round(head(sort(D[D<=Dr])),2),collapse=", ")
      #U2<-paste(round(tail(sort(D[D<=Dr])),2),collapse=", ")
      cat(paste(c("Smaller deviances: ",a,"\n"),collapse=""))
      cat(noquote(paste(c(U1,"\n"),collapse="")))
      cat("-----------\n")
      cat(noquote(paste(c("Proportion: ",a/n,"\n"),collapse="")))
    }
    cat("-----------\n")
    Keep<-X;Permute<-V
    lm1<-lm(y~Keep)
    ylm<-lm(y~Keep+Permute)
    if(lm) {print(summary(ylm))}
    print(anova(lm1,ylm))
    cat("===========\n")
  }

##Permcol table
permcol_table<-function (X,y,PermuteL=(1:ncol(X))[-y],n=1000)
{
  X2<-X;y2<-y
  if (is.null(colnames(X2))){cn<-1:ncol(X)} else {cn<-colnames(X2)}
  cat("\n")
  cat("===========\n")
  cat(noquote(paste(c("Response (y): ",paste(cn[y],collapse=", "),"\n"),collapse="")))
  cat(noquote(paste(c("All: ",paste(cn,collapse=", "),"\n"),collapse="")))
  cat(noquote(paste(c("Permute: ",paste(cn[PermuteL],collapse=", "),"\n"),collapse="")))
  ans<-NULL
  y<-X2[,y]
  for (Permute in PermuteL){
    ansv<-NULL
    X<-X2[,-c(Permute,y2)];V<-X2[,Permute]
    if (is.null(X)) {X<-rep(1,length(y))}
    V<-as.matrix(cbind(V)); X<-as.matrix(cbind(X));
    if(dim(X)[2]==0) {X<-cbind(X,1)}
    D<-NULL
    for (i in 1:n)
    {
      v2<-(apply(V,2,sample)); ylm2<-lm(y~X+v2);
      D<-c(D,deviance(ylm2))
    }
    ylm<-lm(y~X+V); Dr<-deviance(ylm)
    D<-round(D,8);Dr<-round(Dr,8)
    ansv<-c(ansv,round(Dr,3))
    a<-length(D[D<Dr])+length(D[D==Dr])/2
    ansv<-c(ansv,c(n,a,a/n))
    Keep<-X;Permute<-V
    lm1<-lm(y~Keep)
    ylm<-lm(y~Keep+Permute)
    an<-anova(lm1,ylm)
    ansv<-c(ansv,round(an$Pr[2],6),round(an$F[2],3))
    ans<-rbind(ans,ansv)
  }
  colnames(ans)<-c("Deviance","N","smaller","prop","PrF","F")
  rownames(ans)<-cn[PermuteL]
  ans
}


DataX<-data.frame(V1=c(1,1,1),V2=c(4,2,6))

#Q3
X<-cbind(DataX$V1,DataX$V2)
X_t<-t(X)

X_t%*%X

H_mtx<-X%*%solve(X_t%*%X)%*%X_t

round(H_mtx)
sum(diag(H_mtx))

h_eigen<-eigen(H_mtx)
E<-h_eigen$values
sum(h_eigen$values)
sum(diag(H_mtx))
h_eigen_vec<-h_eigen$vectors

DIAG<-matrix(c(1,0,0,0,1,0,0,0,0),nrow = 3,ncol = 3)

h_eigen_vec%*%DIAG%*%t(h_eigen_vec)


#c
H<-X%*%solve(t(X)%*%X)%*%t(X)
one<-cbind(rep(1,3))
H1<-one%*%solve(t(one)%*%one)%*%t(one)
I<-diag(3)
A1<-I-H
A2<-I-H1
A3<-H-H1

lapply(eigen(A1),round,4)
lapply(eigen(A2),round,4)
lapply(eigen(A3),round,4)

eigenA1<-eigen(A1)
eigenA2<-eigen(A2)
eigenA3<-eigen(A3)


#Q4
##a
set.seed(2264)
M<-matrix(rnorm(10000),1000,10)
head(M)

##b
c<-apply(M,1,function(v){sum(v^2)})
head(c)
hist(c)

##c
###Based on the histogram of c, the common distribution of any entry c_i in c is chi-square distribution

##d
k1<-qchisq(0.025,df=10)
k2<-qchisq(1-0.025,df=10)

###Thus, k1= 3.25, k2=20.48

##e
c[c<k1]
c[c>k2]
num1<-length(c[c<k1])
num2<-length(c[c>k2])
proportion1<-num1/1000
proportion2<-num2/1000

###The proportion of values in c that are less than k1 is 0.023. The proportion of values in c that are less than k2 is 0.026

##f
u<-c[1:100]/c[501:600]
head(u)
hist(u)

##g
### The common distribution of any entry in u is F distribution

##h
rho1<-qf(0.1,df1=10,df2=10)
rho1
rho2<-qf(0.9,df1=10,df2 = 10)
rho2
## Thus, rho1 = 0.431,rho2 = 2.323

##i
u[u<rho1]
u[u>rho2]
num_less_rho1<-length(u[u<rho1])
num_more_rho2<-length(u[u>rho2])
prop_rho1<-num_less_rho1/100
prop_rho2<-num_more_rho2/100


#Q5
##a
beta<-c(10,1:5,rep(0,5))
set.seed(2644);epss<-rnorm(1000,mean=0,sd=2)
y_star<-cbind(1,M)%*%beta+epss
head(y_star)

##b
lm_ystar<-lm(y_star~M)
summary(lm_ystar)

###Discuss

##c
set.seed(2644)
epsd<-sample(c(-3,-1,-1,0,0,1,4),1000,replace = TRUE)
y_dag<-cbind(1,M)%*%beta+epsd


##d

##e
lm_ydag<-lm(y_dag~M)
summary(lm_ydag)


#Q10
##a
data(prostate)
head(prostate)

lpsa_model<-lm(lpsa~.,data = prostate)
summary(lpsa_model)

permcol_table(X=prostate,y=9)

##Q11
###a
lm_no_above0.05<-lm(lpsa~lcavol+lweight+svi,data = prostate)

###b
summary(lpsa_model)
summary(lm_no_above0.05)
SSR<-deviance(lm_no_above0.05)-deviance(lpsa_model)
F_sta<-(SSR/(lm_no_above0.05$df.residual-lpsa_model$df.residual))/(deviance(lpsa_model)/(lpsa_model$df.residual))

anova(lm_no_above0.05,lpsa_model)
SSR<-deviance(lm_no_above0.05)-deviance(lpsa_model)
F_sta<-(SSR/(lm_no_above0.05$df.residual-lpsa_model$df.residual))/(deviance(lpsa_model)/(lpsa_model$df.residual))
prostate_sub<-prostate[,c(1,2,5,9)]
round(permcol_table(X=prostate_sub,y=4),digits = 4)
####Proportion of smaller deviance and given p value are identical

#Q12
