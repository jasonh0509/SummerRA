install.packages("faraway")
install.packages()
library(faraway)
library(ggplot2)

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
###Based on the histogram of c, the common distribution of any entry c_i in c is F distribution

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

###The proportion of values in c that are less than k1 is 0.023. The proportion of values in c that are les than k2 is 0.026

##f
u<-c[1:100]/c[501:600]
head(u)
hist(u)
