#Exercise1
set.seed(100)
#Construct x1
X1<-vector()
X1<-runif(10000,min=1,max=3)
#Construct x2
X2<-vector()
X2<-rgamma(10000, 3, scale = 2)
#Construct x3
X3<-vector()
X3<-rbinom(10000,1, 0.3)

#Construct eps
eps<-vector()
eps<-rnorm(10000,mean=2,sd=1)
#Construct Y by using X1,X2,X3 and eps
Y<-0.5+ 1.2*X1 + -0.9*X2 + 0.1*X3 + eps
#Construct ydum
ydum<-vector()
for (i in Y){
  if (i>mean(Y)){
    ydum<-c(ydum,1)
  }
  else{
    ydum<-c(ydum,0)
  }
}