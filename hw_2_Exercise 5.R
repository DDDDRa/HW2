#Exercise 5
## Question 1: Compute the marginal eﬀect of X on Y according to the probit and logit models.
#Install and use the package numDeriv
#install.packages("numDeriv")
library(numDeriv)
#Construct a matrix and put X variables inside
X<-as.matrix(cbind(1,X1,X2,X3))
#Construct a matrix and put ydum and X variables inside
DATA5<-data.frame(cbind(ydum,X1,X2,X3))
#Construct a function to get marginal effect of probit model
ME_probit<-function(beta.,X.){
  Y_H<-X.%*%beta.
  pdf<-dnorm(Y_H)
  ME<-pdf%*%t(beta.)
  return(ME)
}
ME_p<-ME_probit(probit_belta5,X)
write.csv(ME_p,file="marginaleffect_probit")

#Construct a function to get marginal effect of logit model
ME_logit<-function(beta.,X.){
  Y_H<-X.%*%beta.
  pdf<-(exp(Y_H)/((1+exp(Y_H))^2))
  ME<-pdf%*%t(beta.)
  return(ME)
}
ME_l<-ME_logit(logit_belta5,X)
write.csv(ME_l,file="marginaleffect_logit")

##Calculate the standard deviation of variouse marginal effects
###By using delta method
#Calculate the variance-covariance matrix
myprobit<-glm(ydum~X1+X2+X3,family = binomial(link = "probit"),data=DATA5)
var_probit<-vcov(myprobit)
mylogit<-glm(ydum~X1+X2+X3,family = binomial,data=DATA5)
var_logit<-vcov(mylogit)


# Calculate the first order derivative of marginal effect in probit model (at mean level)

#Calculate the marginal effect function of probit model
xmean=apply(X,2,mean)
MEmean_probit<-function(beta.,x.=xmean){
  Y_H<-x.%*%beta.
  pdf<-dnorm(Y_H)
  MEmean<-pdf%*%t(beta.)
  return(MEmean)
}
me_derivative_pro<-jacobian(MEmean_probit,probit_belta5)

#Calculate the marginal effect function of probit model
MEmean_logit<-function(beta.,x.=xmean){
  Y_H<-x.%*%beta.
  pdf<-(exp(Y_H))/((1+exp(Y_H))^2)
  MEmean<-pdf%*%t(beta.)
  return(MEmean)
}
me_derivative_log<-jacobian(MEmean_logit,logit_belta5)
#Calculate the standard errors of marginal effect of probit model
sd_me_pro<-me_derivative_pro*var_probit*t(me_derivative_pro)
sdv_me_pro<-sqrt(diag(sd_me_pro))
#Calculate the standard errors of marginal effect of logit model
sd_me_log<-me_derivative_log*var_logit*t(me_derivative_log)
sdv_me_log<-sqrt(diag(sd_me_log))

###By using bootstrap
num5=499
pgn<-matrix(0,nrow=10000,ncol=4)
DATA<-as.matrix(cbind(ydum,X1,X2,X3))
sdboot1<-matrix(0,nrow=num5,ncol=4)
Xmean<-matrix(0,nrow=num5,ncol=4)
#Calculate the marginal effect function of logit model
ME_LOGIT<-matrix(0,nrow=num5,ncol=4)
for (j in 1:num5){
  for (i in 1:10000){
    k<-sample(1:nrow(DATA),1)
    sp<-as.matrix(DATA[k,])
    pgn[i,]<-t(sp)
  }
  Xloop<-as.matrix(cbind(1,pgn[,2],pgn[,3],pgn[,4]))
  X_mean<-apply(Xloop,2,mean)
  Xmean[j,]<-as.matrix(X_mean)
  ME_p<-(exp(Xmean[j,]%*%logit_belta5)/((1+exp(Xmean[j,]%*%logit_belta5))^2))%*%t(logit_belta5)
  ME_p<-as.matrix(ME_p)
  ME_LOGIT[j,]<-ME_p
}
#Calculate the standard errors of the logit model
sdvboot_log<-apply(ME_LOGIT,2,sd)
#Calculate the marginal effect function of probit model
ME_PROBIT<-matrix(0,nrow=num5,ncol=4)
for (j in 1:num5){
  for (i in 1:10000){
    k<-sample(1:nrow(DATA),1)
    sp<-as.matrix(DATA[k,])
    pgn[i,]<-t(sp)
  }
  Xloop<-as.matrix(cbind(1,pgn[,2],pgn[,3],pgn[,4]))
  X_mean<-apply(Xloop,2,mean)
  Xmean[j,]<-as.matrix(X_mean)
  ME_p<-pnorm(Xmean[j,]%*%probit_belta5)%*%t(probit_belta5)
  ME_p<-as.matrix(ME_p)
  ME_PROBIT[j,]<-ME_p
}
#Calculate the standard errors of the probit model
sdvboot_pro<-apply(ME_PROBIT,2,sd)
