# Exercise 4
# Construct a likelihood function of probit model
probit_likelihood<-function(belta,X. = X, ydum. = ydum){
  cdf<-pnorm(X.%*%belta)
  cdf[cdf>0.99999] <- 0.99999
  cdf[cdf<0.00001] <- 0.00001
  loglikelihood<--sum(ydum.*log(cdf))-sum((1-ydum.)*log(1-cdf))
  return (loglikelihood)
}

# Construct a likelihood function of logit model
logit_likelihood<-function(belta,X. = X, ydum. = ydum){
  cdf<-exp(X.%*%belta)/(1+exp(X.%*%belta))
  cdf[cdf>0.99999] <- 0.99999
  cdf[cdf<0.00001] <- 0.00001
  loglikelihood<--sum(ydum.*log(cdf))-sum((1-ydum.)*log(1-cdf))
  return (loglikelihood)
}

belta<-solve(t( X ) %*%X ) %*%t( X ) %*%ydum
#Calculated the optimized probit_omdel and the corresponding coefficient
probit_model<-optim(par=belta,probit_likelihood)
probit_belta5<-probit_model$par
#Calculated the optimized probit_omdel and the corresponding coefficient
logit_model<-optim(par=belta,logit_likelihood)
logit_belta5<-logit_model$par

#Construct a SSR of linear model
linear_SSR<-function(belta,X.=X,ydum.=ydum){
  ydum_hat<-X%*%belta
  residu<-ydum-ydum_hat
  SSR<-sum(residu^2)
  return(SSR)
}
linear_model<-optim(par=belta,linear_SSR,hessian=TRUE)

#Compare the coefficients and testify whether they are significant or not
belta_probit<-glm(ydum~X1+X2+X3,family = binomial(link = "probit"),data=DATA5)
belta_linear<-lm(ydum~X1+X2+X3,data=DATA5)
belta_logit<-glm(ydum~X1+X2+X3,family = binomial,data=DATA5)
summary(belta_probit)
summary(belta_logit)
summary(belta_linear)
##
