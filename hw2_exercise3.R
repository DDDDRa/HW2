####Exercise 3
###Question 1: Write a function that returns the likelihood of the probit
#Calculate the coefficient by using OLS method
belta0 <-solve(t( X ) %*%X ) %*%t( X ) %*%ydum
belta<-belta0

#Construct a matrix to place data
X<-as.matrix(cbind(1,X1,X2,X3))
#Construct a likelihood function

likelihood_cal<-function(belta,X. = X, ydum. = ydum){
  #Calculate the cdf of X*beta
  cdf<-pnorm(X.%*%belta)
  #Check whether or not the cdf is in the range(0.00001,0.99999)
  cdf[cdf>0.99999] <- 0.99999
  cdf[cdf<0.00001] <- 0.00001
  #Calculate the likelihood of probit model
  loglikelihood<--sum(ydum.*log(cdf))-sum((1-ydum.)*log(1-cdf))
  return (loglikelihood)
}
###Question 2: Implement the steepest ascent optimization algorithm to maximize that likelihood.
steep_gradient<-function(belta,X){
  beltadif<-c(1,1,1,1)
  # Set while loop to optimize the likelihood by using gradient ascent method
  while(sqrt(sum(beltadif^2))>=0.0000001){
    likelihood<-likelihood_cal(belta,X)
    #Calculate the direction (a vector of 4*1)
    direction<-matrix(0,nrow=4,ncol=1)
    interval<-matrix(0,nrow=4,ncol=4)
    interval[1,]<-matrix(c(0.0001,0,0,0),nrow=4,ncol=1)
    interval[2,]<-matrix(c(0,0.0001,0,0),nrow=4,ncol=1)
    interval[3,]<-matrix(c(0,0,0.0001,0),nrow=4,ncol=1)
    interval[4,]<-matrix(c(0,0,0,0.0001),nrow=4,ncol=1)
    likelihood_new<-matrix(0,nrow=4,ncol=1)
    for (j in 1:4){
      transfer<-as.matrix(interval[j,])
      belta_new<-belta+transfer
      likelihood_new[j,1]<-likelihood_cal(belta_new,X)
      direction[j,1]<-(likelihood_new[j,1]-likelihood)/0.0001
    }
    #Get gradient function
    belta1<-belta-0.00001*direction
    beltadif<-as.matrix(belta1-belta)
    belta<-belta1
  }
  #Get updated likelihood function
  llh<-likelihood_cal(belta,X)
  print(belta)
  return (belta)
}
# Get optimized beta and optimized likelihood
belta_probit<-steep_gradient(belta,X)
likelihood_cal(belta_probit,X)

###Question3: How diﬀerent are the parameters from the true parameters
##Compare the parameters calculated with the true parameters calculated int the exercise 4 (by using glm function)
