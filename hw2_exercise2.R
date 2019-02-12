#Exercise 2

#Question 1
#Calculate the correlation between Y and X1
correlation<-cor(Y,X1)

#Question 2 & Question 3

#Using OLS method
##Put X variables, Y variable and eps variable in the matrix form
X<-as.matrix(cbind(1,X1,X2,X3))
y<-as.matrix(Y)
eps<-as.matrix(eps)
##Calculate the coefficients on the regression by using OLS method
belta2 <-solve(t( X ) %*%X ) %*%t( X ) %*%y
##Calculate the standard errors
##Calculate the standard errors
Sderror<-(solve(t( X ) %*%X ) %*%t( X ) %*%eps)

#Calculate the coefficients and their standard errors by using bootstrap
##49 replications
num=49
#Create a empty matrix to store samples which are re-sampled from the original data
pgn<-matrix(0,nrow=10000,ncol=4)
#Put Y and X variables in a data frame
DATA<-as.matrix(cbind(Y,X1,X2,X3))
#Construct a empty matrix to store standard errors of each sample
sdboot1<-matrix(0,nrow=num,ncol=4)
for (j in 1:num){
  #Sampling from original data with replacement
  for (i in 1:10000){
    k<-sample(1:nrow(DATA),1)
    sp<-as.matrix(DATA[k,])
    pgn[i,]<-t(sp)
  }
  #Constrcut Xloop; put new sample(X variables) in this matrix
  Xloop<-as.matrix(cbind(1,pgn[,2],pgn[,3],pgn[,4]))
  #Constrcut Yloop; put new sample(Y variable) in this matrix
  Yloop<-as.matrix(pgn[,1])
  #Calculate coefficients by using OLS method
  coeff<-solve(t( Xloop) %*%Xloop ) %*%t( Xloop ) %*%Yloop
  #Calculate residuals
  residu<-as.matrix(pgn[,1]-coeff[1]-coeff[2]*pgn[,2]-coeff[3]*pgn[,3]-coeff[4]*pgn[,4])
  #Calculate the standard errors by using OLS method
  Sderrorboot1<-solve(t( Xloop) %*% Xloop ) %*%t( Xloop ) %*%residu
  #Plug the standard errors into matrix named sdboot1
  sdboot1[j,]<-Sderrorboot1
}

##499 replications
num1=499
#Construct a empty matrix to store standard errors of each sample
sdboot2<-matrix(0,nrow=num1,ncol=4)
for (j in 1:num1){
  #Sampling from original data with replacement
  for (i in 1:10000){
    k<-sample(1:nrow(DATA),1)
    sp<-as.matrix(DATA[k,])
    pgn[i,]<-t(sp)
  }
  #Constrcut Xloop; put new sample(X variables) in this matrix
  Xloop<-as.matrix(cbind(1,pgn[,2],pgn[,3],pgn[,4]))
  #Constrcut Yloop; put new sample(Y variable) in this matrix
  Yloop<-as.matrix(pgn[,1])
  #Calculate coefficients by using OLS method
  coeff<-solve(t( Xloop) %*%Xloop ) %*%t( Xloop ) %*%Yloop
  #Calculate residuals
  residu<-as.matrix(pgn[,1]-coeff[1]-coeff[2]*pgn[,2]-coeff[3]*pgn[,3]-coeff[4]*pgn[,4])
  #Calculate the standard errors by using OLS method
  Sderrorboot1<-solve(t( Xloop) %*% Xloop ) %*%t( Xloop ) %*%residu
  #Plug the standard errors into matrix named sdboot2
  sdboot2[j,]<-Sderrorboot1
}

