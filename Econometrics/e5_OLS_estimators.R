## The result of running the code in this document is
## the mean and variance of the simulated OLS estimates
## for the following parameter settings:
## n = 50, rho = 0.5, sigma = 1.
## To get the results for (e.g.) different sample sizes (n),
## you will need to manually change the value of n and re-run the code;
## likewise for different values of the other parameters.

## We need to call up another package (should already be installed)
## for sampling from a multivariate normal distribution:
library(MASS)

## To ensure that results *only* change as a result of changing
## the parameter values in the simulation setup (n, rho etc.)
## (and not the sample values), set a 'seed' for the random number generation
## (it doesn't have to be 12):
set.seed(12)

## Type in the parameter values (see question):
reps <-1000
n<-50
beta0<-0
beta1<-1
beta2<-1
rho<-0.5
sigma<-1

## Set up the multivariate Xs:
muX <-rbind(0,0)
varX <-cbind(rbind(1,rho), rbind(rho,1))

## Store the slope estimates in a beta1.hat column:
beta1.hat <-matrix(nrow=reps, ncol=1)

## The loop below does the following (1000 times):
## samples values of X1 and X2;
## generates data on Y from the Xs;
## regresses (by OLS) Y on the Xs;
## saves the estimated X1 coefficient (beta1.hat).

for (j in 1:reps){
  X <-mvrnorm(n, mu=muX, Sigma=varX)
  X1<-X[,1]
  X2<-X[,2]
  Y <-rnorm(n, mean=beta0+beta1*X1+beta2*X2, sd=sigma)
  eq1 <-lm(Y~X1+X2)
  beta1.hat[j] <-eq1$coefficients[2]
}

## To get the mean beta1.hat, its variance and histogram:
print(mean(beta1.hat))
print(var(beta1.hat))
hist(beta1.hat)
