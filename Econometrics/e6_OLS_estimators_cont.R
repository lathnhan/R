## Running this code will return four numbers (0.048, 0.047, 0.952 and 0.953);
## The first two are the results for Case 1 of Part A;
## The second two are the results for Case 1 of Part B, 2;
## To get the remaining results, simply change the parameter values
## in the simulation setup accordingly; e.g., for Case 3 of Parts A and B, 2,
## change the sample size (n) to 100.

## First, call up required packages:
library(MASS)
library(AER)
set.seed(12)

## Set parameter values & 'counters':
reps <-1000
rejH0 <-0
rejH0.HC <-0
n<-50
beta0<-0
beta1<-0
beta2<-1
rho<-0

## Create space for 'inside CI' indicator (Part B, 2)
incl.OLS <- matrix(nrow=reps,ncol=1)
incl.HC <- matrix(nrow=reps,ncol=1)
#2-sided critical t-value
t.cv <-qt(0.975, df=n-3)

## Set up multivariate Xs
muX <-rbind(0,0)
varX <-cbind(rbind(1,rho), rbind(rho,1))

## Note: in the loop below, to run the heteroskedastic cases,
## remove the hashtag (#) in front of the Y line immediately below
## '# hetero Y' and put a hashtag in front of the Y line 
## immediately below ''# homo Y'; running the code as is
## will give you results for the Homo cases

for (j in 1:reps) {
  X <-mvrnorm(n, mu=muX, Sigma=varX)
  X1<-X[,1]
  X2<-X[,2]
  #homo Y:
Y <-rnorm(n, mean=beta0+beta1*X1+beta2*X2, sd=1)
  #hetero Y:
#Y <-rnorm(n, mean=beta0+beta1*X1+beta2*X2, sd=sqrt(exp(X1)))
  eq1 <-lm(Y~X1+X2)
  #t-test using OLS SEs:
  testOLS <-coeftest(eq1)
  pOLS <-testOLS[2,4]
  if (pOLS <0.05) {rejH0 <-rejH0+1}
  #t-test using HC SEs:
  testHC <-coeftest(eq1, vcov=vcovHC(eq1))
  pHC <-testHC[2,4]
  if (pHC <0.05) {rejH0.HC <-rejH0.HC+1}
#CI calculations:
  se.OLS <-coeftest(eq1)[,2]
  ciL.OLS <-eq1$coefficients[2]-t.cv*se.OLS[2]
  ciU.OLS <-eq1$coefficients[2]+t.cv*se.OLS[2]
  incl.OLS[j] <-(ciL.OLS < beta1) & (ciU.OLS > beta1)
  se.HC <-coeftest(eq1, vcov=vcovHC(eq1))[,2]
  ciL.HC <-eq1$coefficients[2]-t.cv*se.HC[2]
  ciU.HC <-eq1$coefficients[2]+t.cv*se.HC[2]
  incl.HC[j] <-(ciL.HC < beta1) & (ciU.HC > beta1)
}

## Print rejection frequencies (Part A)
print(rejH0/reps)
print(rejH0.HC/reps) 

## Print coverage probabilities (Part B, 2)
print(colMeans(incl.OLS))
print(colMeans(incl.HC))
