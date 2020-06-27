
## Note: don't forget to set working directory!
## Also don't forget to install and call CADFtest.
## Running this code produces two time series plots,
## two acfs, and two ADF tests.

install.packages("CADFtest")

##(1) Read in data, create time series, plots and acfs:
t11a <-read.csv("GoyalWelch.csv")
EP <-ts(t11a$EqPrem, start=1926, end=2002)
DY <-ts(t11a$DivYield, start=1926, end=2002)
plot(DY)
plot(EP)
acf(DY)
acf(EP)

##(2) Unit root testing
library(CADFtest)
# Include intercept in model with "drift" option 
# Use modified AIC for lag selection
DF.EP <-CADFtest(EP, type="drift", criterion="MAIC", max.lag.y=8)
DF.DY <-CADFtest(DY, type="drift", criterion="MAIC", max.lag.y=8)
print(DF.EP)
print(DF.DY)

## R code for tute 11, Part B
## This can be run as a new R script
## (or you can copy/paste it in after the Part A code).
## You will need to change parameter settings 
## where indicated (by 'but vary') to cover all cases.
## This code produces three rows of numbers:
## row 1: beta1.hat means for regressions i, ii, iii;
## row 2: beta1.hat SDs for regressions i, ii, iii;
## row 3: beta1.hat zero-null rejection frequencies for regressions i, ii, iii.

##(1) Simulate GY-type 'unbalanced' variables (question 1(a))
set.seed(12)
n <-100
## (do NOT vary!)
beta0 <-1
beta1 <-1
# Set up X variable to follow a random walk: 
# (see tute answers for details)
X <-cumsum(rnorm(n))
Y <-rnorm(n,mean=beta0+beta1*X, sd=1)

##(2) Look at time series plots and acfs of levels variables
## and then those of the first differences:
plot(X, type="line")
plot(Y, type="line")
acf(X)
acf(Y)
# Create first differences
D1X <-diff(X)
D1Y <-diff(Y)
plot(D1X, type="line")
plot(D1Y, type="line")
acf(D1X)
acf(D1Y)

##(3) Use simulations to check properties of 
## balanced and unbalanced regressions (question 1(b)):
n <-50
# (but vary)
reps <-1000
beta1.hat <-matrix(nrow=reps, ncol=3)
tbeta1.sig <-matrix(nrow=reps, ncol=3)
# Set up the loop:
for (j in 1:reps){
  X <-cumsum(rnorm(n))
  Y <-rnorm(n,mean=beta0+beta1*X, sd=1)
  D1Y <-diff(Y)
  D1X <-diff(X)
  # (i) regression in levels (balanced):
  eq_LL <-summary(lm(Y~X))
  beta1.hat[j,1] <-eq_LL$coefficients[2,1]
  tbeta1.sig[j,1] <-(eq_LL$coefficients[2,4]<0.05)
  # (ii) regression in first differences (balanced):
  eq_DD <-summary(lm(D1Y~D1X))
  beta1.hat[j,2] <-eq_DD$coefficients[2,1]
  tbeta1.sig[j,2] <-(eq_DD$coefficients[2,4]<0.05)
  # (iii) regression of D1Y on X (unbalanced):
  eq_DL <-summary(lm(D1Y~X[2:n]))
  beta1.hat[j,3] <-eq_DL$coefficients[2,1]
  tbeta1.sig[j,3] <-(eq_DL$coefficients[2,4]<0.05)
}

##(4) Get beta1.hat means, SDs and rejection frequencies:
Means <-apply(beta1.hat, 2, mean)
SDs <-apply(beta1.hat, 2, sd)
Sigs <-apply(tbeta1.sig, 2, mean)

print(Means)
print(SDs)
print(Sigs)

# Run the VAR model

beta1.hat.var <-matrix(nrow=reps, ncol=3)
tbeta1.sig.var <-matrix(nrow=reps, ncol=3)

for (j in 1:reps){
  X <-cumsum(rnorm(n))
  Y <-rnorm(n,mean=beta0+beta1*X, sd=1)
  D1Y <-diff(Y)
  D1X <-diff(X)
  # (i) regression in levels (balanced):
  eq_LL.var <-summary(dynlm(Y~L(X,1)))
  beta1.hat.var[j,1] <-eq_LL.var$coefficients[2,1]
  tbeta1.sig.var[j,1] <-(eq_LL.var$coefficients[2,4]<0.05)
  # (ii) regression in first differences (balanced):
  eq_DD.var <-summary(dynlm(D1Y~L(D1X,1)))
  beta1.hat.var[j,2] <-eq_DD.var$coefficients[2,1]
  tbeta1.sig.var[j,2] <-(eq_DD.var$coefficients[2,4]<0.05)
  # (iii) regression of D1Y on X (unbalanced):
  eq_DL.var <-summary(dynlm(D1Y~L(X[2:n],1)))
  beta1.hat.var[j,3] <-eq_DL.var$coefficients[2,1]
  tbeta1.sig.var[j,3] <-(eq_DL.var$coefficients[2,4]<0.05)
}

##(4) Get beta1.hat means, SDs and rejection frequencies:
Means.var <-apply(beta1.hat.var, 2, mean)
SDs.var <-apply(beta1.hat.var, 2, sd)
Sigs.var <-apply(tbeta1.sig.var, 2, mean)

print(Means.var)
print(SDs.var)
print(Sigs.var)

