## Notes: 
## (a) Don't forget to set working directory before running code!
## (b) Having installed the dynlm package, call it up for use:
library(dynlm)
library(AER)
## Part A
##(1) Read in data and create time series variables:
## (with obvious names!):
t9 <-read.csv("GoyalWelch.csv")
EP <-ts(t9$EqPrem, start=1926, end=2002)
DP <-ts(t9$DivPrice, start=1926, end=2002)
## Get time series plots and correlograms (ACFs):
plot(EP)
plot(DP)
##(2) Autocorrelation functions (aka correlograms):
acf(EP)
acf(DP)

## Part B
## Create DP up to 1999 only:
DP99 <-window(DP, start=1926, end=1999)
n <-length(DP99)
##(3) Forecasting with AR(1) model:
## (Forecasts labelled DP2000, DP2001, DP2002 below)
eqAR1 <-dynlm(DP99~L(DP99, 1))
print(summary(eqAR1))
beta0hat <-eqAR1$coefficients[1]
beta1hat <-eqAR1$coefficients[2]
DP2000 <-beta0hat+beta1hat*DP99[n]
DP2001 <-beta0hat+beta1hat*DP2000
DP2002 <-beta0hat+beta1hat*DP2001

eqAR1.1step <- dynlm(DP99~L(DP99-DP99[n],1))
print(round(coeftest(eqAR1.1step, vcov=vcovHC(eqAR1.1step)),3))

#DP00 <- window(DP, start=1926, end=2000)
#n00 <- length(DP00)
#eqAR1.2001 <- dynlm(DP00~L(DP00-DP00[n],1))
#print(round(coeftest(eqAR1.2001, vcov=vcovHC(eqAR1.2001)),3)) 

#eqAR1.2002 <- dynlm(DP99~L(DP99-DP[n+2],1))
#print(round(coeftest(eqAR1.2002, vcov=vcovHC(eqAR1.2002)),3)) 

## Print forecasts:
print(rbind(DP2000,DP2001,DP2002))

##(4) Forecasting with AR(2) model (very similar!):
## (Forecast names add _2 below)
eqAR2 <-dynlm(DP99~L(DP99, 1:2))
print(summary(eqAR2))
beta0hat <-eqAR2$coefficients[1]
beta1hat <-eqAR2$coefficients[2]
beta2hat <-eqAR2$coefficients[3]
DP2000_2 <-beta0hat+beta1hat*DP99[n]+beta2hat*DP99[n-1]
DP2001_2 <-beta0hat+beta1hat*DP2000_2+beta2hat*DP99[n]
DP2002_2 <-beta0hat+beta1hat*DP2001_2+beta2hat*DP2000_2
print(rbind(DP2000_2,DP2001_2,DP2002_2))

##(5) Comparing forecast & actual values in a table:
actualDP <-rbind(DP[n+1], DP[n+2], DP[n+3])
ar1forecasts <-rbind(DP2000, DP2001, DP2002)
ar2forecasts <-rbind(DP2000_2, DP2001_2, DP2002_2)
mat <-cbind(ar1forecasts, ar2forecasts, actualDP)
colnames(mat) <-c("AR1 Forecasts", "AR2 Forecasts", "Actual Values")
print(mat)

##(6) Computing root mean squared errors (RMSEs):
ar1esq <-(actualDP-ar1forecasts)^2
ar2esq <-(actualDP-ar2forecasts)^2
ar1RMSE <-sqrt(mean(ar1esq))
ar2RMSE <-sqrt(mean(ar2esq))
print(cbind(ar1RMSE,ar2RMSE))

