## Notes: 
## (a) Don't forget to set working directory before running code!
## (b) To run the code, you'll need to install the packages 
## dynlm, vars, texreg

## Part A
##(1) Read in data and create time series variables:
## (with obvious names!):
t10<-read.csv("GoyalWelch.csv")
EP<-ts(t10$EqPrem,start=1926,end=2002)
DY<-ts(t10$DivYield,start=1926,end=2002)
EP99<-window(EP,end=1999)
DY99<-window(DY,end=1999)
n<-length(EP99)

##(2) Call up the required packages:
library(dynlm)
library(vars)

##(3) Find appropriate lag length for a range of models.
## First, see if DY has a trend to inform VARselect option:
plot(DY)
plot(EP)
acf(EP)
acf(DY)

#This one won't handle rift and trend as well
maxlag <- 5
aic_a <- matrix(nrow=maxlag, ncol=1)
for (j in 1:maxlag){
aic_a[j] <- AIC(dynlm(EP99~L(EP99,1:j)))
}
print(which.min(aic_a))

## Having already found no obvious trend in EP,
## but some in DY (see plot), we will use the "const" option in VARselect for EP,
## and the option "both" (which adds a trend) for DY.
##3(a) Lag length for AR model for equity premium
ICa <-VARselect(EP99, type="const", lag.max=5)
print(ICa$selection[1])
##3(b) Lag length for AR model for dividend yield
ICb <-VARselect(DY99, type="both", lag.max=5)
print(ICb$selection[1])
##3(c) Lag length for VAR in EP and DY
ICc <-VARselect(cbind(EP99,DY99), type="both", lag.max=5)
print(ICc$selection[1])
## Results: 1 lag for (a) and (b), 2 for (c)

## Part B
##(4) Estimate dynamic regressions:
##4(a) AR(1) model for equity premium
eq.a<-dynlm(EP99~L(EP99,1))
## If you wish to manually check the AR(1) EP forecast produced later:
print(summary(eq.a))
##4(b) AR(1) model for dividend yield
eq.b<-dynlm(DY99~L(DY99,1))
print(summary(eq.b))
##4(c) VAR(2) model
VAR.ep<-dynlm(EP99~L(EP99,1:2)+L(DY99,1:2))
VAR.dy<-dynlm(DY99~L(EP99,1:2)+L(DY99,1:2))
##4(d) Predictive regression for equity premium
eq.d<-dynlm(EP99~L(DY99,1))

print(summary(VAR.ep))
print(summary(VAR.dy))
print(summary(eq.d))

##(5) Obtain ACFs for residuals:
acf(eq.a$residuals,main="ACF for part (a)")
acf(eq.b$residuals,main="ACF for part (b)")
acf(VAR.ep$residuals,main="ACF for part (c), equity premium")
acf(VAR.dy$residuals,main="ACF for part (c), dividend yield")
acf(eq.d$residuals,main="ACF for part (d)")

##(6) AR(1) EP forecasts:
n<-length(EP99)
cAR1<-eq.a$coefficients
EP.AR1<-EP
for (h in 1:3) {
  Z<-c(1, EP.AR1[n+h-1])
  EP.AR1[n+h]<-cAR1%*%Z
}

#A more manual way
EP_Z1 <- c(1,EP.AR1[n])
EP_AR_1 <- cAR1 %*% EP_Z1
print(EP_AR_1)

EP_Z2 <- c(1, EP_AR_1)
EP_AR_2 <- cAR1 %*% EP_Z2
print(EP_AR_2)

EP_Z3 <- c(1, EP_AR_2)
EP_AR_3 <- cAR1 %*% EP_Z3
print(EP_AR_3)


##(7) VAR(2) forecasts:
cEP<-VAR.ep$coefficients
cDY<-VAR.dy$coefficients
EP.VAR<-EP
DY.VAR<-DY
for (h in 1:3)
{
  Z<-c(1, EP.VAR[n+h-(1:2)],DY.VAR[n+h-(1:2)])
  EP.VAR[n+h]<-cEP%*%Z
  DY.VAR[n+h]<-cDY%*%Z
}

##(8) Predictive regression (PR) forecast (only one-step ahead possible):
c.PR<-eq.d$coefficients
EP.PR<-EP
Z<-c(1, DY[n])
EP.PR[n+1]<-c.PR%*%Z
EP.PR[n+(2:3)]<-NA

##(9) Sample mean forecast:
EP.mean<-EP
EP.mean[n+(1:3)]<-mean(EP99)

##(10) Tabulate actual and forecast values for 2000--2002
tab<-cbind(EP,EP.AR1,EP.VAR,EP.PR,EP.mean)[n+(1:3),]
rownames(tab)<-c(2000,2001,2002)
colnames(tab)<-c("Actual","AR(1)","VAR","PR","Mean")
print(round(tab,3))

##(11) RMSE calculations:
RMSE<-NULL
for (j in c(2,3,5)){
  RMSE<-rbind(RMSE,sqrt(mean((tab[,j]-tab[,1])^2)))
}
rownames(RMSE)<-c("AR(1)","VAR(2)","Mean")
colnames(RMSE)<-"RMSE"
print(round(RMSE,3))

RMSE_PR <- sqrt(mean((EP[n+1]-EP.PR[n+1])^2))

##(12) Tabulate regression results using texreg:
library(texreg)
htmlreg(list(eq.a,eq.b,VAR.ep,VAR.dy,eq.d), file="t10_results.doc",
        caption="Regression results, tute 10", caption.above = TRUE,
        custom.model.names = c("(a); EP", "(b); DY", "(c); EP", "(c); DY", "(d); EP"), digits=2)

