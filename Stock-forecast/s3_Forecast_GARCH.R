#********************************************************************************
# Quantitative Research Test
# Portland House Research and Advisory
# Nhan La
#********************************************************************************


#********************************************************************************
# TASK 4.3: BUILD AND EVALUATE FORECAST MODELS

setwd("D:/Portland House/Data")

library(FinTS) #for function `ArchTest()`
library(rugarch) #for GARCH models
library(tseries) # for `adf.test()`
library(dynlm) #for function `dynlm()`
library(vars) # for function `VAR()`
library(nlWaldTest) # for the `nlWaldtest()` function
library(lmtest) #for `coeftest()` and `bptest()`.
library(broom) #for `glance(`) and `tidy()`
library(car) #for `hccm()` robust standard errors
library(sandwich)
library(knitr) #for `kable()`
library(forecast) 
#library(lubridate)
#library(tidyr)

## 1/ Further steps of data preparation 
### Create lag series of technical features
fcr.ABC <- fcr.ABC %>%
  mutate(hl.ABC.lag = f.lag(hl.ABC))%>%
  mutate(volume.ABC.lag = f.lag(volume.ABC)) %>%
  mutate(d.open.ABC.lag = f.lag(d.open.ABC))

write.csv(fcr.ABC,file=("fcr.ABC.txt"),row.names=FALSE)

fcr.ABC <- read.csv("fcr.ABC.txt", header=TRUE) #Only read if needed, e.g., in case R Studio is crashed
fcr.ABC <- fcr.ABC %>%
 mutate(date=ymd(date)) #Need to call lubridate
  
#### Since the library that we will use to specify and estimate the model will not handle missing data,
#### we will drop the observations with missing data.
fcr.ABC <- fcr.ABC %>%
  drop_na()

### Split the original data set to the train and test data sets
#### In this exercise, we will use 80% of the original data set as the train set and the remaining 20%
#### as the test set. We will use these data sets to fit several models for forecasting future close returns
#### of ABC, in particular variations of ARIMA-GARCH models.

train.ABC <- head(fcr.ABC, round(nrow(fcr.ABC) *0.8))
test.ABC <- tail(fcr.ABC, nrow(fcr.ABC)-nrow(train.ABC))

## 2/ Test for the presence of conditional heteroskedastic effect

ARCH.ABC.mean <- dynlm(train.ABC$fcr.ABC~1)
summary(ARCH.ABC.mean)
ARCH.ABC.ehatsq <- ts(resid(ARCH.ABC.mean)^2)
plot.ts(ARCH.ABC.ehatsq,main="",ylab="")
acf(ARCH.ABC.ehatsq,main="")
pacf(ARCH.ABC.ehatsq,main="")

ARCH.ABC <- ArchTest(train.ABC$fcr.ABC, lags=1, demean=TRUE)
ARCH.ABC

## 2/ Specify, estimate and crossvalidate ARIMA-GARCH models

### ARMA(1,0) - GARCH(1,1) without external features
garch.1011.n <- ugarchspec(
  variance.model=list(model="fGARCH", garchOrder=c(1,1),submodel="GARCH"),
  mean.model=list(armaOrder=c(1,0), include.mean=TRUE, archm=TRUE, archpow=2),
  distribution.model="norm" )
garch.1011.n.fit <- ugarchfit(spec=garch.1011.n, solver = 'hybrid', data=train.ABC$fcr.ABC)
garch.1011.n.fit

### ARMA(1,0) - GARCH(1,1) with external features
garch.1011.e <- ugarchspec(
  variance.model=list(model="fGARCH", garchOrder=c(1,1),submodel="GARCH"),
  mean.model=list(armaOrder=c(1,0), include.mean=TRUE, archm=TRUE, archpow=2,
                  external.regressors=as.matrix(train.ABC[,c(3,5,6,8)])),
  distribution.model="norm" )
garch.1011.e.fit <- ugarchfit(spec=garch.1011.e, solver = 'hybrid', data=train.ABC$fcr.ABC)
garch.1011.e.fit

### ARMA(1,0) - GARCH(1,2) without external features
garch.1012.n <- ugarchspec(
  variance.model=list(model="fGARCH", garchOrder=c(1,2),submodel="GARCH"),
  mean.model=list(armaOrder=c(1,0), include.mean=TRUE, archm=TRUE, archpow=2),
  distribution.model="norm" )
garch.1012.n.fit <- ugarchfit(spec=garch.1012.n, solver = 'hybrid', data=train.ABC$fcr.ABC)
garch.1012.n.fit

### ARMA(1,0) - GARCH(1,2) with external features
garch.1012.e <- ugarchspec(
  variance.model=list(model="fGARCH", garchOrder=c(1,2),submodel="GARCH"),
  mean.model=list(armaOrder=c(1,0), include.mean=TRUE, archm=TRUE, archpow=2,
                  external.regressors=as.matrix(train.ABC[,c(3,5,6,8)])),
  distribution.model="norm" )
garch.1012.e.fit <- ugarchfit(spec=garch.1012.e, solver = 'hybrid', data=train.ABC$fcr.ABC)
garch.1012.e.fit

### ARMA(1,0) - GARCH(2,1) without external features
garch.1021.n <- ugarchspec(
  variance.model=list(model="fGARCH", garchOrder=c(2,1),submodel="GARCH"),
  mean.model=list(armaOrder=c(1,0), include.mean=TRUE, archm=TRUE, archpow=2),
  distribution.model="norm" )
garch.1021.n.fit <- ugarchfit(spec=garch.1021.n, solver = 'hybrid', data=train.ABC$fcr.ABC)
garch.1021.n.fit

### ARMA(1,0) - GARCH(2,1) with external features
garch.1021.e <- ugarchspec(
  variance.model=list(model="fGARCH", garchOrder=c(2,1),submodel="GARCH"),
  mean.model=list(armaOrder=c(1,0), include.mean=TRUE, archm=TRUE, archpow=2,
                  external.regressors=as.matrix(train.ABC[,c(3:8)])),
  distribution.model="norm" )
garch.1021.e.fit <- ugarchfit(spec=garch.1021.e, solver = 'hybrid', data=train.ABC$fcr.ABC)
garch.1021.e.fit

### ARMA(1,0) - GARCH(2,2) without external features
garch.1022.n <- ugarchspec(
  variance.model=list(model="fGARCH", garchOrder=c(2,2),submodel="GARCH"),
  mean.model=list(armaOrder=c(1,0), include.mean=TRUE, archm=TRUE, archpow=2),
  distribution.model="norm" )
garch.1022.n.fit <- ugarchfit(spec=garch.1022.n, solver = 'hybrid', data=train.ABC$fcr.ABC)
garch.1022.n.fit

### ARMA(1,0) - GARCH(2,2) with external features
garch.1022.e <- ugarchspec(
  variance.model=list(model="fGARCH", garchOrder=c(2,2),submodel="GARCH"),
  mean.model=list(armaOrder=c(1,0), include.mean=TRUE, archm=TRUE, archpow=2,
                  external.regressors=as.matrix(train.ABC[,c(3,5,6,8)])),
  distribution.model="norm" )
garch.1022.e.fit <- ugarchfit(spec=garch.1022.e, solver = 'hybrid', data=train.ABC$fcr.ABC)
garch.1022.e.fit

### ARMA(1,0) - GARCH(3,3) with external features - NOT CONVERGED
garch.1033.e <- ugarchspec(
  variance.model=list(model="fGARCH", garchOrder=c(3,3),submodel="GARCH"),
  mean.model=list(armaOrder=c(1,0), include.mean=TRUE, archm=TRUE, archpow=2,
                  external.regressors=as.matrix(train.ABC[,c(3,5,6,8)])),
  distribution.model="norm" )
garch.1033.e.fit <- ugarchfit(spec=garch.1033.e, solver = 'hybrid', data=train.ABC$fcr.ABC)
garch.1033.e.fit

### The best model is ARMA(1,0) - GARCH(1,1) without external features. Examine the residuals
garch.1011.n.res <- (garch.1011.n.fit@fit$residuals)
plot.ts(garch.1011.n.res,ylab="") 
acf(garch.1011.n.res,main="")

#### Validate (or filter, as used in the rugarch terminology) model on the test data
garch.1011.n.valid <- ugarchspec(
  variance.model=list(model="fGARCH", garchOrder=c(1,1),submodel="GARCH"),
  mean.model=list(armaOrder=c(1,0), include.mean=TRUE, archm=TRUE, archpow=2),
  distribution.model="norm" )
garch.1011.n.valid.fit <- ugarchfit(spec=garch.1011.n.valid, solver = 'hybrid', data=train.ABC$fcr.ABC)
setfixed(garch.1011.n.valid) <- as.list(coef(garch.1011.n.valid.fit))
garch.1011.n.valid.fit <- ugarchfilter(spec=garch.1011.n.valid,data=test.ABC$fcr.ABC)
garch.1011.n.valid.fit

#### Plot the forecast values and confidence intervals on the test data
garch.1011.n.forecast <- ugarchforecast(garch.1011.n.fit, n.ahead = 176)
garch.1011.n.forecast.fitted <- data.frame("fitted"=c(fitted(garch.1011.n.forecast)))
garch.1011.n.forecast.sig <- data.frame("sig"=c(sigma(garch.1011.n.forecast)))

test.ABC <- test.ABC %>%
  mutate(fcr.ABC.fitted = garch.1011.n.forecast.fitted$fitted) %>%
  mutate(fcr.ABC.lCI = fcr.ABC.fitted -2*garch.1011.n.forecast.sig$sig)%>%
  mutate(fcr.ABC.hCI = fcr.ABC.fitted +2*garch.1011.n.forecast.sig$sig)

test.ABC %>%
    ggplot(aes(x=date))+ 
    geom_line(aes(y = fcr.ABC))+ 
    geom_line(aes(y = fcr.ABC.fitted))+
    geom_line(aes(y = fcr.ABC.lCI), lty=2)+
    geom_line(aes(y = fcr.ABC.hCI), lty=2)+
    ylab("")

##############################################################
## 3/ Specify and estimate ARIMA-GARCH models on the original data
### ARMA(1,0) - GARCH(1,1) without external features
garch.1011.n.o <- ugarchspec(
  variance.model=list(model="fGARCH", garchOrder=c(1,1),submodel="GARCH"),
  mean.model=list(armaOrder=c(1,0), include.mean=TRUE, archm=TRUE, archpow=2),
  distribution.model="norm" )
garch.1011.n.o.fit <- ugarchfit(spec=garch.1011.n.o, solver = 'hybrid', data=fcr.ABC$fcr.ABC)
garch.1011.n.o.fit

### ARMA(1,0) - GARCH(1,1) with external features
garch.1011.e.o <- ugarchspec(
  variance.model=list(model="fGARCH", garchOrder=c(1,1),submodel="GARCH"),
  mean.model=list(armaOrder=c(1,0), include.mean=TRUE, archm=TRUE, archpow=2,
                  external.regressors=as.matrix(fcr.ABC[,c(3:8)])),
  distribution.model="norm" )
garch.1011.e.o.fit <- ugarchfit(spec=garch.1011.e.o, solver = 'hybrid', data=fcr.ABC$fcr.ABC)
garch.1011.e.o.fit

### ARMA(1,0) - GARCH(1,2) without external features
garch.1012.n.o <- ugarchspec(
  variance.model=list(model="fGARCH", garchOrder=c(1,2),submodel="GARCH"),
  mean.model=list(armaOrder=c(1,0), include.mean=TRUE, archm=TRUE, archpow=2),
  distribution.model="norm" )
garch.1012.n.o.fit <- ugarchfit(spec=garch.1012.n.o, solver = 'hybrid', data=fcr.ABC$fcr.ABC)
garch.1012.n.o.fit

### ARMA(1,0) - GARCH(1,2) with external features
garch.1012.e.o <- ugarchspec(
  variance.model=list(model="fGARCH", garchOrder=c(1,2),submodel="GARCH"),
  mean.model=list(armaOrder=c(1,0), include.mean=TRUE, archm=TRUE, archpow=2,
                  external.regressors=as.matrix(fcr.ABC[,c(3:8)])),
  distribution.model="norm" )
garch.1012.e.o.fit <- ugarchfit(spec=garch.1012.e.o, solver = 'hybrid', data=fcr.ABC$fcr.ABC)
garch.1012.e.o.fit

### ARMA(1,0) - GARCH(2,1) without external features
garch.1021.n.o <- ugarchspec(
  variance.model=list(model="fGARCH", garchOrder=c(2,1),submodel="GARCH"),
  mean.model=list(armaOrder=c(1,0), include.mean=TRUE, archm=TRUE, archpow=2),
  distribution.model="norm" )
garch.1021.n.o.fit <- ugarchfit(spec=garch.1021.n.o, solver = 'hybrid', data=fcr.ABC$fcr.ABC)
garch.1021.n.o.fit

### ARMA(1,0) - GARCH(2,1) with external features
garch.1021.e.o <- ugarchspec(
  variance.model=list(model="fGARCH", garchOrder=c(2,1),submodel="GARCH"),
  mean.model=list(armaOrder=c(1,0), include.mean=TRUE, archm=TRUE, archpow=2,
                  external.regressors=as.matrix(fcr.ABC[,c(3:8)])),
  distribution.model="norm" )
garch.1021.e.o.fit <- ugarchfit(spec=garch.1021.e.o, solver = 'hybrid', data=fcr.ABC$fcr.ABC)
garch.1021.e.o.fit

### ARMA(1,0) - GARCH(2,2) without external features
garch.1022.n.o <- ugarchspec(
  variance.model=list(model="fGARCH", garchOrder=c(2,2),submodel="GARCH"),
  mean.model=list(armaOrder=c(1,0), include.mean=TRUE, archm=TRUE, archpow=2),
  distribution.model="norm" )
garch.1022.n.o.fit <- ugarchfit(spec=garch.1022.n.o, solver = 'hybrid', data=fcr.ABC$fcr.ABC)
garch.1022.n.o.fit

### ARMA(1,0) - GARCH(2,2) with external features
garch.1022.e.o <- ugarchspec(
  variance.model=list(model="fGARCH", garchOrder=c(2,2),submodel="GARCH"),
  mean.model=list(armaOrder=c(1,0), include.mean=TRUE, archm=TRUE, archpow=2,
                  external.regressors=as.matrix(fcr.ABC[,c(3:8)])),
  distribution.model="norm" )
garch.1022.e.o.fit <- ugarchfit(spec=garch.1022.e.o, solver = 'hybrid', data=fcr.ABC$fcr.ABC)
garch.1022.e.o.fit

### Produce forecast for the next 5 trading days
garch.1012.n.o.forecast <- ugarchforecast(garch.1012.n.o.fit, n.ahead = 5)
c(fitted(garch.1012.n.o.forecast))
c(sigma(garch.1012.n.o.forecast)^2)


