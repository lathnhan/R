## R-code for tute 12
## Read in data file gdp.csv
## and set working directory.
## Make sure to have installed packages:
## dynlm, CADFtest, vars

## Part A
t12 <-read.csv("gdp.csv")
# Package
library(dynlm)
gdp.au <-ts(t12$aus, start=c(1970, 1), end=c(2000, 4), frequency=4)
gdp.us <-ts(t12$usa, start=c(1970, 1), end=c(2000, 4), frequency=4)
n <-length(gdp.au)
##(1) Plot both GDPs on same graph:
ts.plot(gdp.au, gdp.us, col=c("black", "red"))
##(2) Create and fit linear time trends and choose between linear and log:
t <-1:n
eq.au <-dynlm(gdp.au~t)
ts.plot(gdp.au, eq.au$fitted.values, col=c("black", "blue"))
eq.us <-dynlm(gdp.us~t)
ts.plot(gdp.us, eq.us$fitted.values, col=c("black", "green"))
##(3) Do same for logs:
lgdp.au <-log(gdp.au)
lgdp.us <-log(gdp.us)
eq.au.log <-dynlm(lgdp.au~t)
ts.plot(lgdp.au, eq.au.log$fitted.values, col=c("black", "brown"))
eq.us.log <-dynlm(lgdp.us~t)
ts.plot(lgdp.us, eq.us.log$fitted.values, col=c("black", "purple"))
## (Plots show that logged GDP series closer to linear)

##(4) Unit root testing
library(CADFtest)
# Use "trend" type
df.lgdp.au <-CADFtest(lgdp.au, type="trend", criterion="MAIC", max.lag.y=12)
df.lgdp.us <-CADFtest(lgdp.us, type="trend",criterion="MAIC", max.lag.y=12)
print(df.lgdp.au)
print(df.lgdp.us)
## (Both series have trends and unit roots)

## Part B
##(5) Create difference between log GDPs (Aus minus USA)
e <-lgdp.au-lgdp.us
## (this is the so-called error correction term)
ts.plot(e)
# UR test e with intercept only
df.e <-CADFtest(e, type="drift", criterion="MAIC", max.lag.y=12)
print(df.e)

##(6) Select lag length for VECM from levels VAR
# (see week 10 lecture slides)
library(vars)
# Limit size of VAR by reducing max.lag (and include both intercept and trend)
IC <-VARselect(cbind(lgdp.au, lgdp.us), type="both", lag.max=8)
print(IC$selection[1])
## (lag length of 6 selected)

##(7) Estimate VECM
# Generate diffs (in this case, growth rates)
g.au <-diff(log(gdp.au))
g.us <-diff(log(gdp.us))
# Estimate VECM regressions
eq.ec.au <-dynlm(g.au~L(e, 1)+L(g.au, 1:5)+L(g.us,1:5))
eq.ec.us <-dynlm(g.us~L(e, 1)+L(g.au, 1:5)+L(g.us,1:5))
print(summary(eq.ec.au))
print(summary(eq.ec.us))
