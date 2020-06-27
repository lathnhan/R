## Data file is wagepanel.csv (see LMS/Data Files)
## You will need to install package called plm
## Don't forget to set the working directory!
#install.packages("plm")

## Part A
dt <-read.csv("wagepanel.csv")
library(AER)

##(A1) (following tute numbering) Standard OLS
tab.ols <-coeftest(lm(lwage~Educ+Exper+(I(Exper^2))+Married+Hisp+Black+Union,
                      data=dt), vcov=vcovHC)
print(tab.ols)

##(A2) Pooled OLS
# Create pooled dataframe having installed plm package
library(plm)
dtp <-pdata.frame(dt, index=c("ID","Year"))
tab.pols <-coeftest(plm(lwage~Educ+Exper+(I(Exper^2))+Married+Hisp+Black+Union, 
                        data=dtp, model="pooling"), vcov=vcovHC)
print(tab.pols)

##(A4) (following tute numbering) Fixed effects model
tab.fem <-coeftest(plm(lwage~Educ+Exper+(I(Exper^2))+Married+Hisp+Black+Union, 
                       data=dtp, model="within"), vcov=vcovHC)
print(tab.fem)

##(A5) Fixed effects model using individual dummy variables
id <-dtp$ID
tab.dfem <-coeftest(lm(lwage~0+Educ+Exper+(I(Exper^2))+Married+Hisp+Black+Union+id,
                       data=dt), vcov=vcovHC)
print(tab.dfem)
## (note similarity to fem coefs)

## Part B
##(B1) Fixed effects model with time effects only (add effect = "time")
tab.tfem <-coeftest(plm(lwage~Educ+Exper+(I(Exper^2))+Married+Hisp+Black+Union, 
                        data=dtp, model="within", effect="time"), vcov=vcovHC)
print(tab.tfem)
## (similar to before)

##(B2) Fixed effects model with time & individual effects (effect = "twoways")
tab.twfem <-coeftest(plm(lwage~Educ+Exper+(I(Exper^2))+Married+Hisp+Black+Union, 
                         data=dtp, model="within", effect="twoways"), vcov=vcovHC)
print(tab.twfem)

##(B3) Interact Educ and Year in twoways model
# Fixed effects model with time & individual effects (effect = "twoways")
tab.twfemEdYr <-coeftest(plm(lwage~Educ+Exper+(I(Exper^2))+(Educ*as.factor(Year))+Married+Hisp+Black+Union, 
                             data=dtp, model="within", effect="twoways"), vcov=vcovHC)
print(tab.twfemEdYr) 





library(texreg)

## regressions for question A1
htmlreg(list(tab.ols), file="a1.doc", 
        caption="Standard OLS", caption.above = TRUE, 
        custom.model.names = c("A1"), digits=4)

## regressions for question A2
htmlreg(list(tab.pols), file="a2.doc", 
        caption="Pooled OLS", caption.above = TRUE, 
        digits=4)

## regressions for question A4
htmlreg(list(tab.fem), file="a4.doc", 
        caption="(A4) Fixed effects", caption.above = TRUE, 
        digits=4)

## regressions for question A5
htmlreg(list(tab.dfem), file="a5.doc", 
        caption="(A5) Dummy fixed effects", caption.above = TRUE, 
        digits=4)

## regressions for question B1
htmlreg(list(tab.tfem), file="b1.doc", 
        caption="(B1) Time fixed effects", caption.above = TRUE, 
        digits=4)

## regressions for question B2
htmlreg(list(tab.twfem), file="b2.doc", 
        caption="(B2) Time and individual fixed effects", caption.above = TRUE, 
        digits=4)

## regressions for question B3
htmlreg(list(tab.twfemEdYr), file="b3.doc", 
        caption="(B3) Time and individual fixed effects with interaction", caption.above = TRUE, 
        digits=4)
