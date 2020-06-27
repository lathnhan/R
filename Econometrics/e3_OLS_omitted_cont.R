## First install the AER package (Packages, Install, AER) and call it up for use:
library(AER)

## Read in the data:
bw <-read.csv("bwght.csv")

##(1) For OLS regression:
eq1 <-lm(log(bwghtlbs)~packs, data=bw)
print(summary(eq1))

##(2) For 2SLS regression, using cigprice as an IV for packs:
eq2 <-ivreg(log(bwghtlbs)~packs|cigprice, data=bw)
print(summary(eq2))

##(3) To check whether cigprice is relevant for packs:
eq3 <-lm(packs~cigprice, data=bw)
print(summary(eq3))

## (4) To compare OLS results based on kilos and pounds:
bwghtkg <-bw$bwghtlbs/2.2
eq4 <-lm(log(bwghtkg)~packs, data=bw)
print(summary(eq4))

#check conversion and outputs
con_cept <- eq1$coeff[1] - eq4$coeff[1] - log(2.2)
print(con_cept)
