#From handling evil spreadsheets
#dt <- read.csv("evil.csv")
#print(summary(lm(Evil~Gender, data=dt)))
#dt$Male <- (dt$Gender=="M")*1 #create variable Male which has value 1 if Gender == M
#dt$Random[dt$Random=="."] <- NA #assign value . of Random with NA 
#dt$Random <- as.numeric(as.character(dt$Random)) #change variable type from character to numeric
#dt <- dt[which(!is.na(dt$Random)),] #select observations for which Random isn't NA
#print(summary(lm(Evil~Male+log(Random),data=dt)))


## First, because we want to present our regression results neatly in a single table 
## (as required for full marks in assignments!) install the package texreg 
## (Packages, Install, texreg) and call it up for use:
installed.packages("texreg")
library(texreg)

## Also call up AER (for 2SLS estimation):
library(AER)

## Read in data:
df1 <-read.csv("mroz.csv")

## (1) Cleaning up the data (see "Handling evil spreadsheets" video)
## (1a) Set hours=0 cases to NA:
df1$hours[df1$hours=="0"] <-NA
## (1b) Now create a new dataframe (called df) that omits all NA cases:
df <-na.omit(df1)
## (1c) Change wage to numeric (right justified):
df$wage <-as.numeric(as.character(df$wage))

## We now have what's required: 
## a workable dataframe for the 428 employed married women

## (2) For 2SLS (IV) estimation of the hours equation:
eq1 <-ivreg(hours~log(wage)+age+kidslt6+nwifeinc+
              educ|age+kidslt6+nwifeinc+educ+exper+expersq, data=df)
print(summary(eq1))

## It is worth noting that the instrumental variables (IVs), 
## which follow the "|" in the command line above, 
## comprise *all* the exogenous variables in the system of equations
## forming the demand/supply model (namely, the hours and log(wage) equations); 
## one can think of the log(wage) equation as an inverse labour demand equation 
## (i.e., price in terms of quantity). Hence, we use *all* the exogenous variables 
## in the system as IVs, not just the ones in the hours equation.

## (3) For 2SLS (IV) estimation of the hours equation 
## with endogenous education 'instrumented' by parents' education 
## (fatheduc and motheduc):
eq2 <-ivreg(hours~log(wage)+age+kidslt6+nwifeinc+
              educ|age+kidslt6+nwifeinc+fatheduc+motheduc+exper+expersq, data=df)
print(summary(eq2))

## (4) To present both sets of regression results (equations 1 and 2) 
## neatly in a single table (as required in assignments!), try this:
htmlreg(list(eq1, eq2), file="tute4regs.doc", 
        caption="Estimated labour supply equations for tute 4", caption.above = TRUE, 
        custom.model.names = c("Exogenous education", "Endogenous education"), digits=3)

## This should create a Word doc table of regression results called tute4regs,
## which you should see in the Files window of R-Studio.
## You can open this in Word and do further editing there if you wish.
## You might have to experiment with other file extensions depending on your computer:
## e.g., file="tute4regs.docx" might work better;
## or you might try "tute4regs.html" (though I'm not used to this!)

## (5) If the above does not work, something simpler you can try is:
print(screenreg(list(eq1,eq2)))

## which presents the regressions side by side in the R-Studio output window,
## and is something you can copy/paste into Word and edit further if you wish
