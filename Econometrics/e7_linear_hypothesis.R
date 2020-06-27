## Note: don't forget to set your working directory before running the code!

## Assignment marks vs group size
mx <-read.csv("groups.csv")
library(AER)
##(1) Regressions for Part A:
eq1_mx <-lm(Mark~0+G1+G2+G3+G4, data=mx)
print(summary(eq1_mx))
eq2_mx <-lm(Mark~G2+G3+G4, data=mx)
print(summary(eq2_mx))

##(2) Tests for Part B: 
# Test for no group size effect on marks (eq1_mx):
print(linearHypothesis(eq1_mx, c("G1-G2=0","G2-G3=0","G3-G4=0"), 
                       test="Chisq", vcov=vcovHC(eq1_mx)))
# Test for no group size effect on marks (eq2_mx):
print(linearHypothesis(eq2_mx, c("G2=0","G3=0","G4=0"), 
                       test="Chisq", vcov=vcovHC(eq2_mx)))
