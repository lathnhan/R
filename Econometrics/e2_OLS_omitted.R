## Read in data:
choc_df <-read.csv("ChocVsNobels.csv")

##(1) OLS regressions, Part A (Q2)
eq1 <-lm(Nobels~ChocPerCap, data=choc_df) #sigma
eq2 <-lm(Nobels~ChocPerCap+GDPPerCap, data=choc_df)
print(summary(eq1))
print(summary(eq2))

##(2) To eliminate the 'e number' from the second regression's results:
eq2a <-lm(Nobels~ChocPerCap+I(GDPPerCap/1000), data=choc_df) #beta1, beta2
print(summary(eq2a))

##(3) Also useful for Part B (2b) is ...
eq3 <-lm(GDPPerCap~ChocPerCap, data=choc_df)
print(summary(eq3))
##(4) ... or, consistent with eq2a:
eq3a <-lm(I(GDPPerCap/1000)~ChocPerCap, data=choc_df) #gamma
print(summary(eq3a))

##(5) Matrix exercise (Part A)
A1 <-rbind(1,2)
A2 <-rbind(2,5)
A <-cbind(A1,A2)
print(A)
B1 <-rbind(1,2)
B2 <-rbind(2,4)
B <-cbind(B1,B2)
print(B)
print(det(A))
print(det(B))
print(solve(A))
X1 <-rbind(1,5)
X2 <-rbind(1,-2)
X3 <-rbind(1,3)
X12 <- X1 %*% t(X1)
X22 <- X2 %*% t(X2)
X32 <- X3 %*% t(X3)
sum <-X12 + X22 + X32
print(sum)
inv <-solve(sum)
print(inv)
