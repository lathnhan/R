##(1) To read in and display the data:
choc_df <-read.csv("ChocVsNobels.csv")
print(choc_df)

#Another way to display (part of) the data set
head(choc_df, 10) #display the first 10 lines/observations
tail(choc_df,10) #display the last 10 lines/observations

##(2) Matrix calculation (last exercise only because it covers all bases really!):
A1 <-rbind(1,2,3)
print(A1)
A2 <-rbind(6,7,8)
print(A2)
A <-cbind(A1,A2)
print(A)
B <-rbind(4,3,2)
print(B)
AAt <-A %*% t(A)
print(AAt)
BBt <-B %*% t(B)
print(BBt)
print(sumA2B2 <-AAt + BBt)

#An illustration of data frames in vector format in R
#Note that these data frames df1, df2, df3 all have the vector name as a
df1 <- data.frame(a=c(1,2))  
print(df1)
df2 <- data.frame(a=c(3,4))
print(df2)
df3 <- data.frame(a=c(5,6))
print(df3)
df123 <- rbind(df1,df2,df3)
print(df123)

#To perform other matrix calculations
At <- t(A) #A'
print(At)
Bt <- t(B) #B'
print(Bt)
sumAA <- A + A #A+A 
print(sumAA)
sumAB <- A + B #A+B
print(sumAB)
mulAB <- A %*% B #AB
print(mulAB)
mulABt <- A %*% Bt #AB'
print(mulABt)
mulBtA <- Bt %*% A #B'A
print(mulBtA)

##(3) For a basic histogram:
hist(choc_df$ChocPerCap)

##(4) For a fancier histogram:
hist(choc_df$ChocPerCap, main=" Chocolate Consumption",
     xlab = "kg/person/year", breaks=8, col="chocolate4")

##(5) For descriptive statistics .
print(summary(choc_df$ChocPerCap))
##(6) . and to fill in the missing standard deviation:
print(sd(choc_df$ChocPerCap))

##(7) To create a neat table of descriptive statistics:
statstable <- rbind(mean(choc_df$ChocPerCap),
                    median(choc_df$ChocPerCap),
                    sd(choc_df$ChocPerCap),
                    min(choc_df$ChocPerCap),
                    max(choc_df$ChocPerCap))
rownames(statstable)<-c("Mean", "Median", "SD", "Min", "Max")
colnames(statstable)<-"ChocPerCap"
print(round(statstable,3))

##(8) To save the table as an exportable csv file:
write.csv(statstable, file="statstable.csv")

##(9) To regress by OLS Nobels on ChocPerCap:
eq1 <-lm(Nobels~ChocPerCap, data = choc_df)
print(summary(eq1)) 


