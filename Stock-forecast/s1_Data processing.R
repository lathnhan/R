#********************************************************************************
# Quantitative Research Test
# Portland House Research and Advisory
# Nhan La
#********************************************************************************


#********************************************************************************
# TASK 4.1: DATA PROCESSING

## 1/ Set the working directory, load packages
setwd("D:/Portland House/ASX-2015-2018")
library(tidyverse)
###library(plyr)

## 2/ Specify functions to be used
### Function: Create separate data frames
asx.df <- function(x){
  select(asx,ticker,date,x)
}

### Function: Reshape data frames to the wide format as described in the question
asx.reshape <- function(x){
  reshape(x,
          timevar="ticker",
          idvar="date",
          direction="wide")
}

### Function: Save data files
asx.write <- function(df,x){
  write.csv(df,file=paste(x,".txt"),row.names=FALSE)
}

## 3/ Read txt files and create data frames
files = list.files(pattern="*.txt")
asx = ldply(files, read_csv,col_names=c("ticker","date","open","high", "low", "close", "volume"))
asx <- asx[order(asx$date),]

open.long <- asx.df(3)
high.long <- asx.df(4)
low.long <- asx.df(5)
close.long <- asx.df(6)
volume.long <- asx.df(7)

open.wide <- asx.reshape(open)
high.wide <- asx.reshape(high)
low.wide <- asx.reshape(low)
close.wide <- asx.reshape(close)
volume.wide <- asx.reshape(volume)

## 4/ Create return, future return, high/low ratio data sets
### Return data set
f.lag <- function(x) lag(x,1)
close.lag <- data.frame(close.wide[1],apply(close.wide[2:2774],2,f.lag))
names(close.lag) <- sub(".*\\.", "lag.", names(close.lag))

close.return <- close.lag
names(close.return) <- sub(".*\\.", "cr.", names(close.return))
for (i in 2:ncol(close.lag)){
    close.return[i] <- data.frame(close.wide[i]/close.lag[i]-1)
}

### Future return data set
f.future <- function(x) lead(x,1)
close.future <- data.frame(close.wide[1],apply(close.wide[2:2774],2,f.future))
names(close.future) <- sub(".*\\.", "ftr.", names(close.future))

fcr <- close.future
names(fcr) <- sub(".*\\.", "fcr.", names(fcr))
for (i in 2:ncol(close.future)){
  fcr[i] <- data.frame(close.future[i]/close.wide[i]-1)
}

### High/low ratio data set
hl <- high_ts
names(hl) <- sub(".*\\.", "hl.", names(hl))
for (i in 2:ncol(hl)){
  hl[i] = high_ts[i]/low_ts[i]
}

## 5/ Save data frames
asx.write(open.wide, "open.wide")
asx.write(high.wide, "high.wide")
asx.write(low.wide, "low.wide")
asx.write(volume.wide, "volume.wide")
asx.write(hl, "hl")
asx.write(close.wide, "close.wide")
asx.write(close.lag, "close.lag")
asx.write(close.future, "close.future")
asx.write(close.return, "close.return")
asx.write(fcr, "fcr")

# END OF TASK 4.1: DATA PROCESSING
#********************************************************************************

