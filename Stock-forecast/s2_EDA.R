#********************************************************************************
# Quantitative Research Test
# Portland House Research and Advisory
# Nhan La
#********************************************************************************


#********************************************************************************
# TASK 4.2: EXPLORATORY DATA ANALYSIS

## 1/ Set the working directory, load packages
setwd("D:/Portland House/Data")
library(tidyverse)

set.seed(123)

### Read the future close return data set prepared in part 4.1 for EDA. We will use this file for EDA
fcr <- read.csv("fcr.txt", header=TRUE)

### Specify functions to be used
#### Functions: Creating histogram and boxplot graphs
eda.3.h <- function(df, z){
  df %>%
    ggplot(aes(!!sym(z))) + 
    geom_histogram(aes(y =..density..), 
                   binwidth = 0.004,
                   col="red", 
                   fill="green", 
                   alpha=.2) + 
    geom_density(col=2) + 
    labs(title="Future close return histograms", x=z, y="")
}

eda.3.b <- function(df, z){
  df %>%
    ggplot(aes(x="",y=!!sym(z))) + 
    geom_boxplot(fill="green") +
    labs(title="Future close return boxplots", x=z, y="")
}

#### Function: Create TNE time series line graphs
eda.4.p <- function(df,i,j){
  ggplot(data=df, aes(date, !!sym(i))) + 
    geom_line() + labs(title=j,x="Date",y="")
}

## 2/ Investigate missing data
## Count the frequencies of missing values for each feature in the dataset
missing.values <- fcr %>%
  gather(key = "key", value = "val") %>%
  mutate(is.missing = is.na(val)) %>%
  group_by(key, is.missing) %>%
  summarise(num.missing = n()) %>%
  filter(is.missing==TRUE) %>% #Depending, may be coded as T/F
  select(-is.missing) %>%
  arrange(desc(num.missing)) 

## Visualize missing data
### Visualize the frequency of missing values in the data set above
missing.values %>%
  ggplot() +
  geom_bar(aes(x=reorder(key,num.missing), y=num.missing), stat = 'identity') +
  labs(x='Tickers', y="Number of missing values", title='Future close returns: Number of missing values') +
  #theme(axis.text.x = element_text(angle = 45, hjust = 1))
  theme(axis.text.x = element_blank())

### Visualize the percentage of missing values
missing.values.pct <- fcr %>%
  gather(key = "key", value = "val") %>%
  mutate(isna = is.na(val)) %>%
  group_by(key) %>%
  mutate(total = n()) %>%
  group_by(key, total, isna) %>%
  summarise(num.isna = n()) %>%
  mutate(pct = num.isna / total * 100)

levels <- (missing.values.pct  %>% filter(isna == T) %>% arrange(desc(pct)))$key

percentage.plot <- missing.values.pct %>%
  ggplot() +
  geom_bar(aes(x = reorder(key, desc(pct)), 
               y = pct, fill=isna), 
           stat = 'identity', alpha=0.8) +
  scale_x_discrete(limits = levels) +
  scale_fill_manual(name = "", 
                    values = c('steelblue', 'tomato3'), labels = c("Present", "Missing")) +
  coord_flip() +
  labs(title = "Future close returns: Percentage of missing values", x =
         'Tickers', y = "% of missing values")+
  theme(axis.text.y = element_blank())
percentage.plot

### Visualize the missing values over time
row.plot <- fcr %>%
  mutate(id = row_number()) %>%
  gather(-id, key = "key", value = "val") %>%
  mutate(isna = is.na(val)) %>%
  ggplot(aes(key, id, fill = isna)) +
  geom_raster(alpha=0.8) +
  scale_fill_manual(name = "",
                    values = c('steelblue', 'tomato3'),
                    labels = c("Present", "Missing")) +
  scale_x_discrete(limits = levels) +
  labs(x = "Tickers",
       y = "Time", title = "Future close return: Missing values over time") +
  coord_flip()+
  theme(axis.text.y = element_blank())
row.plot

### Many tickers in this data set have significant extents of missing data. Given the time constraint for this excercise,
### I will only focus on the tickers with full data. More generally, I will pick the tickers with
### relatively less issues. If time permits, I will try several measures to investigate the missingness
### issue and conduct measures. I will see first if the missingness is random or not random. Tickers whose data
### are missing due to non-random reasons, such as merging and acqusition with other tickers, would be better taken
### out of consideration. For random missingness, we can impute the missing values using forward filling.

### Select stocks with full data
fcr.full.list <- missing.values %>%
  filter(num.missing==1) %>%
  select(key)

### Sample 4 random stocs for the next investigation of outliers
sample(1:196, 4, replace=FALSE)
fcr.full.list$key[4]
fcr.full.list$key[152]
fcr.full.list$key[56]
fcr.full.list$key[127]
fcr.eda <- fcr %>%
  select(date, fcr.ABC, fcr.TNE, fcr.GDI, fcr.RIC)

## 3/ Investigate outliers from the subset of data without missingness
### Convert data to time series
fcr.eda <- fcr.eda %>%
  mutate(date=ymd(date))%>%
  as_tsibble(index=date)

### Plot series
fcr.eda %>% autoplot(fcr.ABC)
fcr.eda %>% autoplot(fcr.TNE) 
fcr.eda %>% autoplot(fcr.GDI)
fcr.eda %>% autoplot(fcr.RIC)

### From the first outlook, these time series seem not be subject to a substantial issue of outliers. 
### Let's take another look at the plots of these series, including histograms and boxplots

#### Histogram and boxplot graphs
h.ABC <- eda.3.h(fcr.eda,"fcr.ABC")
h.TNE <- eda.3.h(fcr.eda,"fcr.TNE")
h.GDI <- eda.3.h(fcr.eda,"fcr.GDI")
h.RIC <- eda.3.h(fcr.eda,"fcr.RIC")
multiplot(h.ABC, h.TNE, h.GDI, h.RIC, cols=2)

b.ABC <- eda.3.b(fcr.eda,"fcr.ABC")
b.TNE <- eda.3.b(fcr.eda,"fcr.TNE")
b.GDI <- eda.3.b(fcr.eda,"fcr.GDI")
b.RIC <- eda.3.b(fcr.eda,"fcr.RIC")
multiplot(b.ABC, b.TNE, b.GDI, b.RIC, cols=2)

jarque.bera.test(na.omit(fcr.eda$fcr.ABC))

### At this step, we will choose the ticker TNE for our further exploratory data analysis and forecast modelling.
### As I plan to use other features in building the model, it is useful to take a look at the properties
### of other variables of this ticker. 

### Create a data frame consisting of time series variables of TNE, save the data set
open.ABC <- read.csv("open.wide.txt", header=TRUE) %>% select(open.ABC)
hl.ABC <- read.csv("hl.txt", header=TRUE) %>% select(hl.ABC)
volume.ABC <- read.csv("volume.wide.txt", header=TRUE) %>% select(volume.ABC)
fcr.ABC <- fcr %>%
  select(date, fcr.ABC) %>%
  #mutate(date=ymd(date))%>%
  mutate(open.ABC = open.ABC$open.ABC) %>%
  mutate(hl.ABC = hl.ABC$hl.ABC) %>%
  mutate(volume.ABC = volume.ABC$volume.ABC)

### Verify the missing value of these series
missing.ABC <- fcr.ABC %>%
  gather(key = "key", value = "val") %>%
  mutate(is.missing = is.na(val)) %>%
  group_by(key, is.missing) %>%
  summarise(num.missing = n()) %>%
  filter(is.missing==TRUE) %>% #Depending, may be coded as T/F
  select(-is.missing) %>%
  arrange(desc(num.missing))   

### Convert date to date format
fcr.ABC <- fcr.ABC %>%
  mutate(date=ymd(date)) #library lubridate
  
## 4/ Investigate basic properties of the time series.
### The main purpose of this step is to examine the properties of time series to see whether they are ready to be used
### for forecast modelling and, if not, perform necessary measures. The focus will be on the
### examination of the condition of stationarity of times series in relation to the ticker XKO that
### we identified in the previous step.

### Prepare the data
#fcr.ABC$date <- as.Date(fcr.abc$date, "%Y-%m-%d")
fcr.text <- c("Future close return")
open.text <- c("Open price")
hl.text <- c("High-low ratio")
volume.text <- c("Trade volume")

### Plot series
plot(decompose(ts(fcr.ABC[,"fcr.ABC"],frequency = 252)))
p.ABC.fcr <- eda.4.p(fcr.ABC,"fcr.ABC",fcr.text)
p.ABC.open <- eda.4.p(fcr.ABC,"open.ABC",open.text)
p.ABC.hl <- eda.4.p(fcr.ABC,"hl.ABC",hl.text)
p.ABC.volume <- eda.4.p(fcr.ABC,"volume.ABC",volume.text)
multiplot(p.ABC.open, p.ABC.hl, p.ABC.volume, cols=2)

### Check autocorrelation and partial autocorrelation coefficients
acf(na.omit(fcr.ABC$fcr.ABC), main="Future close return")
pacf(na.omit(fcr.ABC$fcr.ABC), main="Future close return")
Box.test(na.omit(fcr.ABC$fcr.ABC), lag=40, type="Ljung-Box")


### Perform the Augmented Dickey-Fuller test to test for the unit roots
adf.test(na.omit(fcr.ABC$fcr.ABC))
adf.test(na.omit(fcr.ABC$open.ABC))
adf.test(na.omit(fcr.ABC$hl.ABC))
adf.test(na.omit(fcr.ABC$volume.ABC))

#### Only the Open price series shows the presence of a unit root, hence implying nonstationarity. We will
#### create a differencing series to make it stationary

fcr.ABC$d.open.ABC=c(NA,diff(fcr.ABC$open.ABC))
adf.test(na.omit(fcr.ABC$d.open.ABC))
d.open.ABC.p <- ggplot(data=fcr.ABC, aes(date, d.open.ABC)) + 
  geom_line() + labs(title="Differencing open price",x="Date",y="")

#### Drop the original Open price series.
fcr.ABC <- fcr.ABC %>%
  select(-open.ABC) 

# END OF TASK 4.2: EXPLORATORY DATA ANALYSIS
#********************************************************************************






