
## LOADING THE LIBRARY
library(quantmod)
library(moments)
library(PerformanceAnalytics)
library(tseries)
library(tidyverse)
library(xts)
library(randtests)
library(zoo)

###############################################################
# 1) LOADING THE DATA
ngseasi <- read.csv("C:/Users/hp/Desktop/GITHUB CODES/Weak EMH/NGSEASI.csv")
str(ngseasi)

###############################################################
# 2) DATA PREPROCESSING

### Converting factor Variable  to a  Date variable
ngseasi$Date<- as.Date(ngseasi$Date,format = "%m/%d/%Y")

### Converting the Volume variable from factor to Numeric
ngseasi$Vol. = as.numeric(ngseasi$Vol.) 

#
str(ngseasi)

##############################################################
# 3) CONVERTING FROM DATAFRAME TO XTS

## Save the Date Variable as Vector
period <- ngseasi$Date

### Removing Date Variable and Rearranging our Variables

colnames(ngseasi)

ngseasi = ngseasi %>%  select(Price, Open,High, Low, Vol.)

### Converting to zoo for time series Analysis
ngseasi <- zoo(ngseasi,order.by=period)

### Converting to Xts
ngseasi <- as.xts(ngseasi)


###############################################################
# 4) DESCRIPTIVE ANALYSIS

# trend in the price
chartSeries(ngseasi,theme=chartTheme('white'),
            name="Nigeria All Share Index Closing Price")

# summary of the daily price
table.Stats(ngseasi$Price,digits=3)

# Daily stock Returns
dr.ngseasi<-dailyReturn(ngseasi,type="log")
colnames(dr.ngseasi)<- "NGSEASI"

# Chart of Daily Returns: Volatility Clustering
chartSeries(dr.ngseasi,theme=chartTheme('white'),name="Volatility Clustering")

# Descriptive Summary of the daily return
table.Stats(dr.ngseasi,digits=7)


#### Density Distribution
chart.Histogram(dr.ngseasi,methods = "add.density",main = "Daily return of NGSEASI")

# Test for normality
jarque.test(as.vector(dr.ngseasi))

####################################################################
#5) Random walk Test

#a) Unit Root test
adf.test(ngseasi$Price, alternative = "stationary",k=1)

# Serial correlation test
# Lag = 1
Box.test(dr.ngseasi,lag=1,type=("Ljung-Box"))
# Lag = 4
Box.test(dr.ngseasi,lag=4,type=("Ljung-Box"))
# Lag = 9
Box.test(dr.ngseasi,lag=9,type=("Ljung-Box"))
# Lag = 16
Box.test(dr.ngseasi,lag=16,type=("Ljung-Box"))

# Autocorrelation Plot
chart.ACF(dr.ngseasi)


#b) Run test

runs.test(as.vector(dr.ngseasi),"two.sided")



