#----------------------------------------------------------------------------------------------------------
############# Business Understanding ###############
# Global Mart is an online store whi takes orders and delivers across the globe and deals with
# all major product categories - consumer, corporate and home office. 
# We want to forcast the sales and the demand for the next 6 months.
#----------------------------------------------------------------------------------------------------------
# Set Working Directory

# Required packages
library(tidyr)
library(dplyr)
library(raster)
library(lubridate)
library(graphics)
library(tseries)
library(forecast)

# Loading data file
global_superstore <- read.csv("Global Superstore.csv")

# structure of data
str(global_superstore)

# Limiting variables in dataset which are more important to analysis
global_superstore_final <- global_superstore[c(3,8,13,16,19,20,21,22)]

# Segment and Market factors level
unique(global_superstore_final$Segment)
unique(global_superstore_final$Market)

#---------------------------------------------------------------------------------------------------------------------------
############# DATA UNDERSTANDING ########################
# the data has transactional level data, where each row represents particular order made on online mode.
# There are 24 attributes related to each trnasaction.
# The Market attribute has 7 factor level viz. APAC, Africa, LATM, US, EMEA, EU, Canada representing the geographical market sector.
# The Segment attribute tells which of the 3 segements i.e.Consumer, Home Office, Corporate that customers belongs to.
#-----------------------------------------------------------------------------------------------------------------------------
# Data Cleaning
# Setting the OrderDate in proper format
global_superstore_final$Order.Date <- as.Date(global_superstore_final$Order.Date,"%d-%m-%Y")

# Structure of final model
str(global_superstore_final)

#checking for NA values in the DS
sapply(global_superstore_final,function(x) sum(is.na(x)))

#Checking for duplicate rows in DS
sum(duplicated(global_superstore_final))

# Checking for blank values
sapply(global_superstore_final$Segment, function(x) length(which(x == "")))
sapply(global_superstore_final$Market, function(x) length(which(x == "")))
sapply(global_superstore_final$Sales, function(x) length(which(x == "")))
sapply(global_superstore_final$Category, function(x) length(which(x == "")))
sapply(global_superstore_final$Quantity, function(x) length(which(x == "")))
sapply(global_superstore_final$Discount, function(x) length(which(x == "")))

sapply(global_superstore_final$Profit, function(x) length(which(x == "")))
#------------------------------------------------------------------------------------------------------------
# Data preparation:

#1. We need to first segment the whole dataset into 21 subsets based on market and customer segment level. 
#2. To convert the transaction-level data into a time series. 
# For this we need to aggregate the 3 attributes  - Sales, Quantity & Profit
# over the Order Date to arrive at monthly values for these attributes for each market segment.
#3. After arriving at these 3 time series for each of the 21 segement, 2 most profitable segements needs to selected
#---------------------------------------------------------------------------------------------------------------------- 

# subsetting year and month from Order.Date Column. 

global_superstore_final$Order_year = year(global_superstore_final$Order.Date)
global_superstore_final$Order_month = month(global_superstore_final$Order.Date)

# We derive a new column Month_year_seq which starts with 1 for the earliest date and increments by 1 for
# every month from the earliest date.
Month_year_order <- global_superstore_final$Order_year*12 + global_superstore_final$Order_month

Min_month_year_order <- min(Month_year_order)
Month_year_seq <- Month_year_order - Min_month_year_order +1
global_superstore_final$Month_seq = Month_year_seq
global_superstore_final <- global_superstore_final[order(global_superstore_final$Month_seq),c(1:11)]


# Aggregating Sales,Quantity & Profit over Order.Date to arrive at monthly values
# for all combinations of Market and Segments.

global_superstore_agg <- aggregate(global_superstore_final[,c("Sales","Quantity","Profit")],
                            by=list(global_superstore_final$Market,global_superstore_final$Segment,
                                    global_superstore_final$Month_seq), FUN = sum)

colnames(global_superstore_agg)[1] <- c("Market")
colnames(global_superstore_agg)[2] <- c("Segment")
colnames(global_superstore_agg)[3] <- c("Month_Seq")

# For identification of market segments that are consistently profitable and profits with least variance
# we need to calculate sum of profit and variance for each market segment.

# aggregate (sum) profit for each month for each market segment
global_superstore_profit_agg <- aggregate(global_superstore_agg[,c("Profit")],
                                    by=list(global_superstore_agg$Market,global_superstore_agg$Segment)
                                    ,FUN = sum) 

colnames(global_superstore_profit_agg)[1]<-c("Market")
colnames(global_superstore_profit_agg)[2]<-c("Segment")
colnames(global_superstore_profit_agg)[3]<-c("Profit_sum")

#aggregate coefficnet of variation(cv) for each month for each market segment
global_superstore_cv_agg <-aggregate(global_superstore_agg[,c("Profit")],
                                by=list(global_superstore_agg$Market,global_superstore_agg$Segment)
                                ,FUN = cv)

colnames(global_superstore_cv_agg)[1]<-c("Market")
colnames(global_superstore_cv_agg)[2]<-c("Segment")
colnames(global_superstore_cv_agg)[3]<-c("Profit_cv")

# merging global superstore_profit_agg and global_superstore_cv_agg
global_superstore_agg_merge <- merge(global_superstore_profit_agg, global_superstore_cv_agg)


# First two rows with highest profit
head(global_superstore_agg_merge[order(-global_superstore_agg_merge$Profit_sum),c(1:2)],2)
# Market Segement is APAC Consumer and EU Consumer

# First two rows with least coefficient of variation
head(global_superstore_agg_merge[order(global_superstore_agg_merge$Profit_cv),c(1:2)],2)
# Market Segement is APAC Consumer and EU Consumer

# subsetting global_superstore_agg for APAC market and Consumer segementaion 
APAC_consumer_agg<-subset(global_superstore_agg,global_superstore_agg$Market=="APAC" & global_superstore_agg$Segment=="Consumer")

# subsetting global superstore_agg for EU market and Consumer segemntation
EU_consumer_agg<-subset(global_superstore_agg,global_superstore_agg$Market=="EU" & global_superstore_agg$Segment=="Consumer")



# Top 2 profitable Market-Segment are
# 1. APAC Consumer
# 2. EU Consumer


########## MODEL BUILDING FOR APAC-CONSUMER AND EU-CONSUMER ################
###Building the Time Series for APAC-Consumer 

ylab <- c("Sales")
xlab<-c("Month")
title<-c("Sales from Jan 2011 to Dec 2014")
xcol <- c(1)
ycol <- c(2)

# There are 48 unique months and Out of that , first 42 months will be used to train the model.
# and the rest 6 months to test the model.

total_timeser_APAC_sales <- ts(APAC_consumer_agg$Sales)
indata_apac_sales_train <- APAC_consumer_agg[1:42,]
timeser_apac_sales <- ts(indata_apac_sales_train$Sales)
plot(timeser_apac_sales)

# Time Series plot has moslty trend , Smoothing the series - Moving Average Smoothing
w <-3
smoothedseries_apac_sales <- stats::filter(timeser_apac_sales, 
                                             filter=rep(1/(2*w + 1),(2*w + 1)), 
                                             method='convolution', sides=2)	

# Smoothing left end of the time series
diff <- smoothedseries_apac_sales[w+2] - smoothedseries_apac_sales[w+1]
for (i in seq(w,1,-1)) {
  smoothedseries_apac_sales[i] <- smoothedseries_apac_sales[i+1] - diff
}

# Smoothing right end of the time series
n <- length(timeser_apac_sales)
diff <- smoothedseries_apac_sales[n-w] - smoothedseries_apac_sales[n-w-1]
for (i in seq(n-w+1, n)) {
  smoothedseries_apac_sales[i] <- smoothedseries_apac_sales[i-1] + diff
}

#Plot the smoothed time series
timevals_in_apac_sales <- indata_apac_sales_train$Month_Seq
lines(smoothedseries_apac_sales, col="blue", lwd=2)

#Building a model on the smoothed time series using classical decomposition
#First, let's convert the time series to a dataframe
smootheddf_apac_sales <- as.data.frame(cbind(timevals_in_apac_sales, as.vector(smoothedseries_apac_sales)))
colnames(smootheddf_apac_sales) <- c('Month', 'Sales')


#Now, let's fit a multiplicative model with trend and seasonality to the data
#Seasonality will be modeled using a sinusoid function
lmfit_apac <- lm(Sales ~ sin(0.5*Month) * poly(Month,3) + cos(0.5*Month) * poly(Month,3)
            +             + Month, data=smootheddf_apac_sales)
global_pred_apac <- predict(lmfit_apac, Month=timevals_in_apac_sales)
summary(global_pred_apac)
lines(timevals_in_apac_sales, global_pred_apac, col='red', lwd=2)

# Now, let's look at the locally predictable series
# We will model it as an ARMA series

local_pred_apac_sales <- timeser_apac_sales - global_pred_apac
plot(local_pred_apac_sales,col="red",type='l')
acf(local_pred_apac_sales)
acf(local_pred_apac_sales,type = "partial")
armafit <- auto.arima(local_pred_apac_sales)

armafit
# Result:
## Series: local_pred_apac_sales 
## ARIMA(0,0,0) with zero mean 
## sigma^2 estimated as 101931784:  log likelihood=-446.83
## AIC=895.66   AICc=895.76   BIC=897.4

# We'll check if the residual series is white noise
resi_apac_sales<-local_pred_apac_sales - fitted(armafit)
adf.test(resi_apac_sales,alternative = "stationary")

# Result: 
# Augmented Dickey-Fuller Test
# data:  resi_apac_sales
# Dickey-Fuller = -5.1102, Lag order = 3, p-value = 0.01 
# p-value is less than 0.05 hence it rejects null hypothesis, hence the timeseries is stationary
# alternative hypothesis: stationary

kpss.test(resi_apac_sales)
# Result: 
# KPSS Test for Level Stationarity
# data:  resi_apac_sales
# KPSS Level = 0.025409, Truncation lag parameter = 1, p-value = 0.1
# p-values is more than 0.05 then it failes to reject the null hypothesis, hence the timeseries is stationary


# Now, let's evaluate the model using MAPE
# First, let's make a prediction for the last 6 months
outdata_apac_sales <- APAC_consumer_agg[43:48,]
timevals_out_apac_sales <- outdata_apac_sales$Month_Seq
global_pred_out_apac_sales <- predict(lmfit_apac, data.frame(Month=timevals_out_apac_sales))

fcast_apac_sales <- global_pred_out_apac_sales

#Now, let's compare our prediction with the actual values, using MAPE
MAPE_class_dec_apac_sales <- accuracy(fcast_apac_sales,outdata_apac_sales[,4])[5]
MAPE_class_dec_apac_sales
# Result: 37.3835

#Let's also plot the predictions along with original values, to
#get a visual feel of the fit
class_dec_pred_apac_sales <- c(ts(global_pred_apac),ts(global_pred_out_apac_sales))
plot(total_timeser_APAC_sales, col = "black")
lines(class_dec_pred_apac_sales, col = "red")

# changing the degree polynomial to 2 and then predicting

lmfit_apac_1 <- lm(Sales ~ sin(0.5*Month) * poly(Month,2) + cos(0.5*Month) * poly(Month,2)
              +             +             + Month, data=smootheddf_apac_sales)
global_pred_apac_1 <- predict(lmfit_apac_1, Month=timevals_in_apac_sales)
lines(timevals_in_apac_sales, global_pred_apac_1, col='green', lwd=2) 
local_pred_apac_sales_1 <- timeser_apac_sales - global_pred_apac_1

plot(local_pred_apac_sales_1,col="red",type='l')
acf(local_pred_apac_sales_1,type = "partial")
acf(local_pred_apac_sales_1) ### acf of the series shows only spike and all other falling within the acceptable range.
armafit_1 <- auto.arima(local_pred_apac_sales_1)
##checking the armafit
armafit_1
# Result:
# Series: local_pred_apac_sales 
# ARIMA(0,0,0) with zero mean 
# sigma^2 estimated as 104589005:  log likelihood=-447.37
# AIC=896.74   AICc=896.84   BIC=898.48 ###AIC,BIC and log likelihood will be comapred with the Auto Arima Model

# peforming the dickey fuller and kpss test to check for stationarity
resi_apac_sales_1<-local_pred_apac_sales_1 - fitted(armafit_1)
adf.test(resi_apac_sales_1,alternative = "stationary")
# Result:
# Augmented Dickey-Fuller Test
# data:  resi_apac_sales_1
# Dickey-Fuller = -4.7612, Lag order = 3, p-value = 0.01
# alternative hypothesis: stationary
kpss.test(resi_apac_sales_1)
# Result:
# KPSS Test for Level Stationarity
# data:  resi_apac_sales_1
# KPSS Level = 0.029775, Truncation lag parameter = 1, p-value = 0.1
# evaluating the model on the last 6 months of data
global_pred_out_apac_sales_1 <- predict(lmfit_apac_1,data.frame(Month=timevals_out_apac_sales))
fcast_apac_sales_1 <- global_pred_out_apac_sales_1

# evaluating the model by the help of MAPE
MAPE_class_dec_apac_sales_1 <- accuracy(fcast_apac_sales_1,outdata_apac_sales[,4])[5]
MAPE_class_dec_apac_sales_1
# 27.71026
# MAPE of the model has considerably improved
class_dec_pred_apac_sales_1 <- c(ts(global_pred_apac_1),ts(global_pred_out_apac_sales_1))
plot(total_timeser_APAC_sales, col = "black")
lines(class_dec_pred_apac_sales_1, col = "blue")

# Now predicting sales using ARIMA model

autoarima_apac_sales<-auto.arima((timeser_apac_sales))
autoarima_apac_sales
# Result:
# Series: (timeseries_apac_sales) 
# ARIMA(0,1,1) 
# Coefficients:
# ma1
# -0.7559
# s.e.   0.1381
# sigma^2 estimated as 174361555:  log likelihood=-447.11
# AIC=898.23   AICc=898.55   BIC=901.66

plot(autoarima_apac_sales$x,col="black")
lines(fitted(autoarima_apac_sales), col="red")

# Again, let's check if the residual series is white noise
resi_autoarima_apac_sales <- timeser_apac_sales-fitted(autoarima_apac_sales)

# Dickey -Fuller Test 
adf.test(resi_autoarima_apac_sales,alternative = "stationary")
# Result:
# Augmented Dickey-Fuller Test
# data:  resi_arima_apac_sales
# Dickey-Fuller = -4.2563, Lag order = 3, p-value = 0.01 #p <0.05; hence stationary
# alternative hypothesis: stationary

kpss.test(resi_autoarima_apac_sales)
# Result:
# KPSS Test for Level Stationarity
# data:  resi_arima_apac_sales
# KPSS Level = 0.042734, Truncation lag parameter = 1, p-value = 0.1

fcast_autoarima_apac_sales <- predict(autoarima_apac_sales,n.ahead=6)
Mape_autoarima_apac_sales <- accuracy(fcast_autoarima_apac_sales$pred,outdata_apac_sales[,4])[5]
Mape_autoarima_apac_sales
# 27.68952

auto_arima_pred <- c(fitted(autoarima_apac_sales),ts(fcast_autoarima_apac_sales$pred))
plot(total_timeser_APAC_sales, col = "black")
lines(auto_arima_pred, col = "red")

#------------------------------------------------------------------------------------------------------
# Now Modelling the Consumer-APAC Quantity
# Classical Decomposition method
total_timeser_APAC_quantity <- ts(APAC_consumer_agg$Quantity)
indata_apac_quantity_train <- indata_apac_sales_train

# creating the timeseries
timeser_apac_quantity <- ts(indata_apac_quantity_train$Quantity)
plot(timeser_apac_quantity)

# Time Series plot has both trend , smoothening is done using simple Moving avdrage
w <-3
smoothedseries_apac_quantity <- stats::filter(timeser_apac_quantity, 
                                           filter=rep(1/(2*w + 1),(2*w + 1)), 
                                           method='convolution', sides=2)

# Smoothing left end of the time series
diff <- smoothedseries_apac_quantity[w+2] - smoothedseries_apac_quantity[w+1]
for (i in seq(w,1,-1)) {
  smoothedseries_apac_quantity[i] <- smoothedseries_apac_quantity[i+1] - diff
}


# Smoothing right end of the time series
diff <- smoothedseries_apac_quantity[n-w] - smoothedseries_apac_quantity[n-w-1]
for (i in seq(n-w+1, n)) {
  smoothedseries_apac_quantity[i] <- smoothedseries_apac_quantity[i-1] + diff
}

#Plot the smoothed time series

timevals_in_apac_quantity <- indata_apac_quantity_train$Month_Seq
lines(smoothedseries_apac_quantity, col="blue", lwd=2)

#Building a model on the smoothed time series using classical decomposition
#First, let's convert the time series to a dataframe
smootheddf_apac_quantity<- as.data.frame(cbind(timevals_in_apac_quantity, as.vector(smoothedseries_apac_quantity)))
colnames(smootheddf_apac_quantity) <- c('Month', 'Quantity')


# let's fit a multiplicative model with trend and seasonality to the data
lmfit_apac_2 <- lm(Quantity ~ sin(0.5*Month) * poly(Month,3) + cos(0.5*Month) * poly(Month,3)
              +             + Month, data=smootheddf_apac_quantity)
global_pred_apac_2 <- predict(lmfit_apac_2, Month=timevals_in_apac_quantity)
lines(timevals_in_apac_quantity, global_pred_apac_2, col='red', lwd=2)

# Now, let's look at the locally predictable series
# We will model it as an ARMA series
local_pred_apac_quantity <- timeser_apac_quantity-global_pred_apac_2
plot(local_pred_apac_quantity,col="orange",type='l')
acf(local_pred_apac_quantity)
acf(local_pred_apac_quantity,type = "partial")


armafit_2<- auto.arima(local_pred_apac_quantity)

armafit_2
# Result:
# Series: local_pred_apac_qty 
# ARIMA(0,0,0) with zero mean 
# sigma^2 estimated as 12908:  log likelihood=-258.37
# AIC=518.75   AICc=518.85   BIC=520.48

# We'll check if the residual series is white noise
resi_apac_quantity <- local_pred_apac_quantity - fitted(armafit_2)
adf.test(resi_apac_quantity,alternative = "stationary")
# Result
# Augmented Dickey-Fuller Test
# data:  resi_apac_qty
# Dickey-Fuller = -5.574, Lag order = 3, p-value = 0.01
# alternative hypothesis: stationary

kpss.test(resi_apac_quantity)
# Result: 
# KPSS Test for Level Stationarity
# data:  resi_apac_qty
# KPSS Level = 0.022934, Truncation lag parameter = 1, p-value = 0.1

# Now, let's evaluate the model using MAPE
# First, let's make a prediction for the last 6 months

timevals_out_apac_quantity <- timevals_out_apac_sales
global_pred_out_apac_sales_2 <- predict(lmfit_apac_2, data.frame(Month=timevals_out_apac_quantity))

fcast_apac_quantity <- global_pred_out_apac_sales_2
outdata_apac_quantity <- outdata_apac_sales
fcast_apac_quantity <- global_pred_out_apac_sales_2
#Now, let's compare our prediction with the actual values, using MAPE

MAPE_class_dec_apac_quantity <- accuracy(fcast_apac_quantity,outdata_apac_quantity[,5])[5]
MAPE_class_dec_apac_quantity
#30.49665

#Let's also plot the predictions along with original values, to
#get a visual feel of the fit
class_dec_pred_apac_quantity <- c(ts(global_pred_apac_2),ts(global_pred_out_apac_sales_2))
plot(total_timeser_APAC_quantity, col = "black")
lines(class_dec_pred_apac_quantity, col = "green")

# Now predicting sales using ARIMA model
autoarima_apac_quantity <- auto.arima((timeser_apac_quantity))
autoarima_apac_quantity
# Result:
# Series: (timeseries_apac_qty) 
# ARIMA(0,1,0) 
# sigma^2 estimated as 25366:  log likelihood=-266.07
# AIC=534.14   AICc=534.24   BIC=535.85
# all the parameters are more than the classical decomposition

plot(autoarima_apac_quantity$x,col="black")
lines(fitted(autoarima_apac_quantity), col="red")

# Again, let's check if the residual series is white noise
resi_autoarima_apac_quantity <- timeser_apac_quantity - fitted(autoarima_apac_quantity)

# Dickey FUller test
adf.test(resi_autoarima_apac_quantity,alternative = "stationary")
# Result:
# Augmented Dickey-Fuller Test
# data:  resi_arima_apac_qty
# Dickey-Fuller = -4.3326, Lag order = 3, p-value = 0.01
# alternative hypothesis: stationary

kpss.test(resi_autoarima_apac_quantity)
# Result:
# KPSS Test for Level Stationarity
# data:  resi_arima_apac_qty
# KPSS Level = 0.031535, Truncation lag parameter = 1, p-value = 0.1

# from the above formal test we conclude that the model is statioanry

fcast_autoarima_apac_quantity <- predict(autoarima_apac_quantity,n.ahead=6)
Mape_autoarima_apac_quantity <- accuracy(fcast_autoarima_apac_quantity$pred,outdata_apac_quantity[,4])[5]
Mape_autoarima_apac_quantity
# Result: 98.71101 ### Mape higher than the classical decomposition

auto_arima_pred_1 <- c(fitted(autoarima_apac_quantity),ts(fcast_autoarima_apac_quantity$pred))

plot(total_timeser_APAC_quantity, col = "black")
lines(auto_arima_pred_1, col = "red")


####Now modelling EU- CONSUMER segment 
##First classical decomposition method
###Creating time series on the Sales

ylab <- c("Sales")
xlab<-c("Month")
title<-c("Sales from Jan 2011 to Dec 2014")
xcol <- c(1)
ycol <- c(2)

total_timeser_eu_sales <- ts(EU_consumer_agg$Sales)
indata_eu_sales_train <- EU_consumer_agg[1:42,]
timeser_eu_sales <- ts(indata_eu_sales_train$Sales)
plot(timeser_eu_sales)

# Time Series plot has moslty trend , Smoothing the series - Moving Average Smoothing
w <-2
smoothedseries_eu_sales <- stats::filter(timeser_eu_sales, 
                                           filter=rep(1/(2*w + 1),(2*w + 1)), 
                                           method='convolution', sides=2)	

# Smoothing left end of the time series
diff <- smoothedseries_eu_sales[w+2] - smoothedseries_eu_sales[w+1]
for (i in seq(w,1,-1)) {
  smoothedseries_eu_sales[i] <- smoothedseries_eu_sales[i+1] - diff
}

# Smoothing right end of the time series
n <- length(timeser_eu_sales)
diff <- smoothedseries_eu_sales[n-w] - smoothedseries_eu_sales[n-w-1]
for (i in seq(n-w+1, n)) {
  smoothedseries_eu_sales[i] <- smoothedseries_eu_sales[i-1] + diff
}


#Plot the smoothed time series
timevals_in_eu_sales <- indata_eu_sales_train$Month_Seq
lines(smoothedseries_eu_sales, col="blue", lwd=2)

#Building a model on the smoothed time series using classical decomposition
#First, let's convert the time series to a dataframe
smootheddf_eu_sales <- as.data.frame(cbind(timevals_in_eu_sales, as.vector(smoothedseries_eu_sales)))
colnames(smootheddf_eu_sales) <- c('Month', 'Sales')

#Now, let's fit a multiplicative model with trend and seasonality to the data
#Seasonality will be modeled using a sinusoid function
lmfit_eu <- lm(Sales ~ sin(0.5*Month) * poly(Month,3) + cos(0.5*Month) * poly(Month,3)
                 +             + Month, data=smootheddf_eu_sales)
global_pred_eu <- predict(lmfit_eu, Month=timevals_in_eu_sales)
summary(global_pred_eu)
lines(timevals_in_eu_sales, global_pred_eu, col='red', lwd=2)


# Now, let's look at the locally predictable series
# We will model it as an ARMA series

local_pred_eu_sales <- timeser_eu_sales - global_pred_eu
plot(local_pred_eu_sales,col="red",type='l')
acf(local_pred_eu_sales)
acf(local_pred_eu_sales,type = "partial")
armafit_eu <- auto.arima(local_pred_eu_sales)

armafit_eu
# Result:
# Series: local_pred_eu_sales 
# ARIMA(0,0,0) with zero mean 
# sigma^2 estimated as 97892454:  log likelihood=-445.98
# AIC=893.96   AICc=894.06   BIC=895.7

# We'll check if the residual series is white noise
resi_eu_sales<-local_pred_eu_sales - fitted(armafit_eu)
adf.test(resi_eu_sales,alternative = "stationary")
# Result:
# Augmented Dickey-Fuller Test
# data:  resi_eu_sales
# Dickey-Fuller = -6.7179, Lag order = 3, p-value = 0.01
# alternative hypothesis: stationary 
# p-value is less than 0.05 hence it rejects null hypothesis, hence the timeseries is stationary

kpss.test(resi_eu_sales)
# Result:
# KPSS Test for Level Stationarity
# data:  resi_eu_sales
# KPSS Level = 0.019284, Truncation lag parameter = 1, p-value = 0.1
# p-values is more than 0.05 hence the timeseries is stationary

# Now, let's evaluate the model using MAPE
# First, let's make a prediction for the last 6 months
outdata_eu_sales <- EU_consumer_agg[43:48,]
timevals_out_eu_sales <- outdata_eu_sales$Month_Seq
global_pred_out_eu_sales <- predict(lmfit_eu, data.frame(Month=timevals_out_eu_sales))

fcast_eu_sales <- global_pred_out_eu_sales

#Now, let's compare our prediction with the actual values, using MAPE
MAPE_class_dec_eu_sales <- accuracy(fcast_eu_sales,outdata_eu_sales[,4])[5]
MAPE_class_dec_eu_sales
# Result: 28.27462

#Let's also plot the predictions along with original values, to
#get a visual feel of the fit
class_dec_pred_eu_sales <- c(ts(global_pred_eu),ts(global_pred_out_eu_sales))
plot(total_timeser_eu_sales, col = "black")
lines(class_dec_pred_eu_sales, col = "red")


###Now predicting sales using ARIMA model
autoarima_eu_sales<-auto.arima((timeser_eu_sales))
autoarima_eu_sales
# Result:
# Series: (timeseries_eu_sales) 
# ARIMA(2,1,0) 
# Coefficients:
#          ar1      ar2
#      -0.5796  -0.4906
#s.e.   0.1346   0.1310
# sigma^2 estimated as 168564623:  log likelihood=-445.84
# AIC=897.67   AICc=898.32   BIC=902.81\
# Arima has high AIC,AICc,BIC values as compared to Classical decomposition

plot(autoarima_eu_sales$x,col="black")
lines(fitted(autoarima_eu_sales), col="red")

# Again, let's check if the residual series is white noise
resi_autoarima_eu_sales <- timeser_eu_sales-fitted(autoarima_eu_sales)

# Dickey -Fuller Test 
adf.test(resi_autoarima_eu_sales,alternative = "stationary")
# Result:
# Augmented Dickey-Fuller Test
# data:  resi_arima_eu_sales
# Dickey-Fuller = -4.3522, Lag order = 3, p-value = 0.01 
# alternative hypothesis: stationary

kpss.test(resi_autoarima_eu_sales)
# Result:
# KPSS Test for Level Stationarity
# data:  resi_arima_eu_sales
# KPSS Level = 0.05314, Truncation lag parameter = 1, p-value = 0.1

fcast_autoarima_eu_sales <- predict(autoarima_eu_sales,n.ahead=6)
Mape_autoarima_eu_sales <- accuracy(fcast_autoarima_eu_sales$pred,outdata_eu_sales[,4])[5]
Mape_autoarima_eu_sales
# 28.9226

auto_arima_pred_3 <- c(fitted(autoarima_eu_sales),ts(fcast_autoarima_eu_sales$pred))
plot(total_timeser_eu_sales, col = "black")
lines(auto_arima_pred_3, col = "red")



# Modelling the Consumer-EU Quantity
###First using Classical Decomposition method

total_timeser_eu_quantity <- ts(EU_consumer_agg$Quantity)
indata_eu_quantity_train <- indata_eu_sales_train

# creating the timeseries
timeser_eu_quantity <- ts(indata_eu_quantity_train$Quantity)
plot(timeser_eu_quantity)

#Time Series plot has both trend , smoothening is done using simple Moving avdrage
w <-2
smoothedseries_eu_quantity <- stats::filter(timeser_eu_quantity, 
                                              filter=rep(1/(2*w + 1),(2*w + 1)), 
                                              method='convolution', sides=2)

# Smoothing left end of the time series
diff <- smoothedseries_eu_quantity[w+2] - smoothedseries_eu_quantity[w+1]
for (i in seq(w,1,-1)) {
  smoothedseries_eu_quantity[i] <- smoothedseries_eu_quantity[i+1] - diff
}

# Smoothing right end of the time series
diff <- smoothedseries_eu_quantity[n-w] - smoothedseries_eu_quantity[n-w-1]
for (i in seq(n-w+1, n)) {
  smoothedseries_eu_quantity[i] <- smoothedseries_eu_quantity[i-1] + diff
}

#Plot the smoothed time series

timevals_in_eu_quantity <- indata_eu_quantity_train$Month_Seq
lines(smoothedseries_eu_quantity, col="blue", lwd=2)

#Building a model on the smoothed time series using classical decomposition
#First, let's convert the time series to a dataframe
smootheddf_eu_quantity<- as.data.frame(cbind(timevals_in_eu_quantity, as.vector(smoothedseries_eu_quantity)))
colnames(smootheddf_eu_quantity) <- c('Month', 'Quantity')

# let's fit a multiplicative model with trend and seasonality to the data
lmfit_eu_2 <- lm(Quantity ~ sin(0.5*Month) * poly(Month,3) + cos(0.5*Month) * poly(Month,3)
                   +             + Month, data=smootheddf_eu_quantity)
global_pred_eu_2 <- predict(lmfit_eu_2, Month=timevals_in_eu_quantity)
lines(timevals_in_eu_quantity, global_pred_eu_2, col='red', lwd=2)

# Now, let's look at the locally predictable series
# We will model it as an ARMA series
local_pred_eu_quantity <- timeser_eu_quantity-global_pred_eu_2
plot(local_pred_eu_quantity,col="orange",type='l')
acf(local_pred_eu_quantity)
acf(local_pred_eu_quantity,type = "partial")

armafit_eu_2<- auto.arima(local_pred_eu_quantity)
armafit_eu_2
# Result: 
# Series: local_pred_eu_qty 
# ARIMA(2,0,0) with zero mean 
# Coefficients:
#          ar1      ar2
#      -0.5833  -0.5739
# s.e.   0.1246   0.1204
# sigma^2 estimated as 8505:  log likelihood=-249.06
# AIC=504.12   AICc=504.75   BIC=509.33

# We'll check if the residual series is white noise
resi_eu_quantity <- local_pred_eu_quantity - fitted(armafit_eu_2)
adf.test(resi_eu_quantity,alternative = "stationary")
# Result:
# Augmented Dickey-Fuller Test
# data:  resi_eu_qty
# Dickey-Fuller = -4.9629, Lag order = 3, p-value = 0.01
# alternative hypothesis: stationary

kpss.test(resi_eu_quantity)
# Result:
#	KPSS Test for Level Stationarity
# data:  resi_eu_qty
# KPSS Level = 0.025165, Truncation lag parameter = 1, p-value = 0.1

# Now, let's evaluate the model using MAPE
# First, let's make a prediction for the last 6 months

timevals_out_eu_quantity <- timevals_out_eu_sales
global_pred_out_eu_sales_2 <- predict(lmfit_eu_2, data.frame(Month=timevals_out_eu_quantity))

fcast_eu_quantity <- global_pred_out_eu_sales_2
outdata_eu_quantity <- outdata_eu_sales
fcast_eu_quantity <- global_pred_out_eu_sales_2

#Now, let's compare our prediction with the actual values, using MAPE

MAPE_class_dec_eu_quantity <- accuracy(fcast_eu_quantity,outdata_eu_quantity[,5])[5]
#30.51226

#Let's also plot the predictions along with original values, to
#get a visual feel of the fit
class_dec_pred_eu_quantity <- c(ts(global_pred_eu_2),ts(global_pred_out_eu_sales_2))
plot(total_timeser_eu_quantity, col = "black")
lines(class_dec_pred_eu_quantity, col = "green")

# Now predicting sales using ARIMA model
autoarima_eu_quantity <- auto.arima((timeser_eu_quantity))
autoarima_eu_quantity
# Result:
# Series: (timeseries_eu_qty) 
# ARIMA(2,1,0) 
# Coefficients:
#          ar1      ar2
#       -0.7359  -0.5879
# s.e.   0.1224   0.1185
# sigma^2 estimated as 21185:  log likelihood=-261.9
# AIC=529.8   AICc=530.44   BIC=534.94
# all the parameters are more than the classical decomposition

plot(autoarima_eu_quantity$x,col="black")
lines(fitted(autoarima_eu_quantity), col="red")

# Again, let's check if the residual series is white noise
resi_autoarima_eu_quantity <- timeser_eu_quantity - fitted(autoarima_eu_quantity)

# Dickey FUller test
adf.test(resi_autoarima_eu_quantity,alternative = "stationary")
# Result:
# Augmented Dickey-Fuller Test
# data:  resi_arima_eu_qty
# Dickey-Fuller = -3.5969, Lag order = 3, p-value = 0.04521
# alternative hypothesis: stationary
  
  
kpss.test(resi_autoarima_eu_quantity)
# Result: 
# KPSS Test for Level Stationarity
# data:  resi_arima_eu_qty
# KPSS Level = 0.047939, Truncation lag parameter = 1, p-value = 0.1

fcast_autoarima_eu_quantity <- predict(autoarima_eu_quantity,n.ahead=6)
Mape_autoarima_eu_quantity <- accuracy(fcast_autoarima_eu_quantity$pred,outdata_eu_quantity[,5])[5]
Mape_autoarima_eu_quantity
##37.54059 ### Mape higher than the classical decomposition

auto_arima_pred_4<- c(fitted(autoarima_eu_quantity),ts(fcast_autoarima_eu_quantity))

plot(total_timeser_eu_quantity, col = "black")
lines(auto_arima_pred_4, col = "red")


# The classical decomposition method provided a better MAPE paramenter value for all the forecast as compared to the auto ARIMA.








