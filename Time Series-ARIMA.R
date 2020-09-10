# Time series Analysis to Predict for next 10 years using "data" 

library(ggfortify)
library(tseries)
library(forecast)



data<-(AirPassengers)
#The AirPassenger dataset in R provides monthly totals of a US airline passengers, from 1949 to 1960.
class(data) # this is time series class therefore no further class or date manipulation is required.

# Step 1: PERFORM EXPLORATORY DATA ANALYSIS
start(data) # This shows the starting time of time series
end(data) # This shows the end time of time series
frequency(data) #  Check the frequency of the time series
sum(is.na(data))# Check for missing values
cycle(data)# Check the cycle of the time series
summary(data)

# Plot the raw data using the base plot function
plot(data,xlab="Date", ylab = "Number of Passengers",main="Air Passenger numbers from 1949 to 1961")
# to fit the line 
abline(reg=lm(data~time(data)))
# here the mean is changing according to the time frame , it is not constant but in time series forcasting we should have constant mean
# Transform the data to have constant variance, mean, covariance 
#Let's use the boxplot function to see any seasonal effects.
boxplot(data~cycle(data),xlab="Date", ylab = "Number of Passengers" ,main ="Monthly Air Passengers Boxplot from 1949 to 1961")
# in the month of august and july the number of passengers are more. this is the seasonality
#AirPassengers appears to be multiplicative time series as the passenger numbers increase, it appears so does the pattern of seasonality.
#There do not appear to be any outliers and there are no missing values. Therefore no data cleaning is required.
#Step 2: TIME SERIES DECOMPOSITION
decomposeAP <- decompose(data,"multiplicative")
autoplot(decomposeAP)
#In these decomposed plots we can again see the trend and seasonality as inferred previously, 
#but we can also observe the estimation of the random component depicted under the "remainder".
# GENERAL TREND
plot(aggregate(data,FUN = mean)) # this will aggregrate the cycles and display year after year trend
# the values are increasing every year , the trend is increasing 
#Step 3: TEST STATIONARITY OF THE TIME SERIES
#In order to test the stationarity of the time series, let's run the Augmented Dickey-Fuller Test
#using the adf.test function from the tseries R package.

#First set the hypothesis test:
  
  #The null hypothesis H0 : that the time series is non stationary
  #The alternative hypothesis HA : that the time series is stationary

adf.test(data) 

#As a rule of thumb, where the p-value is less than 5%, we strong evidence against the null hypothesis, so we reject the null hypothesis.
#As per the test results above, the p-value is 0.01 which is <0.05 therefore we reject the null in favour of the alternative hypothesis 
#that the time series is stationary.

# TRANSFORM TO STATIONARY 
 plot(diff(log(data)))
# for this series the mean, variance are constant , hence it is statinoary ,now Time series analysis can be applied
#Step 4: FIT A TIME SERIES MODEL
 #ARIMA
 # we know d=1 as we have differenciated once
 # to know q value
 acf(diff(log(data))) # q = 1
 #to know the value of p
 pacf(diff(log(data))) # p= 0 
 
plot(diff(log(data)))
# Lets fit the (p,d,q) in the ARIMA model

fit<- arima(log(data),c(0,1,1),seasonal =list(order=c(0,1,1),period=12))
pred=predict(fit, n.ahead = 10*12)     
pred1 <-2.718^pred$pred
ts.plot(data,pred1,log="y",lty=c(1,3))

#TESTING THE MODEL

datawide<- ts(data,frequency = 12,start=c(1949,1),end=c(1959,12))
fit<-arima(log(datawide),c(0,1,1),seasonal =list(order=c(0,1,1),period=12))
pred=predict(fit, n.ahead = 10*12)     
pred1 <-2.718^pred$pred
# Predicting from 1960 to 1970
data1<-head(pred1,12)
predicted_1960 <-round(data1,digits = 0)
original_1960 <-tail(data,12)
ts.plot(data,pred1,log="y",lty=c(1,3))
