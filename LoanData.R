library("forecast")
#install.packages("urca")
library(urca)
#install.packages("tseries")
library(tseries)

#Case Study HW$

Start = 1 
End = 60 
Frequency = 1 
#The series analyzed here is the monthly volume of commercial bank real-estate 
#loans, in billions of dollars, from January 1973 to October 1978, 
#a total of 70 observations.  
#The data are derived from reports to the Federal Reserve System from large
#commercial banks.  

#Here is the data with 70 observations from January 1973 through October 1978:

loans<-c(46.5, 47, 47.5, 48.3, 49.1, 50.1, 51.1, 52, 53.2, 53.9, 54.5, 55.2, 55.6, 55.7, 56.1,
            56.8, 57.5, 58.3, 58.9, 59.4, 59.8, 60, 60, 60.3, 60.1, 59.7, 59.5, 59.4, 59.3, 59.2,
            59.1, 59, 59.3, 59.5, 59.5, 59.5, 59.7, 59.7, 60.5, 60.7, 61.3, 61.4, 61.8,
            62.4, 62.4, 62.9, 63.2, 63.4, 63.9, 64.5, 65, 65.4, 66.3, 67.7, 69, 70, 71.4,
            72.5, 73.4, 74.6, 75.2, 75.9, 76.8, 77.9, 79.2, 80.5, 82.6, 84.4, 85.9, 87.6)

#1)Data Visualization
plot.ts(loans)
par(mfrow=c(1,2))
acf(loans)
pacf(loans)

#We observe that the ACF decreases slowly. Also, the timeseries is showing a
#trend as well as a drift.Let us perform some tests for checking stationarity.

#ADF Test
adf.test(loans)
adf.test(loans,k=3)
adf.test(loans,k=5)
adf.test(loans,k=2)


# The p-value is 0.3516 which is greater than 0.05. Hence we cannot the reject 
# the null Hypothesis that the time series is non-stationary.

#fit<-arima(loans,order=c(1,0,0))

# Let us perform further analysis with trend and drift


#Let us test for trend
test=ur.df(loans, type = "trend", lags = 3)
summary(test)
#As we can see -2.7121 is greater than all critical values. As suck we cannot reject
#the null that the time series is non-stationary.


#Testing for drift
test1=ur.df(loans, type = "drift", lags = 3)
summary(test1)
#Again we reject the null hypothesis because the test-statistic value is greater 
#than all critical values

#Testing with no drift and trend
test2=ur.df(loans, type = "none", lags = 3)
summary(test2)
#Again we reject the null hypothesis because the test-statistic value is greater 
#than all critical values

#performing the KPSS Unite Root Test
test=ur.kpss(loans)
summary(test)

#0.2552 is less than all the critical values and hence we reject the null
#hypothesis that the time series is stationary

# The plot of the time series shows both a trend and a drift  but we trust 
# the trend as can be seen from the time series plot


#Finding the number of differencs required to make the timeseries stationary
#Taking the first order difference
loan.diff.one<-loans %>% diff() 
loan.diff.one%>%adf.test()
#The p-value is 0.6721 and hence we cannot reject the null hypothesis that the
#time series is non-stationary. Hence the first order difference is not stationary

#Performing a second order difference
loan.diff.two<-loan.diff.one %>% diff()
loan.diff.two %>% adf.test()

#The second order diffrence results in a p-value smaller than the critical value.
#Hence we can conclude that the time series is stationary by rejecting the
#null hypothesis

#2)Identification of models
#Let us plot the data for second order difference, d=2
plot.ts(loan.diff.two)
acf(loan.diff.two)
pacf(loan.diff.two)


#We can see that the time series looks stationary with a mean around zero. There is
#a constant variance.The ACF is significant at lag 1 and the PACF seems to be 
#exponentially decreasing. This is a characteristic of an MA(1) model

# Let us fit the original series in a MA(1) model with d=2

fit<-arima(loans,order=c(0,2,1))

#Comapring the AIC with other similar models
arima(loans,order=c(1,2,0))
arima(loans,order=c(0,2,2))
#We see that the AIC for the MA(1) model is the least

#3)Parameter Estimation
fit

#μ= 0 , θ1= -0.3722
# We can also see that the parameter is significant since it is greater than 
#2 s.e and we can reject the null hypothesis that it is not significant

#4)Residual Diagnostics
checkresiduals(fit)
# The residuals seem to be following a random pattern and the ACF shows
#characteristics of white noise. Also, the Ljung-Box testgives a p-value of 0.4244.
#This is greater than 0.05 hence we cannot reject the null hypothesis that
#the residuals are white noise. Hence we go forward with the ARIMA(0,2,1) model.

#The final equation is 


#5) Forecatsing for the next 2 years
forecast(fit,h=24)
