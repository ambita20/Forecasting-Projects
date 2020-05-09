library(urca)
library(tseries)
library(forecast)
data<- read.csv('C:/Users/ambit/Downloads/CPIAUCSLsince1954.csv',skip=199)

colnames(data)<-c('date','cpiucsl')
cpi<-data$cpiucsl
#1) Data Visuualization
par(mfrow=c(1,1))
plot.ts(cpi)
par(mfrow=c(1,2))
acf(cpi)
pacf(cpi)

#From the plot of timeseries, it is evident that the time series is non stationary. Also the ACF decreases gradually which implies that thge time series s non stationary
#Let us perform the ADF test for non-stationarity

#2) Identification of the non-stationarity of the time series
adf.test(cpi)
# The p-value is 0.5785 which is greater than 0.05. Hence we cannot reject the null that the time series is non stationary.
#Since it shows a trend, let us check for trend
test1<-ur.df(cpi,type="trend", lags = 8)
summary(test1)
#The test statistic is not more negative than the critical values, hence we conclude that the time series is non-stationary with a trend

#3) Parameter estimation
cpi.diff.one<-diff(cpi)
adf.test(cpi.diff.one)
#After taking the first order difference, we get a p-value of 0.01 which is less than 0.05. Hence we reject the null hypothesis that the time series is non-stationary and conclude that the time series is stationary.
par(mfrow=c(1,2))
acf(cpi.diff.one)
pacf(cpi.diff.one)

#The plot shows that the ACF cutts of at lag=2 and the PACF decreases exponentially. Lets fit an ARMA(0,2) model

Arima(cpi,order=c(0,1,2))
#Checking for other models
Arima(cpi,order=c(2,1,2))

#We see that ARIMA(2,1,3) has the lowest AIC, hence let's fit this model.
model<-Arima(cpi,order=c(2,1,3))

#We see that ma1 is not significant
# We fit the final model by fixing the ar1 and ma1 to 0
model.revised<-Arima(cpi,order=c(2,1,3),fixed=c(NA,NA,0,NA,NA))
#Final model


#4) Diagnostic checking
checkresiduals(model.revised)
#We observe that the residuals are random and the ACF is not significant. The p-value is greater than 0.05 hence we cannot reject the null hypotheses that the residuals are white noise

#Forecast for next 50 months
forecast(model.revised,h=50)
autoplot(forecast(model.revised,h=50))





