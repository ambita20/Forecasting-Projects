library(urca)
library(tseries)
library(forecast)

data1<- read.csv('data/MORTGAGE30USsince2005.csv',skip=200)
colnames(data1)<-c('year','mortgage')
ts1<-data1$mortgage

data2<- read.csv('data/MORTGAGE15USsince2005.csv',skip=200)
colnames(data2)<-c('year','mortgage')
ts2<-data2$mortgage

data3<- read.csv('data/MORTGAGE5USsince2005.csv',skip=200)
colnames(data3)<-c('year','mortgage')
ts3<-data2$mortgage

bfx1<- as.matrix(cbind(ts1,ts2),demean=FALSE)

po.test(bfx1)
#p-value is greater than 0.05, we fail to reject the null and the two time series are not cointegrated

bfx2<- as.matrix(cbind(ts1,ts3),demean=FALSE)

po.test(bfx2)
#p-value is greater than 0.05, we fail to reject the null and the two time series are not cointegrated

bfx3<- as.matrix(cbind(ts2,ts3),demean=FALSE)

po.test(bfx3)
#p-value is lesser than 0.05, we reject the null and the two time series are  cointegrated

#TS2 and TS3 are cointegrated
