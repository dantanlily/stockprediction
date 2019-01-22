#Chapter1
#1.1
x<-2
logreturn<-function(x){
  log(tail(x,-1)/head(x,-1))
}
logreturn
logreturn(x)

tail(1,-1)
head(CO2)
tail(CO2)
View(CO2)
pi
getwd()
#?work_dir<-"Users/dantan/Desktop/Introduction_to_R_for_Quantitative_Finance/ch1_time_series_analysis"
#setwd(work_dir)
aapl<-read.zoo("aapl.csv",sep = ",",header=TRUE,format="%Y-%m-%d")
as.matrix(aapl)
View(aapl)
plot(aapl,main="APPLE CLOSING PRICES ON NASDAQ",ylab="Price(USD)",xlab="Date")
head(aapl)
tail(aapl)
aapl[which.max(aapl)]#must use [] to extract data from matrix
ret_simple<-diff(aapl)/lag(aapl,k=-1)*100 # next day changed divided by yesterday price
View(ret_simple)
# look into function
diff.default
lag<-as.matrix(lag(aapl,k=-1))
price<-as.matrix(aapl)
diff_price<-as.matrix(diff(aapl))

ret_cont<-diff(log(aapl))*100
View(ret_cont)
summary(coredata(ret_simple))
ret_simple[which.min(ret_simple)]
hist(ret_simple,breaks=100,main="Histogram of Simple Returns",xlab="%") # break how many in the same group
aapl_2013<-window(aapl,start='2013-01-01',end='2013-12-31')
aapl_2013[which.max(aapl_2013)]
# value at risk
quantile(ret_simple,probs=0.01)
# 1.2
#install.package("forecast")
library("forecast")
#library(gbm)
#library(survial)
#library(SurvBoost)
#library(gbm3)
hp<-read.zoo("UKHP.csv",sep=",",header=TRUE,format="%Y-%m",FUN=as.yearmon)
View(hp)
frequency(hp)# monthly data
hp_ret<-diff(hp)/lag(hp,k=-1)*100
hist(hp_ret,breaks=12,main="Histogram of Simple HP Returns",xlab="%") 
hp_ret[which.min(hp_ret)]
mod<-auto.arima(hp_ret,stationary=TRUE,seasonal=FALSE,ic="aic")
mod
confint(mod)
tsdiag(mod)
plot(mod$x,Ity=1,main="UK house prices:raw data vs fitted + values",ylab="Return in percent",xlab="Date")
lines(fitted(mod),Ity=2,lwd=2,col="red")
accuracy(mod)
predict(mod,n.ahead=3)
plot(forecast(mod))
# 1.3
library(zoo)
library(urca)




