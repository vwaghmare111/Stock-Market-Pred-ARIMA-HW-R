library(detrendeR)
library(forecast)
library(tseries)
library(fBasics)

setwd("D:/02 MSC FE/Sem 3/Financial Econometrics/Assignment")
data<-read.csv("D:/02 MSC FE/Sem 3/Financial Econometrics/Assignment/Data_series.csv")
data<-as.ts(data)

plot(data,main="Actual Data")

##basic stats##
basicStats(data)

##Exploratory analysis##
par(mfrow=c(1,2))
acf(data,50,main="Actual data ACF")
pacf(data,50,main="Actual data PACF")
adf.test(data)
normalTest(data,method = "jb")
fit <- tbats(data)
seasonal <- !is.null(fit$seasonal)
seasonal


##First order differencing for stationarity###
mydata<-diff(data)
View(mydata)
fit <- tbats(mydata)
seasonal <- !is.null(fit$seasonal)
seasonal
plot(mydata,main="First difference plot")
par(mfrow=c(1,4))
acf(data,50,main="Actual data ACF")
pacf(data,50,main="Actual data PACF")
acf(mydata,50,main="First difference ACF")
pacf(mydata,50,main="First difference PACF")
adf.test(mydata)
UnitRootTest(mydata)

##Identifying AR order based on OLS method and bic##
mdl1<-ar.ols(mydata,bic=TRUE)
mdl1

##ARIMA model###
fit<-auto.arima(data,ic="bic")
arfit<-arima(data,order=c(2,1,0))
summary(arfit)
p1=c(1,-arfit$coef[1:2]) ###Characeteristic equation####
roots<-polyroot(p1) ###Find root solution###
roots
Mod(roots)
fit<-arima(data,order=c(2,1,1))
summary(fit)
rsquare<-1-(sum(fit$residuals)^2)/sum((data-mean(data))^2)
rsquare
adjrsquare<-1-(var(fit$residuals)/var(data))
adjrsquare

is.factor(fit$residuals)
runs
##Residual diagnosis - ARIMA###
plot(fit$residuals,main="ARIMA (2,1,1) Residuals")
checkresiduals(fit)
normalTest(fit$residuals,method = "jb")
adf.test(fit$residuals)
unitrootTest(fit$residuals,)
library(normwhn.test)
whitenoise.test(fit$residuals)
par(mfrow=c(1,2))
acf(fit$residuals,50,main="ARIMA Residual ACF")
pacf(fit$residuals,50,main="ARIMA Residual PACF")
Box.test(fit$residuals,lag=5,type='Ljung')
pv=1-pchisq(1.3798,2)
pv

###Forecast of the ARIMA model###
fitted.values(fit)
forecast(fit)


### HoltWinters Exponential Smoothing Forecast###
mdl<-HoltWinters(data,beta=FALSE,gamma=FALSE)
mdl
fitted.values(mdl)
forecast(mdl)
mdl$SSE
mdl1<-HoltWinters(data,gamma=FALSE)
mdl1
fitted.values(mdl1)
mdl1$SSE
forecast(mdl1)
par(mfrow=c(1,2))
plot(mdl,main = "Holt-Winters without trend")
plot(mdl1, main = "Holt-Winters with trend")

##Residual diagnosis - HoltWinter###
write.csv(fit$residuals,'residuals.csv')
residual<-read.csv("residuals.csv")
residual<-as.ts(residual)
acf(data, main = "ACF Residual")
pacf(data, main = "PACF Residual")
adf.test(residual)
normalTest(residual,method = "jb")
#runs.test(residual)
whitenoise.test(residual)
Box.test(residual,lag=5,type='Ljung')
pv=1-pchisq(10.306,3)
pv

############ Data series - Bharti Infratel ###########################

data<-read.csv("D:/02 MSC FE/Sem 3/Financial Econometrics/Assignment/Bharti_Infratel.csv")
data<-as.ts(data)
plot(data,main="Actual Bharti Infratel Price")
normalTest(data,method = "jb")
adf.test(data)


##basic stats##
basicStats(data)

##Exploratory analysis##
logdata<-log(data)
View(logdata)
adf.test(logdata)
fit <- tbats(data)
seasonal <- !is.null(fit$seasonal)
seasonal
difflogdata<-diff(logdata)
adf.test(difflogdata)
unitrootTest(difflogdata)
View(difflogdata)
par(mfrow=c(3,3))
acf(data,125,main="Actual data ACF")
pacf(data,125,main="Actual data PACF")
acf(logdata,main="Logdata ACF")
pacf(logdata,main="Logdata PACF")
acf(difflogdata,main="ACF - First difference of natural log")
pacf(difflogdata,main="PACF - First difference of natural log")



##Identifying AR order based on OLS method and bic##
mdl1<-ar.ols(difflogdata,bic=TRUE)
mdl1

##ARIMA model###
fit<-arima(difflogdata,order=c(3,1,1))
fit<-arima(difflogdata,order=c(2,1,2))
fit<-arima(difflogdata,order=c(2,1,1))
fit<-arima(difflogdata,order=c(4,1,2))
fit<-arima(difflogdata,order=c(5,1,2))
fit<-arima(difflogdata,order=c(6,1,2))
summary(fit)
p1=c(1,-fit$coef[1:3]) ###Characeteristic equation####
roots<-polyroot(p1) ###Find root solution###
roots
Mod(roots)
rsquare<-1-(sum(fit$residuals)^2)/sum((data-mean(data))^2)
rsquare
adjrsquare<-1-(var(fit$residuals)/var(data))
adjrsquare

##Residual diagnosis - ARIMA###
normalTest(fit$residuals,method = "jb")
unitrootTest(difflogdata)
adf.test(fit$residuals)
#runs.test(fit$residuals)
whitenoise.test(fit$residuals)
par(mfrow=c(1,2))
acf(fit$residuals,50,main="ARIMA Residual ACF")
pacf(fit$residuals,50,main="ARIMA Residual PACF")
Box.test(fit$residuals,lag=7,type='Ljung')
pv=1-pchisq(7.2925,3)
pv
checkresiduals(fit)

###Forecast of the ARIMA model###
fitted.values(fit)
forecast(fit)


### HoltWinters Exponential Smoothing Forecast###
mdl<-HoltWinters(difflogdata,gamma=FALSE)
mdl
forecast(mdl)
write.csv(fitted.values(mdl),"HoltsWinterWithTrend.csv")

mdl1<-HoltWinters(difflogdata,beta=FALSE,gamma=FALSE)
mdl1
write.csv(fitted.values(mdl1),"HoltsWinterWithoutTrend.csv")
forecast(mdl1)

par(mfrow=c(1,2))
plot(mdl,main = "Holt-Winters with trend")
plot(mdl1, main = "Holt-Winters without trend")

##Residual diagnosis - HoltWinter###
residual<-read.csv("residuals.csv")
residual<-as.ts(residual)
acf(data, main = "ACF Residual")
pacf(data, main = "PACF Residual")
adf.test(residual)
normalTest(residual,method = "jb")
#runs.test(residual)
whitenoise.test(residual)
Box.test(residual,lag=7,type='Ljung')
pv=1-pchisq(17.966,5)
pv

