rain <- read.csv("rain.csv")
raints <- ts(rain, start = c(1900))
raints
plot(raints)
rainforecasts <- HoltWinters(raints, beta = F, gamma = F)
rainforecasts
plot(rainforecasts)
names(rainforecasts)
rainforecasts$fitted
install.packages("forecast")
library(forecast)
rainforecast <- forecast:::forecast.HoltWinters(rainforecasts, h=10)
plot(rainforecast)
rainforecast
hist(rainforecast$residuals)


sales <- read.csv("sales.csv")
salests <- ts(sales[,1],start=1995,freq = 12)
salests
plot(salests)
decom <- stl(salests, s.window = "periodic")
decom
plot(decom)
salesforecast <- HoltWinters(salests, gamma = F)
plot(salesforecast)
salesfuture <- forecast:::forecast.HoltWinters(salesforecast, h=12)
plot(salesfuture)
salesfuture
qqnorm(salesfuture$residuals)


souv <- read.csv("souvenir.csv")
souvts <- ts(souv, frequency = 12, start = c(1987,1))
plot(souvts)
d.sa <- diff(souvts)
plot(d.sa)
d12.sa <- diff(d.sa,12)
plot(d12.sa)
acf(souvts)
pacf(souvts)
library(forecast)
auto.arima(souvts)
arimafit <- arima(souvts,order = c(1,1,1), seasonal = c(0,1,1))
arimafit
arimafuture <- forecast:::forecast.Arima(arimafit, h=48)
arimafuture
plot(arimafuture)
