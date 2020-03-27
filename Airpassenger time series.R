library(datasets)
library(dataseries)
library(datasets.load)
plot(AirPassengers)
abline(reg = lm(log(AirPassengers) ~ time(log(AirPassengers))))
cycle(AirPassengers)
plot(aggregate(AirPassengers,FUN=mean))
class(AirPassengers)
frequency(AirPassengers)
boxplot(AirPassengers ~ cycle(AirPassengers))          
plot(diff(log(AirPassengers)))
plot(log(AirPassengers))
plot(diff(AirPassengers))
#AR(p)I(d)MA(q)- Autoregressive Integration Moving Average
library(tseries)
acf(AirPassengers)
acf(diff(log(AirPassengers))) # q=1 (because the line before the line which first gets inverted is 1)
pacf(diff(log(AirPassengers)))# p=0 (logic same as above)
fit <- arima(log(AirPassengers),c(0,1,1),seasonal = list(order = c(0,1,1), period = 12)) # arima model main line
pred <- predict(fit, n.ahead = 10*12)
pred1 <- 2.718^pred$pred
ts.plot(AirPassengers,2.718^pred$pred, log = "y", lty = c(1,3)) #plot for prediction and previous data
#testing out model
datawide <- ts(AirPassengers, frequency = 12, start = c(1949,1),end = c(1959,12))
fit1 <- arima(log(datawide),c(0,1,1),seasonal = list(order = c(0,1,1),period = 12)) #second arima model without the last year
pred2 <- predict(fit1, n.ahead = 10*12)
pred3 <- 2.718^pred$pred
data <- head(pred3,12)
predict_1960 <- round(data,digits = 0)
original_1960 <- tail(AirPassengers,12)
ts.plot(AirPassengers,2.718^pred$pred, log = "y", lty =c(1,3))
mean(predict_1960)
mean(original_1960)