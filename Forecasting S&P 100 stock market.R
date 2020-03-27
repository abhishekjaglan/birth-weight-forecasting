############################################
# Quick stock Forecasting with XTS objects #
############################################
library(quantmod)
library(forecast)
library(tseries)
library(timeSeries)
library(xts)
#pulling data from Yahoo Finance SPY
getSymbols('SPY',from = '1980-01-01', to = '2020-03-18')
class(SPY)
#want the closing price at the 4th column(1-opening 2-high 3-low 4-closing)
SPY_close_price = SPY[,4]
# plotting the data
par(mfrow = c(1,1))
plot(SPY_close_price)
class(SPY_close_price)
#calculating Acf and Pacf - looking for lags # Pacf->P , Acf->Q
par(mfrow = c(1,2))
Acf(SPY_close_price, main = 'ACF for differenced Series')
Pacf(SPY_close_price, main = 'PACF for Differenced Series')
#adf test for p-value
print(adf.test(SPY_close_price))    #p=.21, therefore we want to lower the value by differencing it
auto.arima(SPY_close_price, seasonal = FALSE) #Arima 3,1,1  AIC/BIC = 7223/7250

fitA = auto.arima(SPY_close_price,seasonal = FALSE) #auto arima
tsdisplay(residuals(fitA),lag.max = 40, main = '(3,1,1) Model Residuals')
auto.arima(SPY_close_price,seasonal = FALSE)

fitB = arima(SPY_close_price, order = c(1,2,4)) # custom arima
tsdisplay(residuals(fitB), lag.max = 40, main = '(1,2,4) Model Residuals')

fitC = arima(SPY_close_price, order = c(5,1,4)) # guess arima
tsdisplay(residuals(fitC), lag.max = 40, main = '(5,1,4) Model Residuals')

fitD = arima(SPY_close_price, order = c(1,1,1)) # standard arima
tsdisplay(residuals(fitD), lag.max = 40, main = '(1,1,1) Model Residuals')

#plotting the models together for comparison
par(mfrow = c(2,2))
term <- 366
#auto arima plot
fcast1 <- forecast(fitB, h = term)
plot(fcast1)
#custom arima plot
fcast2 <- forecast(fitB, h = term)
plot(fcast2)
#guess arima plot
fcast3 <- forecast(fitC, h = term)
plot(fcast3)
#standard arima plot
fcast4 <- forecast(fitD, h = term)
plot(fcast4)

#mape accuracy
accuracy(fcast1) #99.22%
accuracy(fcast2) #99.22%
accuracy(fcast3) #99.22%
accuracy(fcast4) #99.22%