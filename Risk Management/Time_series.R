setwd("~/Desktop/Statistical Tools for Quantitative Risk Management/Assignment")
library(CADFtest)
library(tseries)
library(forecast)
library(zoo)

##############
# Part I - IBM
##############
ibm = read.table("IBM.txt", header = TRUE, dec = ".", row.names = NULL)

#------
# A
#------
# Create data objects
# *zoo: used for irregular time series such that plot axes will have appropriate dates
x = ts(ibm[,2])
x_diff = diff(x)
zoo <- read.zoo(ibm, header=TRUE, format = "%m/%d/%Y")
zoo.diff = diff(zoo)

mean(x)

# Time series plots of series in levels and differences
par(mfrow=c(1,2))
plot(zoo, xlab = "Time", ylab="log-price")
plot(zoo.diff, ylab= "Differenced log-price", xlab="Time")

# Correlograms of series in levels and differences
acf(x, main= "Series in levels")
acf(x_diff, main = "Series in differences")
pacf(x, main="series in levels")
pacf(x_diff, main="series in differences")

# Ljung-Box Test
max.lag = round(log(length(x)))
Box.test(x, lag = max.lag, type="Ljung-Box")
max.lag = round(log(length(x_diff)))
Box.test(x_diff, lag = max.lag, type = "Ljung-Box")

#--------
# B
#--------
# AR model
AR1 = arima(x, order=c(1,0,0))
polyroot(c(1, -AR1$coef[1]))

adf.test(x)

# Integrated AR model
AR1_1 = arima(x, order=c(1,1,0))
AR1_1
polyroot(c(1, -AR1_1$coef[1]))

# Adf test
adf.test(x_diff)

# MA term not significant
ARIMA = arima(x, order=c(1,1,1))
ARIMA

#--------
# C
#--------
# residual plot & residual correlogram
ts.plot(AR1_1$residuals, main="ARIMA(1,1,0) residuals")
acf(AR1_1$residuals, main="Residual correlogram")

# Ljung-box test
Box.test(AR1_1$residuals, lag = max.lag, type = "Ljung-Box")

#--------
# D
#--------
# Retrained model leaving out last 5 observations
AR1_1 = arima(x[-c(598,599,600,601,602)], order=c(1,1,0))
AR1_1

# forecast
forecast = predict(AR1_1,n.ahead=5)
expected = forecast$pred
observed = x[c(598,599,600,601,602)]

# Control limits
LCL = forecast$pred-qnorm(0.975)*forecast$se
UCL = forecast$pred+qnorm(0.975)*forecast$se
cbind(LCL,expected,observed,UCL)

# predictions plot
par(mfrow=c(1,1))
ts.plot(expected,LCL,UCL,col=c("blue", "red" ,"red"), main = "5-step-ahead forecast")
lines(x)

###############
# Simalted Data
###############
# clear environment and load data
rm(list = ls())
data = read.table("data.txt", sep="")

#-------
# A
#-------
dim(data)
x = ts(data$x)
x_diff = diff(x)

mean(x)

# plots of series in levels and in differences
par(mfrow=c(1,2))
ts.plot(x, ylab = "x", main = "Series in levels")
ts.plot(x_diff, ylab = 'x_t - x_t-1', main = "First differences")

# Ljung-Box
max.lag = round(log(length(x)))
Box.test(x, lag = max.lag, type="Ljung-Box")
max.lag = round(log(length(x_diff)))
Box.test(x_diff, lag = max.lag, type="Ljung-Box")

# (partial) autocorrelations
par(mfrow=c(2,2))
acf(x, main="Series in levels")
acf(x_diff, main="Series in differences")
pacf(x, main="Series in levels")
pacf(x_diff, main="Series in differences")

#------
# B
#-----
# AIC and BIC values for different ARMA models
x.fitARMA = matrix(0, nrow=9,ncol=4)
x.fitARMA[1:3,1] = 0
x.fitARMA[4:6,1] = 1
x.fitARMA[7:9,1] = 2
x.fitARMA[1:3,1] = 0:2
x.fitARMA[4:6,2] = 0:2
x.fitARMA[7:9,2] = 0:2

for (i in 1:9){
  fit = arima(x, order=c(x.fitARMA[i,1], 0, x.fitARMA[i,2]))
  x.fitARMA[i,3] = AIC(fit)
  x.fitARMA[i,4] = x.fitARMA[i,3] + log(length(x)-2)*i
}

colnames(x.fitARMA) = c('p', "q", "AIC", "BIC")
x.fitARMA

# ARMA Model
ARMA = arima(x, order=c(1,0,1))
ARMA
polyroot(c(1, -ARMA$coef[1]))
adf.test(x)

# integrated ARMA model
ARIMA = arima(x, order=c(1,1,1))
ARIMA
polyroot(c(1, -ARIMA$coef[1]))
adf.test(x_diff)

#-----
# C
#-----
# forecast for ARMA
forecast_1 = predict(ARMA,n.ahead=20)
expected_1 = forecast_1$pred

# control limits ARMA
LCL_1 = forecast_1$pred-qnorm(0.995)*forecast_1$se
UCL_1 = forecast_1$pred+qnorm(0.995)*forecast_1$se
cbind(LCL_1,expected_1,UCL_1)

# forecast for ARIMA
forecast_2 = predict(ARIMA,n.ahead=20)
expected_2 = forecast_2$pred

# control limits ARIMA
LCL_2 = forecast_2$pred-qnorm(0.995)*forecast_2$se
UCL_2 = forecast_2$pred+qnorm(0.995)*forecast_2$se
cbind(LCL_2,expected_2,UCL_2)

# plots
par(mfrow=c(1,2))
ts.plot(expected_1,LCL_1,UCL_1,col=c("blue", "red" ,"red"), main = "Without differentiation")
ts.plot(expected_2,LCL_2,UCL_2,col=c("blue", "red" ,"red"), main = "With differentiation")
