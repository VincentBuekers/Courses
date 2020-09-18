library(Quandl)
library(forecast)
library(CADFtest)
library(vars)
library(urca)

CO2 = Quandl("BP/C02_EMMISSIONS_BEL")
Oil_cons = Quandl("BP/OIL_CONSUM_BEL")
Gas_cons = Quandl("BP/GAS_CONSUM_O_BEL")
Coal_cons = Quandl("BP/COAL_CONSUM_O_BEL")

# Merge Belgian data on Oil consumption with coal consumption data
Data = merge(CO2, Oil_cons, by = "Date")
Data = merge(Data, Gas_cons, by = "Date")
Data = merge(Data, Coal_cons, by = "Date")
colnames(Data) = c("Date","Carbon Emissions", "Oil consumption", "Gas consumption", "Coal consumption")
attach(Data)

# Declare data to be a time series
Gas_ts = ts(`Gas consumption`, start = 1965)
par(mfrow = c(1,2))
ts.plot(Gas_ts)
acf(Gas_ts, main = "")

# Unit root test for series in levels
max.lag = round(sqrt(length(Gas_ts)))
mytest = CADFtest(Gas_ts, type = "trend", criterion = "BIC", max.lag.y = max.lag)
summary(mytest)

# Log-differences
gas_logts = log(Gas_ts)
gas_logdts = diff(log(Gas_ts))
ts.plot(gas_logdts)

# Recompute lag since one observation is lost due to going in differences
max.lag = round(sqrt(length(gas_logdts)))
# Unit root test on series in log-differences
mytest = CADFtest(gas_logdts, type = "drift", criterion = "BIC", max.lag.y = max.lag)
summary(mytest)

par(mfrow = c(1,2))
acf(gas_logdts, main = "")
pacf(gas_logdts, main = "")

# Q-Test 
Box.test(gas_logdts, lag =  max.lag, type = "Ljung-Box")

# ARIMA(3,1,1)
Model1 = arima(gas_logts, order = c(3,1,1))
summary(Model1)
plot(Model1$residuals)
acf(Model1$residuals)
Box.test(Model1$residuals, lag = max.lag, type = 'Ljung-Box')

# Confidence intervals
# AR(1)
c(1.2276 -1.96*0.2791, 1.2276 + 1.96*0.2791)
# AR(2)
c(0.2794 - 1.96*0.3878, 0.2794 + 1.96*0.3878)
# AR(3)
c(-0.5337 - 1.96* 0.2174, -0.5337 + 1.96* 0.2174)
# MA(1)
c(-0.7618 - 1.96*0.2549, -0.7618 + 1.96*0.2549)

# ARIMA (1,1,1)
Model2 = arima(gas_logts, order = c(1,1,1))
summary(Model2)
plot(Mode21$residuals)
acf(Model2$residuals)
Box.test(Model2$residuals, lag = max.lag, type = 'Ljung-Box')

# Confidence intervals
# AR(1) coefficient
c(0.9620 - 1.96*0.0526, 0.9620 + 1.96*0.0526)
# MA(1) coefficient 
c(-0.3276 - 1.96* 0.1524, -0.3276 + 1.96* 0.1524)

# AIC and SIC
AIC(Model1)
AIC(Model2)
AIC(Model1, k = log(51))
AIC(Model2, k = log(51))

# forecast errors for ARMA(3,1)
y = gas_logts
S = round(0.75*length(y))
h = 1
error1.h = c()
for (i in S:(length(y)-h))
{
  mymodel.sub = arima(y[1:i], order = c(3,1,1))
  predict.h = predict(mymodel.sub,n.ahead=h)$pred[h]
  error1.h = c(error1.h,y[i+h]-predict.h)
}
# forecast errors for ARMA(1,1)
y = gas_logts
S = round(0.75*length(y))
h = 1
error2.h = c()
for (i in S:(length(y)-h))
{
  mymodel.sub = arima(y[1:i], order = c(1,1,1))
  predict.h = predict(mymodel.sub,n.ahead=h)$pred[h]
  error2.h = c(error2.h,y[i+h]-predict.h)
}
# MAE
mean(abs(error1.h)); mean(abs(error2.h))
# Diebold-Mariano
dm.test(error1.h,error2.h,h=h,power=1)

# Forecast using ARIMA(1,1,1)
myforecast = predict(Model2, n.ahead = 15)
expected = myforecast$pred
expected

lower = myforecast$pred + qnorm(0.975)*myforecast$se
upper = myforecast$pred - qnorm(0.975)*myforecast$se
cbind(lower,expected,upper)

# Forecast plot
plot.ts(gas_logts, xlim = c(1965, 2030), ylim = c(0, 5))
lines(expected,col="red")
lines(lower,col="blue")
lines(upper,col="blue")

# Multivariate analysis
Oil_ts = ts(`Oil consumption`, start = 1965)
CO2_ts = ts(`Carbon Emissions`, start = 1965)
Coal_ts = ts(`Coal consumption`, start = 1965)

ts.plot(Oil_ts, Coal_ts, CO2_ts, Gas_ts ,col = c("black", "red", "green", "blue"))
legend(2000, 100, c("Oil", "Coal", "CO2", "Gas"), col = c("black", "red", "green", "blue"), lty = c(1,1))
# Unit root on series in levels
max.lag = round(sqrt(length(Coal_ts)))
CADFtest(Coal_ts, type = "trend", criterion = "BIC", max.lag.y = max.lag)
max.lag = round(sqrt(length(Oil_ts)))
CADFtest(Oil_ts, type = "trend", criterion = "BIC", max.lag.y = max.lag)
max.lag = round(sqrt(length(CO2_ts)))
CADFtest(CO2_ts, type = "trend", criterion = "BIC", max.lag.y = max.lag)

# Log-differences
Coal_logts = log(Coal_ts)
Coal_logdts = diff(Coal_logts)
CO2_logts = log(CO2_ts)
CO2_logdts = diff(CO2_logts)
Oil_logts = log(Oil_ts)
Oil_logdts = diff(Oil_logts)

# Unit root on series in log-differences
max.lag = round(sqrt(length(Coal_logdts)))
CADFtest(Coal_logdts, type = "drift", criterion = "BIC", max.lag.y = max.lag)
CADFtest(CO2_logdts, type = "drift", criterion = "BIC", max.lag.y = max.lag)
CADFtest(Oil_logdts, type = "drift", criterion = "BIC", max.lag.y = max.lag)

# Multivariate model
Model_multi = lm(CO2_logdts ~ Coal_logdts + gas_logdts + Oil_logdts)
summary(Model_multi)
plot.ts(Model_multi$residuals)
acf(Model_multi$residuals)
pacf(Model_multi$residuals)
Box.test(Model_multi$residuals, lag = max.lag, type = "Ljung-Box")

# Johansen Procedure
logdata<-data.frame(log(`Carbon Emissions`),log(`Oil consumption`), log(`Gas consumption`), log(`Coal consumption`))
names(logdata) = c("logCO2","logOil", "logGas", "logCoal")
attach(logdata)

VARselect(logdata,lag.max=7,type="const")
trace_test<-ca.jo(logdata,type="trace",K=2,ecdet="const",spec="transitory")
summary(trace_test)

fit_vecm1 = cajorls(trace_test,r=2)
fit_vecm1

fit_var<-vec2var(trace_test,r=2)
myforecast<-predict(fit_var,n.ahead=7)

# forecast plot
par(mfrow=c(1,1))
logCO2_forecast<-ts(myforecast$fcst$logCO2[,1], start=c(2016))
logCO2_lower<-ts(myforecast$fcst$logCO2[,2], start=c(2016))
logCO2_upper<-ts(myforecast$fcst$logCO2[,3],start=c(2016))
ts.plot(logCO2_forecast,logCO2_lower,logCO2_upper,col=c("black","red","red"), ylab = "CO2")