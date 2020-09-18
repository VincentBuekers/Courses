#################
# GLM - Project 1
#################
# Data
library(readxl)
library(dplyr)
library(MASS)
library(mgcv)
library(lmtest)
library(sandwich)

bike_sharing = read_excel("~/Desktop/GLM/Generalized_Linear_Models_[G0A18a]__Exam_project_GLM/bike_sharing.xlsx")

data = bike_sharing %>% 
  dplyr::select(weathersit, workingday, temp, hum, windspeed, season, cnt)

str(data)
summary(data)
data$season = as.factor(data$season)
data$weathersit = as.factor(data$weathersit)

#########
# Poisson 
#########
fit.ps = glm(cnt~ weathersit + workingday + temp + hum + windspeed +season
             ,family = poisson(link='log'), data)
summary(fit.ps)
anova(fit.ps, test = "Chisq")

drop1(fit.ps, test = "Chisq")
fit.ps = update(fit.ps, ~ . -workingday)
drop1(fit.ps, test = "Chisq")
fit.ps = update(fit.ps, ~ . -hum)
drop1(fit.ps, test="Chisq")
fit.ps = update(fit.ps, ~ . -windspeed)

summary(fit.ps)$coef

#########
# GAM
#########
par(mfrow=c(1,3))
plot(data$temp, data$cnt, xlab="Temperature", ylab="Amount of rental bikes")
ordertemp = order(data$temp)
lines(data$temp[ordertemp],predict(loess(data$cnt ~ data$temp))[ordertemp],lwd=2,col="blue")

plot(data$hum,data$cnt, xlab="Humidity", ylab="Amount of rental bikes")
orderhum = order(data$hum)
lines(data$hum[orderhum],predict(loess(data$cnt ~ data$hum))[orderhum],lwd=2,col="blue")

plot(data$windspeed,data$cnt, xlab="Windspeed", ylab="Amount of rental bikes")
orderwind = order(data$windspeed)
lines(data$windspeed[orderwind],predict(loess(data$cnt ~ data$windspeed))[orderwind],lwd=2,col="blue")

dev.off()

# Gam using GCV and poisson log link
# k = -1 implements GCV
fit.gam = gam(cnt~ weathersit + workingday + season
              + s(temp, bs="ps",k=-1) + s(hum,bs="ps",k=-1) + s(windspeed,bs="ps",k=-1)
              ,family = poisson(link="log"), data = data)

summary(fit.gam)
par(mfrow=c(2,2))
gam.check(fit.gam)

# Partial residual plots
par(mfrow=c(1,3))
plot(fit.gam, residuals = TRUE, shade=TRUE, scale = 0)

# Remove outliers
which(bike_sharing$hum<0.2) == which(bike_sharing$windspeed>.5)
data = data[-c(which(data$hum<0.2),which(data$windspeed>.5)),]

# decrease amount of knots
fit.gam2 = gam(cnt~ weathersit + workingday + season
              + s(temp, bs="ps",k=5) + s(hum,bs="ps",k=4) + s(windspeed, bs="ps",k=5)
              ,family = poisson(link="log"), data = data)

summary(fit.gam2)
par(mfrow=c(2,2))
gam.check(fit.gam2)

# Partial residual plots
par(mfrow=c(1,3))
plot(fit.gam2, residuals = TRUE, shade=TRUE, scale = 0)

#------------------------------------
# most predictive regressors for GAM
#------------------------------------
# no smoothing for windspeed and hum
fit.gam3 = gam(cnt~ weathersit + workingday + season
               + s(temp, bs="ps",k=5) + hum + windspeed
               ,family = poisson(link="log"), data = data)
summary(fit.gam3)

# remove workingday
fit.gam4 = gam(cnt ~ weathersit + season + 
               + s(temp, bs="ps",k=5) + hum + windspeed
               , family = poisson(link="log"), data = data)
summary(fit.gam4)

# remove windspeed
fit.gam5 = gam(cnt ~ weathersit + season 
               + s(temp, bs="ps",k=5) + hum
               , family = poisson(link="log"), data = data)
summary(fit.gam5)

#remove hum
fit.gam6 = gam(cnt ~ weathersit + season 
               + s(temp, bs="ps",k=4)
               , family = poisson(link="log"), data = data)
summary(fit.gam6)

# Model comparison
AIC(fit.gam2, fit.gam3, fit.gam4, fit.gam5, fit.gam6)

dev.off()
################
# Model Checking
################
#---------------
# link functions
#---------------
# log link
myexp = function(x,b0,b1){exp = exp(b0+b1*x)}
# sqrt link
fit2.ps = glm(cnt ~ weathersit + temp + season, family = poisson(link='sqrt'), data)
summary(fit2.ps) 
mysqr = function(x,b0,b1){sqr = (b0+b1*x)^2}

# link functions for cnt vs temp
plot(data$temp,data$cnt ,xlab="Normalized temperature", ylab ="Amount of bikes rentals", main="Link functions")
x = seq(min(data$temp), max(data$temp),by=0.001)
lines(x,myexp(x,b0=fit.ps$coeff[1],b1=fit.ps$coeff[4]),lty=2,col="red",lwd=3)
lines(x,mysqr(x,b0=fit2.ps$coeff[1],b1=fit2.ps$coeff[4]), lty=3,col="steelblue",lwd=3)
legend("topleft",legend=c("Log link","√ link"),lty=2:3,col=c("red","steelblue"),cex=1,lwd = 3)

#-------------------
# Deviance residuals
#-------------------
par(mfrow=c(1,2))
r.dev_ps = residuals(fit.ps, type = "deviance")

plot(bike_sharing$temp,r.dev_ps,xlab="Temperature (°C)",ylab="Deviance residual", main="Residuals: linear Poisson")
loess.dev = loess(r.dev_ps~bike_sharing$temp)
lo.pred = predict(loess.dev, se=T)

ordertemp = order(bike_sharing$temp)
lines(bike_sharing$temp[ordertemp],lo.pred$fit[ordertemp],col="blue",lwd=3)
lines(bike_sharing$temp[ordertemp],lo.pred$fit[ordertemp]+2*lo.pred$s[ordertemp], lty=2,col="red")
lines(bike_sharing$temp[ordertemp],lo.pred$fit[ordertemp]-2*lo.pred$s[ordertemp], lty=2,col="red")

r.dev_gam = residuals(fit.gam4, type = "deviance")

plot(data$temp,r.dev_gam,xlab="Tempeture (°C)",ylab="Deviance residual", main="Residuals: smooth Poisson")
loess.dev_gam = loess(r.dev_gam~data$temp)
lo.pred_gam = predict(loess.dev_gam, se=T)

ordertemp = order(data$temp)
lines(data$temp[ordertemp],lo.pred_gam$fit[ordertemp],col="blue",lwd=3)
lines(data$temp[ordertemp],lo.pred_gam$fit[ordertemp]+2*lo.pred_gam$s[ordertemp], lty=2,col="red")
lines(data$temp[ordertemp],lo.pred_gam$fit[ordertemp]-2*lo.pred_gam$s[ordertemp], lty=2,col="red")

# Model adequacy of selected gam (4)
par(mfrow=c(2,2))
gam.check(fit.gam4)
dev.off()

#-------------------
# Prediction
#-------------------
# Histogram of observed and fitted response
par(mfrow=c(1,2))
hist(data$cnt,nclas=20,col="light blue",prob=T,
     xlab="Amount of rental bikes",ylab=" ",main="Observed Count")
hist(round(fitted(fit.gam4),0),nclas=20,col="red",main="Model-based prediction",
     xlab="Amount of rental bikes",ylab=" ",prob=T)
dev.off()

# scale of covariates
data$temp = data$temp*41
data$temp = scale(data$temp, center = TRUE, scale = TRUE)

data$hum = data$hum*100
data$hum = scale(data$hum, center = TRUE, scale = TRUE)

data$windspeed = data$windspeed*67
data$windspeed = scale(data$windspeed, center = TRUE, scale = TRUE)

###################
# Negative binomial
###################
# check Poisson distribution
poismodel0 = glm(cnt ~ 1,family = poisson(link = "log"),data) 
summary(poismodel0)
poissonmu <- exp(summary(poismodel0)$coefficients[,1])

# check negative binomial distribution
ngbinmodel0 <- glm.nb(cnt ~ 1,data) 
summary(ngbinmodel0)

nbmu <- exp(summary(ngbinmodel0)$coefficients[,1])
nbsize <- summary(ngbinmodel0)$theta
nbp <- 1-nbmu/(nbmu+nbsize)
ngbin <- dnbinom(min(data$cnt):max(data$cnt), prob=nbp, size=nbsize, log = FALSE)

# Dispersion plot
hist(data$cnt, nclas=20,col="grey",prob=T,
     xlab="Amount of bike rentals",ylab="Probability",main="Dispersion")
lines(min(data$cnt):max(data$cnt),dpois(min(data$cnt):max(data$cnt),poissonmu),col="red",lwd=3)
lines(min(data$cnt):max(data$cnt),ngbin,col="green",type="l", lwd=3)
legend("topleft",legend=c("Poisson","Negative Binomial"),col=c("red","green"),cex=1,lwd = 3, bty = "n")

# NB model linear
fit.nb =  glm.nb(cnt~ weathersit + workingday + temp + hum + windspeed + season, data)
summary(fit.nb)

drop1(fit.nb, test="Chisq")
fit.nb = update(fit.nb, ~ . -workingday)
drop1(fit.nb, test="Chisq")

# NB model smooth
fit.nb.gam =  gam(cnt~ weathersit + season 
                  + s(temp,bs="ps",k=-1) + s(hum,bs="ps",k=-1) + s(windspeed, bs="ps",k=-1)
                  , family=nb(link = 'log'), data=data)

par(mfrow=c(1,3))
plot(fit.nb.gam, residuals = TRUE, shade = TRUE, scale = 0)

par(mfrow=c(2,2))
gam.check(fit.nb.gam)

fit.nb.gam2 =  gam(cnt~ weathersit + season 
                  + s(temp,bs="ps",k=-1) + hum + windspeed
                  , family=nb(link = 'log'), data=data)
summary(fit.nb.gam2)

par(mfrow=c(1,1))
plot(fit.nb.gam2, residuals = TRUE, shade = TRUE, scale = 0)

par(mfrow=c(2,2))
gam.check(fit.nb.gam2)

################
# Quasi Poisson
################
# quasi poisson linear
fit.quasi =  glm(cnt~ weathersit + workingday + season +  temp + hum + windspeed,
                family = quasipoisson(link="log"), data=data)
summary(fit.quasi)
dfun <- function(object) {
  with(object,sum((weights * residuals^2)[weights > 0])/df.residual)
}
dfun(fit.quasi)

coeftest(fit.quasi,vcov=sandwich)

par(mfrow=c(2,2))
plot(fit.quasi)


