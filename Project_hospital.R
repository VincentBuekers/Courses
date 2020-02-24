setwd("~/Desktop/Regression Analysis/Project")

#################
# Hospital Data #
#################
rm(list=ls())
hospital.full = read.table("hospital.txt", header = TRUE)

set.seed(0754046)
h.test <- sample(1:dim(hospital.full)[1], 20 )
hospital.test <- hospital.full[h.test, ]
hospital <- hospital.full[-h.test, ]
attach(hospital)

#-----------------#
# Model selection #
#-----------------#
n = dim(hospital)[1]
p = dim(hospital)[2] 

library(MASS)
# Backward elimination
fit.full <- lm(irisk ~ lstay + age + cultratio + xrayratio + nbeds + census + nnurse + facil,
               data = hospital)
stepAIC(fit.full, scope = list(upper = ~ lstay + age + cultratio + 
                                 xrayratio + nbeds + census + nnurse + facil, 
                               lower = ~ 1), direction = "backward")
# Forward elimination
fit.null <- lm(irisk ~ 1, data = hospital)
stepAIC(fit.null, scope = list(upper = ~ lstay + age + cultratio + xrayratio + nbeds + census + nnurse + facil,
                               lower = ~ 1), direction = "forward")
# Stepwise selection full
stepAIC(fit.full, scope = list(upper = ~ lstay + age + cultratio + xrayratio + 
                                 nbeds + census + nnurse + facil,
                               lower = ~ 1), direction = "both")
# Stepwise selection null
stepAIC(fit.null, scope = list(upper = ~ lstay + age + cultratio + xrayratio + 
                                 nbeds + census + nnurse + facil,
                               lower = ~ 1), direction = "both")

# Backward elimination based on F-statistic/t-statistic
dropterm(fit.full, test = "F")
fit1 <- update(fit.full, ~ . - nnurse)
dropterm(fit1, test = "F")
fit2 <- update(fit1, ~ . - nbeds)
dropterm(fit2, test = "F")
fit3 <- update(fit2, ~ . - census)
dropterm(fit3, test = "F")
fit4 <- update(fit3, ~ . - age)
dropterm(fit4, test = "F")
fit5 <- update(fit4, ~ . - xrayratio)
dropterm(fit5, test = "F")

# Forward selection based on F-statistic/t-statistic
addterm(fit.null, ~ . + lstay + age + cultratio + xrayratio + nbeds + census + nnurse + facil, test = "F")
fit1 <- update(fit.null, ~ . + lstay)
addterm(fit1, ~ . + age + cultratio + xrayratio + nbeds + census + nnurse + facil, test = "F")
fit2 <- update(fit1, ~ . + cultratio)
addterm(fit2, ~. + age + xrayratio + nbeds + census + nnurse + facil, test = "F")
fit3 <- update(fit2, ~ . + facil)
addterm(fit3, ~. + age + xrayratio + nbeds + census + nnurse, test = "F")

# Scater plot of considered predictors and irisk
pairs(hospital[,c(3,1,4,5,9)], panel = function(x,y) {points(x,y); lines(lowess(x,y), col = "red")})

library(GGally)
hospital.selected = hospital[,c(3,1,4,5,9)]
ggpairs(hospital.selected)


#--------#
# MODELS #
#--------#

Model1 = lm(irisk ~ lstay + cultratio + xrayratio + facil)
summary(Model1)

Model2 = lm(irisk ~ lstay + cultratio + facil)
summary(Model2)

# Model Comparison
#PRESS
PRESS1 <- sum((residuals(Model1) / (1 - lm.influence(Model1)$hat))^2)
PRESS2 <- sum((residuals(Model2) / (1 - lm.influence(Model2)$hat))^2)
PRESS <- c(PRESS1/n, PRESS2/n)
names(PRESS) <- c("Model1", "Model2")
PRESS
# MSE
MSE1 <- summary(Model1)$sigma^2
MSE2 <- summary(Model2)$sigma^2
MSE <- c(MSE1, MSE2)
names(MSE) <- c("model1", "model2")
MSE
# MSEP
MSEP1 <- mean((predict(Model1, newdata = hospital.test) - irisk)^2)
MSEP2 <- mean((predict(Model2, newdata = hospital.test) - irisk)^2)
MSEP <- c(MSEP1, MSEP2)
names(MSEP) <- c("model1", "model2")
MSEP

# Model 2 validation
model2.res = residuals(Model2)
model2.stdres = stdres(Model2)
model2.fittedvalues = fitted.values(Model2)
par(mfrow = c(2,2))
qqnorm(model2.stdres, main="")
qqline(model2.stdres)
plot(model2.res, xlab = "Index", ylab = "Residual")
plot(model2.fittedvalues, model2.stdres, xlab = "Fitted value", ylab = "Standardized Residual")
lines(lowess(model2.stdres ~ model2.fittedvalues), col = "red")
plot(model2.stdres, xlab = "Index", ylab = "Standardized residual", ylim = c(-3,3))
abline(h = -2.5, lty = 2)
abline(h = 2.5, lty = 2)

# Partial residual plots
library(car)
crPlots(Model2, main="", ylab="Partial Residuals")

# Proportion transformation
hospital = data.frame(hospital, facil.logit = logit(facil, percents = TRUE))
attach(hospital)

# Model 3
Model3 = lm(irisk ~ lstay + log(cultratio) + facil.logit, data = hospital)
summary(Model3)

# Model 3 validation
model3.res = residuals(Model3)
model3.stdres = stdres(Model3)
model3.fittedvalues = fitted.values(Model3)
par(mfrow = c(2,2))
qqnorm(model3.stdres, main="")
qqline(model3.stdres)
shapiro.test(model3.stdres)

plot(model3.res, xlab = "Index", ylab = "Residual")

plot(model3.fittedvalues, model3.stdres, xlab = "Fitted value", ylab = "Standardized Residual")
lines(lowess(model3.stdres ~ model3.fittedvalues), col = "red")

plot(model3.stdres, xlab = "Index", ylab = "Standardized residual", ylim = c(-3,3))
abline(h = -2.5, lty = 2)
abline(h = 2.5, lty = 2)
identify(c(1:93), model2.stdres)
durbin.watson()


##############
# Prediction #
##############

library(magrittr)
library(dplyr)
hospital.test = data.frame(hospital.test, facil.logit = logit(hospital.test$facil, percents = TRUE))
y = hospital.test$irisk
x = hospital.test %>% select(lstay, cultratio, facil.logit) %>% data.frame()
# Prediction
y.hat = predict(Model3, newdata = x)
# Sum of Squares Total and Error
SST = sum((y - mean(y))^2)
SSE = sum((y.hat - y)^2)
# R squared
RSQ = 1 - SSE / SST
RSQ
# MSEP
MSEP <- mean((y.hat - y)^2)
MSEP

#-------#
# Ridge #
#-------#

library(glmnet)
# Select variables from model 2
y = hospital.full$irisk
x.reduced <- hospital.full %>% select(lstay, cultratio, facil) %>% data.matrix()
lambdas <- 10^seq(3, -2, by = -.1)
# Cross validation ridge fit
cv_fit = cv.glmnet(x.reduced, y, alpha = 0, lambda = lambdas)
# Optimal lambda
opt_lambda = cv_fit$lambda.min
# Ridge fit
fit.ridge = cv_fit$glmnet.fit
# Prediction
y.hat <- predict(fit.ridge, s = opt_lambda, newx = x.reduced)
# Sum of Squares Total and Error
SST = sum((y - mean(y))^2)
SSE = sum((y.hat - y)^2)
# R squared
RSQ = 1 - SSE / SST
RSQ
# MSEP
MSEP <- mean((y.hat - y)^2)
MSEP

# Select all predictors
y <- hospital.full$irisk
x.full <- hospital.full %>% select(lstay, age, cultratio, xrayratio, nbeds, census, nnurse, facil) %>% data.matrix()
lambdas <- 10^seq(3, -2, by = -.1)
# Cross-valdition fit
cv_fit = cv.glmnet(x.full, y, alpha = 0, lambda = lambdas)
# Optimal Lambda
opt_lambda = cv_fit$lambda.min
# Ridge fit
fit2.ridge = cv_fit$glmnet.fit
# Prediction
y.hat <- predict(fit2.ridge, s = opt_lambda, newx = x.full)
# Sum of Squares Total and Error
SST = sum((y - mean(y))^2)
SSE = sum((y.hat - y)^2)
# R squared
RSQ = 1 - SSE / SST
RSQ
# MSEP
MSEP <- mean((y.hat - y)^2)
MSEP
