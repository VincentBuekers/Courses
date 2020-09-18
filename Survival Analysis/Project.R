####################
# Vincent Buekers 
# Project 
####################
library(survival)

setwd("~/Desktop/Survival & Reliability/Project")
data = read.table('LungNew.txt', header = TRUE, dec = '.')

n = dim(data)[1]
p = dim(data)[2]

#-----------
# Question 1
#-----------
# Kaplan-Meier Estimate
KM = survfit(Surv(time, status)~1,conf.type='log-log', data)
summary(KM)

plot(KM, main="K-M Estimates", xlab = "Time", ylab = "Survival")
legend(1200, 0.8, lty = c(1,2), legend=c("K-M Curve","Log-log CI"))

# time at which only 35% of patients are still alive according to KM estimates
KM$time[which(KM$surv<=.35)][1]

# Residual lifetime
res_lifetime = (1- KM$surv[which(KM$time>800)][1] - (1 - KM$surv[which(KM$time>500)][1]))/KM$surv[which(KM$time>500)][1]

# Delta method
Var_surv = KM$surv*(1-KM$surv)/n
pdf = dnorm(KM$surv)
Greenwood = (KM$surv)^2*(KM$n.event)/((KM$n.risk)*(KM$n.risk-KM$n.event))

Var_reslife = ((pdf/KM$surv)^2)*Greenwood
SE_reslife = sqrt(Var_reslife)

index = which(KM$time>800)[1]
SE_reslife[index]

#-----------
# Question 2
#-----------
# Kaplan-Meier Estimates based on gender
KM2 = survfit(Surv(time, status)~gender,data)
KM2

plot(KM2, main="K-M Estimates by Gender", xlab = "Time", ylab = "Survival", lty=c(1,2))
legend(1200, .9, lty = c(1,2), legend=c("Male","Female"))

survdiff(Surv(time, status)~gender, rho=1 ,data)

# Patients on list for over a year
survdiff(Surv(time[which(time>365)], status[which(time>365)])~gender[which(time>365)], rho=1 ,data)

# Stratified by disease
KM3 = survfit(Surv(time, status)~gender+strata(disease),data)
KM3

par(mfrow=c(1,2))
plot(KM3[c(1,3)], lty = c(1,2), main="COPD", xlab="Time", ylab = "Survival")
legend(500, 1, legend=c("Male", "Female"),lty = c(1,2), bty = "n")
plot(KM3[c(2,4)], lty=c(1,2), main="Fibrosis", xlab="Time", ylab = "Survival")
legend(500, 1, legend=c("Male", "Female"),lty = c(1,2), bty="n")

# stratified test
survdiff(Surv(time, status)~gender+strata(disease),data)

#------------
# Question 3
#------------
# Cox Regression Model
fit1 = coxph(Surv(time, status)~ age + (gender==1) + bmi + disease, data)
cox.summary = summary(fit1)
cox.summary

# Parsimonious model
fit2 = coxph(Surv(time, status)~ (gender==1) + disease, data) 
summary(fit2)
anova(fit1, fit2)

# Interaction model
cox.int = coxph(Surv(time, status)~ age*gender*disease*bmi, data)
summary(cox.int)

# Hazard Ratio Males
HR_Male = cox.summary$coefficients[2,2]
HR_Male

# Predictions for patient 1 & 2
patient1 = survfit(coxph(Surv(time, status)~age+gender+bmi+disease, data)
                   , type='breslow'
                   , newdata = data.frame(gender=1, disease=2, age=60, bmi=23))
patient1
plot(patient1, main="Patient 1",  xlab = "Time", ylab = "Survival")

patient2 = survfit(coxph(Surv(time, status)~age+gender+bmi+disease, data)
                   , type='breslow'
                   , newdata = data.frame(gender=2, disease=1, age=40, bmi=19))
patient2
plot(patient2, main="Patient 2", xlab = "Time", ylab = "Survival")

# Residual diagnostics
fit.res = residuals(fit1, type="martingale")
fit.pred = predict(fit1)

par(mfrow=c(2,2))
plot(fit.res, ylab="residuals", main="Residuals vs. Index")

plot(fit.pred,fit.res,xlab="prediction",ylab="residuals", main="Residuals vs. fitted values")
lines(lowess(fit.pred,fit.res))

plot(data$age,fit.res,xlab="age",ylab="residuals", main='Residuals vs. age')
lines(lowess(data$age,fit.res))

plot(data$bmi,fit.res,xlab="BMI",ylab="residuals", main='Residuals vs. BMI')
lines(lowess(data$bmi,fit.res))

# proportional hazard assumption 
fit.gender = survfit(Surv(time,status) ~ gender, data)

par(mfrow=c(1,2))
plot(log(fit.gender[2]$time),log(-log(fit.gender[2]$surv)),xlab="log(Time)",ylab="log(-log(Survival)",
     type="s", main = "Gender")
lines(log(fit.gender[1]$time),log(-log(fit.gender[1]$surv)),lty=2,type="s")

fit.disease = survfit(Surv(time,status) ~ disease, data)

plot(log(fit.disease[2]$time),log(-log(fit.disease[2]$surv)),xlab="log(Time)",ylab="log(-log(Survival)",
     type="s", main="Disease")
lines(log(fit.disease[1]$time),log(-log(fit.disease[1]$surv)),lty=2,type="s")

# stratified cox by strata of disease
cox.strat = coxph(Surv(time, status)~ age + gender + bmi + strata(disease), data)
summary(cox.strat)

#-----------
# Question 4
#-----------
# Distributional fit
fit.dist = survfit(Surv(time, status)~1, conf.type="none", data)
par(mfrow=c(2,2))
plot(fit.dist, xlab = "time", ylab = "Survival", main="K-M Estimate")

plot(log(fit.dist$time), log(-log(fit.dist$surv)), type="s"
     , xlab = "log(time)", ylab="log(-log(Survival))", main="Weibull")

plot(fit.dist$time, log(fit.dist$surv), type="s"
     , xlab = "time", ylab="-log(Survival)", main='Exponential')

plot(log(fit.dist$time), qnorm(1-fit.dist$surv), type="s"
     , xlab = "log(time)", ylab="log(-log(Survival))", main="Log-normal")

# AFT
aft_exp = survreg(Surv(time, status)~ age+(gender==1)+bmi+disease, dist = 'exponential', data)
summary(aft_exp)

exp(-aft_exp$coefficients[5])

# Prediction
predict.aft = predict(aft_exp, newdata = list(age=60, bmi=23, gender=1, disease=2)
                      , se.fit = TRUE, type = "response")
predict.aft$fit
predict.aft$se.fit
par(mfrow=c(1,1))
pct = 1:99/100
ptime = predict(aft_exp, newdata = list(age=60, bmi=23, gender=1, disease=2), type='quantile',
                 p=pct, se=TRUE)
matplot(cbind(ptime$fit, ptime$fit + 1.96*ptime$se.fit, ptime$fit - 1.96*ptime$se.fit)
        , 1-pct, xlab="Time", ylab="Survival", type='s', lty=c(1,2,2),col=1, main='Prediction Patient 1')


