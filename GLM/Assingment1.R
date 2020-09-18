#############################
# Generalized linear models #
#        Assignment I       #
#############################

setwd("~/Desktop/GLM/Assignment")
data = read.csv("student-mat.csv", header=T)

library(tidyverse)

##############
# Analysis I
##############

# subset variables of interest (for analysis I and II)
data.sub = data %>% 
  select(sex, age, Pstatus, Medu, famsup, paid, higher, internet, romantic, Score, absences)
attach(data.sub)

str(data.sub)

# Frequentist logistic starting from null model
model1.logit = glm(Score ~ 1, family=binomial(link="logit"), data.sub)
summary(model1.logit)

# variable age is considered significant addition to model 1
add1(model1.logit,~ . + age + paid + Medu + higher + romantic, test="Chisq")

# add age
model1.logit = update(model1.logit, . ~ . + age)
summary(model1.logit)

# variable higher is considered significant addition to model 1
add1(model1.logit,~ . + paid + Medu + higher + romantic, test="Chisq")

# add higher
model1.logit = update(model1.logit, . ~ . + factor(higher))
summary(model1.logit)

# None of the candidate predictors are considered significant additions 
add1(model1.logit,~ . + paid + Medu + romantic, test="Chisq")

# Frequentist logistic starting from full model
model2.logit = glm(Score ~ age + paid + Medu + higher + romantic,
                   family= binomial(link= "logit"), data.sub)

# Remove paid, romantic and Medu
drop1(model2.logit, test="Chisq")
model2.logit = update(model2.logit, ~ . -paid) 
drop1(model2.logit, test="Chisq")
model2.logit = update(model2.logit, ~ . -romantic)
drop1(model2.logit, test="Chisq")
model2.logit = update(model2.logit, ~ . -Medu)
# Same resulting model irrespective of starting from null or full model specification
summary(model2.logit) 

# Graphical plot of logistic regression analysis
score = model1.logit$linear.predictors
phat = model1.logit$fitted.values

plot(score,phat,xlab="Score",ylab="Response",type="n",
     xlim=c(min(score),max(score)),ylim=c(0,1),
     cex.lab=1.5,cex.axis=1.3)
orderscore = order(score)
points(score, data.sub$Score, col="red",pch=1)
lines(score[orderscore], phat[orderscore], col="blue",lwd=3)

# Comparison with probit
model.probit = glm(Score ~ age + factor(higher), family=binomial(link = "probit"), data.sub)
summary(model.probit)

#-----------------
# Goodness-of-fit
#----------------
# Hosmer-Lemeshow
# function provided in lecture notes reported error due to bug...
hosmerlem = function(y, yhat, g=10) 
{  
  cutyhat = cut(yhat,breaks = quantile(yhat, probs = seq(0,1, 1/g)), include.lowest=TRUE)
  obs = xtabs(cbind(1 - y, y) ~ cutyhat)
  expect = xtabs(cbind(1 - yhat, yhat) ~ cutyhat)
  chisq = sum((obs - expect)^2/expect)  
  P = 1 - pchisq(chisq, g - 2)  
  return(list(chisq=chisq,p.value=P))
}

hosmerlem(y=Score, yhat=fitted(model1.logit))

# Hence I will compare functions from different packages for consistency
library(ResourceSelection)
hoslem.test(data.sub$Score, fitted(model1.logit), g=10)
# Poor fit

library(generalhoslem)
logitgof(data.sub$Score, fitted(model1.logit), g = 10, ord = FALSE)
# reports df = 3, while it should be 8... Not sure if this is a reliable function

#-------------------
# Prediction quality
#-------------------
library(fmsb)
NagelkerkeR2(model1.logit)
# Very poor predictive quality

# Concordance measure
OptimisedConc = function(model)
  {
  Data = cbind(model$y, model$fitted.values) 
  ones = Data[Data[,1] == 1,]
  zeros = Data[Data[,1] == 0,]
  conc=matrix(0, dim(zeros)[1], dim(ones)[1])
  disc=matrix(0, dim(zeros)[1], dim(ones)[1])
  ties=matrix(0, dim(zeros)[1], dim(ones)[1])
  for (j in 1:dim(zeros)[1])
  {
    for (i in 1:dim(ones)[1])
    {
      if (ones[i,2]>zeros[j,2])
      {conc[j,i]=1}
      else if (ones[i,2]<zeros[j,2])
      {disc[j,i]=1}
      else if (ones[i,2]==zeros[j,2])
      {ties[j,i]=1}
    }
  }
  Pairs = dim(zeros)[1]*dim(ones)[1]
  PercentConcordance = (sum(conc)/Pairs)*100
  PercentDiscordance = (sum(disc)/Pairs)*100
  PercentTied = (sum(ties)/Pairs)*100
  return(list("Percent Concordance" = PercentConcordance,
              "Percent Discordance" = PercentDiscordance,
              "Percent Tied" = PercentTied,
              "Pairs" = Pairs))
}

OptimisedConc(model1.logit)
# Concordance measure: C = 51.36%
 
#----------------------
# Residual diagnostics
#----------------------
# Deviance residuals
r.dev = residuals(model1.logit, type = "deviance")
summary(r.dev)
par(mfrow=c(1,2))
hist(r.dev)

plot(data.sub$age,r.dev,xlab="Age (years)",ylab="Deviance residual",
     cex.lab=1.5,cex.axis=1.3)
loess.dev <- loess(r.dev~data.sub$age)
lo.pred <- predict(loess.dev, se=T)

orderage <- order(data.sub$age)
lines(data.sub$age[orderage],lo.pred$fit[orderage],col="blue",lwd=3)
lines(data.sub$age[orderage],lo.pred$fit[orderage]+2*lo.pred$s[orderage], lty=2,col="red")
lines(data.sub$age[orderage],lo.pred$fit[orderage]-2*lo.pred$s[orderage], lty=2,col="red")

# Pearson residuals
r.pear <- residuals(model1.logit, type = "pearson")
summary(r.pear)
hist(r.pear)

plot(data.sub$age,r.pear,xlab="Age (years)",
     ylab="Pearson residual",
     cex.lab=1.5,cex.axis=1.3)

lines(data.sub$age[orderage],lo.pred$fit[orderage],col="blue",lwd=3)
lines(data.sub$age[orderage],lo.pred$fit[orderage]+2*lo.pred$s[orderage], lty=2,col="red")
lines(data.sub$age[orderage],lo.pred$fit[orderage]-2*lo.pred$s[orderage], lty=2,col="red")

#----------
# Bayesian
#----------
library(MCMCpack)

data.sub$Score = ifelse(data.sub$Score == "Pass", 1, 0)

model1.bayes = MCMClogit(Score ~ age + higher, family=binomial, data.sub)
summary(model1.bayes)

##############
# Analysis II
##############

#------------
# Frequentist
#------------
# Poisson regression
model0.ps = glm(absences ~ 1, family = poisson(link = "log"), data.sub)
summary(model0.ps)

model1.ps = glm(absences ~ sex + Pstatus + famsup + internet,
                family = poisson(link = "log"), data.sub)
summary(model1.ps)

# Check covariate significance
drop1(model1.ps, . ~ ., test="LRT")
model2.ps = update(model1.ps, . ~ . - famsup)
summary(model2.ps)

#---------
# Bayesian
#---------
model2.bayes = MCMCpoisson(absences ~ Pstatus + internet, data.sub)
summary(model2.bayes)

