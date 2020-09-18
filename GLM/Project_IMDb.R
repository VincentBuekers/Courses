#################
# GLM - Project 2
#################
# Data
library(readxl)
library(dplyr)
library(mgcv)
library(lattice)
library(ggplot2)
library(grid)
library(splines)
library(fmsb)

IMDb = read_excel("~/Desktop/GLM/Generalized_Linear_Models_[G0A18a]__Exam_project_GLM/IMDb.xlsx")

data = IMDb %>%
  dplyr::select(budget, profit, director_facebook_likes, content_rating, duration)

str(data)
data$content_rating = as.factor(data$content_rating)

##############
# Question 1
##############
profit = data$profit
budget = data$budget
content_rating=data$content_rating
duration = data$duration
director_facebook_likes=data$director_facebook_likes

xyplot(profit~budget|content_rating, main = "Profit vs. Budget based on content rating",   
       ylab = "Profit", xlab = "Budget")  
xyplot(profit~director_facebook_likes|content_rating, main = "Profit vs. Director's Facebook likes based on content rating",   
       ylab = "Profit", xlab = "Number of likes of director on his Facebook page")  
bwplot(profit~content_rating, main = "Profit vs. Content Rating", ylab = "Profit")  

#correlations between variables of interest per content_rating  
data %>%  
  group_by(content_rating) %>%  
  summarize(COR=cor(profit, budget))  

data %>%  
  group_by(content_rating) %>%  
  summarize(COR=cor(profit, director_facebook_likes))  

data %>%  
  group_by(content_rating) %>%  
  summarize(COR=cor(budget, director_facebook_likes))  

#same idea as above, but these are p-values for correlations based on Pearson rank correlation:  
data %>%  
  group_by(content_rating) %>%  
  summarize(p=Hmisc::rcorr(cbind(profit, budget))$P[1,2])  

data %>%  
  group_by(content_rating) %>%  
  summarize(p=Hmisc::rcorr(cbind(profit, director_facebook_likes))$P[1,2])  

data %>%  
  group_by(content_rating) %>%  
  summarize(p=Hmisc::rcorr(cbind(budget, director_facebook_likes))$P[1,2])  

#############
# Question 2
#############

#---------------------
# Globabl Polynomials
#---------------------
imdb.poly2 <- lm(profit ~ budget + I(budget^2))
summary(imdb.poly2)

imdb.poly3 <- lm(profit ~ budget + I(budget^2) + I(budget^3))
summary(imdb.poly3)

imdb.poly7 <- lm(profit ~ budget + I(budget^2) + I(budget^3) + I(budget^4) + I(budget^5)
                 + I(budget^6) + I(budget^7))
summary(imdb.poly7)

prd <- data.frame(budget = seq(from = range(budget)[1], to = range(budget)[2], length.out = 100))
err <- predict(imdb.poly2, newdata = prd, se.fit = TRUE)
prd$lci <- err$fit - 1.96 * err$se.fit
prd$fit <- err$fit
prd$uci <- err$fit + 1.96 * err$se.fit
p1 <- ggplot(prd, aes(x = budget, y = fit)) +
  theme_bw() +
  geom_line() +
  geom_smooth(aes(ymin = lci, ymax = uci), stat = "identity") +
  geom_point(data = data, aes(x = budget, y = profit)) +
  labs(x="budget",y="profit",title = "Poly regression degree=2")


err <- predict(imdb.poly3, newdata = prd, se.fit = TRUE)
prd$lci <- err$fit - 1.96 * err$se.fit
prd$fit <- err$fit
prd$uci <- err$fit + 1.96 * err$se.fit
p2 <- ggplot(prd, aes(x = budget, y = fit)) +
  theme_bw() +
  geom_line() +
  geom_smooth(aes(ymin = lci, ymax = uci), stat = "identity") +
  geom_point(data = data, aes(x = budget, y = profit)) +
  labs(x="budget",y="profit",title = "Poly regression degree=3")

err <- predict(imdb.poly7, newdata = prd, se.fit = TRUE)
prd$lci <- err$fit - 1.96 * err$se.fit
prd$fit <- err$fit
prd$uci <- err$fit + 1.96 * err$se.fit
p3 <- ggplot(prd, aes(x = budget, y = fit)) +
  theme_bw() +
  geom_line() +
  geom_smooth(aes(ymin = lci, ymax = uci), stat = "identity") +
  geom_point(data = data, aes(x = budget, y = profit)) +
  labs(x="budget",y="profit",title = "Poly regression degree=7")

grid.newpage()
pushViewport(viewport(layout = grid.layout(1,3)))
vplayout <- function(x,y){viewport(layout.pos.row = x, layout.pos.col = y)}  
print(p1, vp = vplayout(1,1))
print(p2, vp = vplayout(1,2))    	 
print(p3, vp = vplayout(1,3))

#----------------------------------
# Truncated splines of degree 2
#----------------------------------
par(mfrow=c(1,3))
# 2 Knots
budget_eval <- seq(0,300,length=500)
knots <- c(0,300)
b1 <- budget
b2 <- budget^2
b_eval <- matrix(0,500,3)
b_eval[,1] <- rep(1,500)
b_eval[,2] <- budget_eval
b_eval[,3] <- budget_eval^2

lm <- lm(profit~b1+b2)
fitted <- round(b_eval%*%coef(lm),5)

plot(budget,profit,xlab="budget",ylab="Profit",
     cex.lab=1.5,cex.axis=1.3,col="red",cex=1.3,
     main="degree of spline = 2, 2 knots")
lines(budget_eval,fitted,lwd=3,col="blue")

# 3 Knots
budget_eval <- seq(0,300,length=500)
knots <- c(0,150,300)
b1 <- budget
b2 <- budget^2
b3 <- (budget-knots[2])^2*(budget > knots[2])
b_eval <- matrix(0,500,4)
b_eval[,1] <- rep(1,500)
b_eval[,2] <- budget_eval
b_eval[,3] <- budget_eval^2
b_eval[,4] <- (budget_eval-knots[2])^2*(budget_eval > knots[2])

lm <- lm(profit~b1+b2+b3)
fitted <- round(b_eval%*%coef(lm),5)

plot(budget,profit,xlab="budget",ylab="Profit",
     cex.lab=1.5,cex.axis=1.3,col="red",cex=1.3,
     main="degree of spline = 2, 3 knots")
lines(budget_eval,fitted,lwd=3,col="blue")

# 5 Knots
budget_eval <- seq(0,300,length=500)
knots <- c(0,75,150,225,300)
b1 <- budget
b2 <- budget^2
b3 <- (budget-knots[2])^2*(budget > knots[2])
b4 <- (budget-knots[3])^2*(budget > knots[3])
b5 <- (budget-knots[4])^2*(budget > knots[4])
b_eval <- matrix(0,500,6)
b_eval[,1] <- rep(1,500)
b_eval[,2] <- budget_eval
b_eval[,3] <- budget_eval^2
b_eval[,4] <- (budget_eval-knots[2])^2*(budget_eval > knots[2])
b_eval[,5] <- (budget_eval-knots[3])^2*(budget_eval > knots[3])
b_eval[,6] <- (budget_eval-knots[4])^2*(budget_eval > knots[4])

lm <- lm(profit~b1+b2+b3+b4+b5)
fitted <- round(b_eval%*%coef(lm),5)

plot(budget,profit,xlab="budget",ylab="Profit",
     cex.lab=1.5,cex.axis=1.3,col="red",cex=1.3,
     main="degree of spline = 2, 5 knots")
lines(budget_eval,fitted,lwd=3,col="blue")

dev.off()

#----------
# B-splines
#----------
x <- budget
y <- profit
xseq <- seq(range(x)[1],range(x)[2],length=500)

par(mfrow=c(1,3))

# 3 knots
knots=c(0, 150, 300)
fit<-lm(y ~ bs(x, degree=2 ,knots=knots),data = data)

plot(x,y,xlab="Budget",ylab="Profit", main="degree= 2, 3 knots",
     cex.lab=1.5,cex.axis=1.3,col="red",cex=1.3)
points(xseq,predict(fit,newdata = list(x=xseq)),col="blue",lwd=3,type="l")

# 5 knots
knots=c(0, 75, 150, 225, 300)
fit<-lm(y ~ bs(x, degree=2 ,knots=knots),data = data )

plot(x,y,xlab="Budget",ylab="Profit", main="degree= 2, 5 knots",
     cex.lab=1.5,cex.axis=1.3,col="red",cex=1.3)
points(xseq,predict(fit,newdata = list(x=xseq)),col="blue",lwd=3,type="l")

# 8 knots
knots=c(0, 300/7, 2*300/7, 3*300/7, 4*300/7, 5*300/7, 6*300/7, 7*300/7)
fit<-lm(y ~ bs(x, degree=2 ,knots=knots),data = data )

plot(x,y,xlab="Budget",ylab="Profit", main="degree= 2, 8 knots",
     cex.lab=1.5,cex.axis=1.3,col="red",cex=1.3)
points(xseq,predict(fit,newdata = list(x=xseq)),col="blue",lwd=3,type="l")

dev.off()

#----------------
# Cubic P-splines
#----------------
lims = range(data$budget)
grid = seq(from=lims[1], to = lims[2])

fit.cubic5 = gam(profit~ s(budget, bs="cr", k=5), data = data)
fit.cubic8 = gam(profit~ s(budget, bs="cr", k=8), data = data)
fit.cubic20 = gam(profit~ s(budget, bs="cr", k=20), data = data)

par(mfrow=c(1,3))
plot(data$budget,data$profit ,col="grey",  main = '5 knots', xlab = "Budget", ylab = "Profit")
points(grid, predict(fit.cubic5, newdata = list(budget=grid)), lwd=2, type="l")

plot(data$budget,data$profit ,col="grey",  main = '8 knots', xlab = "Budget", ylab = "Profit")
points(grid, predict(fit.cubic8, newdata = list(budget=grid)), lwd=2, type="l")

plot(data$budget,data$profit ,col="grey",  main = '20 knots', xlab = "Budget", ylab = "Profit")
points(grid, predict(fit.cubic20, newdata = list(budget=grid)), lwd=2, type="l")

dev.off()
# penalty parameters
fit.cubic5$sp
fit.cubic8$sp
fit.cubic20$sp

# Summary
summary(fit.cubic20)

# linear model is preferred over smoothing
fit.lm = lm(profit~ budget, data=data)
AIC(fit.cubic20, fit.lm, fit.cubic5, fit.cubic8)

#############
# Question 3
#############
fit.lm = update(fit.lm, ~. + content_rating + duration + director_facebook_likes)
summary(fit.lm)
drop1(fit.lm, test = "F")
fit.lm = update(fit.lm, ~. - duration)
summary(fit.lm)
drop1(fit.lm, test = "F")
fit.lm = update(fit.lm, ~. - content_rating)
summary(fit.lm)

fit.lm1 = lm(profit~ budget + director_facebook_likes, data)
fit.lm2 = lm(profit~ budget + director_facebook_likes + content_rating, data)
fit.lm3 = lm(profit~ budget + director_facebook_likes + content_rating + duration, data)

budget.Z = scale(data$budget, center = TRUE, scale = TRUE)

fit.lm = lm(profit ~ budget.Z + director_facebook_likes, data)
summary(fit.lm)

############
# Question 4
############
# create indicator variable
data$indicator = ifelse(data$profit > 0, 1, 0)  

# Check distribution of profits and losses
table(data$indicator)

# logistic regression logit link
fit.logit = glm(indicator ~ budget + director_facebook_likes, family = binomial(link='cloglog'), data[,-2])
summary(fit.logit)

anova(fit.logit, test="Chisq")

# quasi for overdispersion
fit.quasi = glm(indicator ~ budget + director_facebook_likes, family = quasibinomial(link='logit'), data[,-2])
summary(fit.quasi)

# multipartial check
length(budget)
plot(budget,director_facebook_likes,xlab="budget",ylab="likes",
     type="n",main="Separation / Multicolinearity Check")
points(budget[data$indicator==0],director_facebook_likes[data$indicator==0],pch="0",col="red")
points(budget[data$indicator==1],director_facebook_likes[data$indicator==1],pch="1",col="blue")

# Hosmerlem
hosmerlem = function(y, yhat, g=10) 
{  
  cutyhat = cut(yhat,breaks = quantile(yhat, probs = seq(0,1, 1/g)), include.lowest=TRUE)
  obs = xtabs(cbind(1 - y, y) ~ cutyhat)
  expect = xtabs(cbind(1 - yhat, yhat) ~ cutyhat)
  chisq = sum((obs - expect)^2/expect)  
  P = 1 - pchisq(chisq, g - 2)  
  return(list(chisq=chisq,p.value=P))
}

hosmerlem(y=data$indicator, yhat=fitted(fit.logit))

# R-squared
NagelkerkeR2(fit.logit)

# Concordance
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

OptimisedConc(fit.logit)

# Residuals
r.dev = residuals(fit.logit, type = "deviance")
summary(r.dev)

plot(data$budget,r.dev,xlab="Budget",ylab="Deviance residual", main="Deviance residuals")
loess.dev <- loess(r.dev~data$budget)
lo.pred <- predict(loess.dev, se=T)

orderbudget <- order(data$budget)
lines(data$budget[orderbudget],lo.pred$fit[orderbudget],col="blue",lwd=3)
lines(data$budget[orderbudget],lo.pred$fit[orderbudget]+2*lo.pred$s[orderbudget], lty=2,col="red")
lines(data$budget[orderbudget],lo.pred$fit[orderbudget]-2*lo.pred$s[orderbudget], lty=2,col="red")

# Cook's distance
id = 1:dim(data)[1] 

cook.bw <- cooks.distance(fit.logit)
plot(id,cook.bw,type="l",xlab="Identification",ylab="Cook's distance",col="red")
