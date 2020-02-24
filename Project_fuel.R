
setwd("~/Desktop/Regression Analysis/Project")
fuel = read.table("fuel2001.txt", header = TRUE)
attach(fuel)

#-------------------
# Classical Analysis
#-------------------

n = dim(fuel)[1]
p = dim(fuel)[2]

# Standardized model
fuel.stand <- scale(fuel, center = T, scale = T) / sqrt(n - 1)
fuel.stand <- as.data.frame(fuel.stand)
attach(fuel.stand)

ls.stand = lm(Fuel ~ Income + Miles + Tax + Dlic - 1, data = fuel.stand)
summary(ls.stand)
plot(ls.stand)

par(mfrow=c(1,2))
# Standardized residuals
ls.stdres <- stdres(ls.stand)
plot(ls.stdres, ylim = c(-4,4), ylab = "Standardized residuals")
abline(h = c(-2.5,2.5), col = "red")
identify(c(1:51), ls.stdres, plot=TRUE)

# Studentized residuals
ls.studres <- studres(ls.stand)
plot(ls.studres, ylim = c(-4,4), ylab = "Studentized residuals")
abline(h = c(-2.5,2.5), col = "red")
identify(c(1:51), ls.studres, plot=TRUE)

# Diagonal elements of hat matrix 
par(mfrow= c(1,1))
ls.influence <- influence(ls.stand)
plot(ls.influence$hat, ylab = "Diagonal elements of hat matrix")
abline(h = 2*p/n, col = "red")
identify(c(1:51), ls.influence$hat, plot=TRUE)
# obs 2,7,9 & 44

par(mfrow=c(1,2))
# DFFITS
ls.dffits <- dffits(ls.stand)
plot(abs(ls.dffits), ylab = "|DFFITS|")
abline(h = 2*sqrt(p/n), col = "red")
identify(c(1:51), abs(ls.dffits), plot=TRUE)

# Cook's distance
ls.Cd <- cooks.distance(ls.stand)
plot(ls.Cd, ylab = "Cook's distance", ylim=c(0,1))
abline(h = 1, col = "red")
identify(c(1:51), ls.Cd, plot=TRUE)

# DFBETAS
ls.dfbetas <- dfbetas(ls.stand)
ls.dfbetas = as.data.frame(ls.dfbetas)

par(mfrow=c(2,2))
plot(c(1:51), abs(ls.dfbetas$Income), 
     ylab = "|DFBETAS|", xlab = "Index",ylim = c(0,.4))
abline(h = 2/sqrt(n), col = "red")
identify(c(1:51), abs(ls.dfbetas$Income), plot=TRUE)
plot(c(1:51), abs(ls.dfbetas$Miles), ylab = "|DFBETAS|", xlab = "Index")
abline(h = 2/sqrt(n), col = "red")
identify(c(1:51), abs(ls.dfbetas$Miles), plot=TRUE)
plot(c(1:51), abs(ls.dfbetas$Tax), ylab = "|DFBETAS|", xlab = "Index")
abline(h = 2/sqrt(n), col = "red")
identify(c(1:51), abs(ls.dfbetas$Tax), plot=TRUE)
plot(c(1:51), abs(ls.dfbetas$Dlic), ylab = "|DFBETAS|", xlab = "Index")
abline(h = 2/sqrt(n), col = "red")
identify(c(1:51), abs(ls.dfbetas$Dlic), plot=TRUE)

# ---------------
# Robust Analysis
#----------------
library(robustbase)

RLTS = ltsReg(Fuel ~ Income + Miles + Tax + Dlic -1, alpha = .75, data = fuel.stand)
summary(RLTS)

par(mfrow=c(2,2))
plot(RLTS, which = "rindex")
plot(RLTS, which = "rdiag")
text(2.192477,3.130287,'51')
text(2.144416,-2.713331,'10')
text(2.096355,2.295484,'24')

par(mfrow=c(1,2))
# Standardized residuals
RLTS.stdres = RLTS$residuals/RLTS$scale
plot(RLTS.stdres, ylim = c(-12,12), ylab = "Standardized LTS residuals")
abline(h = c(-2.5,2.5), col = "red")
identify(c(1:51), RLTS.stdres, plot = TRUE)

# Diagnostic plot
plot(RLTS$RD, RLTS.stdres, ylim = c(-10,10), xlim =c(0, 5),
     xlab = "Robust distances", ylab = "Standardized residuals")
abline(v = sqrt(qchisq(0.975, p - 1)), col = "red")
abline(h = c(-2.5,2.5), col = "red")
identify(RLTS$RD, RLTS.stdres, plot = TRUE)

detach(fuel)
