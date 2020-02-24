###############
# Fossil Data #
###############
setwd("~/Desktop/Regression Analysis/Project")
fossil = read.table("fossil.txt", header = TRUE)
attach(fossil)

#------------#
# Parametric #
# -----------#
par(mfrow=c(1,1))
plot(age, strontium.ratio)

# linear
fit1 = lm(strontium.ratio ~ age)
abline(fit1, col = "red")

# quadratic
fit2 = lm(strontium.ratio ~ age + I(age^2))
curve(fit2$coefficients[1] + fit2$coefficients[2]*x + fit2$coefficients[3]*x^2,
      90, 125, add = TRUE, col = "green")

# cubic
fit3 = lm(strontium.ratio ~ age + I(age^2) + I(age^3))
curve(fit3$coefficients[1] + fit3$coefficients[2]*x + fit3$coefficients[3]*x^2 + fit3$coefficients[4]*x^3,
      90, 125, add = TRUE, col = "blue")

legend(95, .70732, c("linear", "quadratic", "cubic"), lty = 1, col = c("red", "green", "blue"))

library(MASS)
fit3.res = residuals(fit3)
fit3.stdres = stdres(fit3)
fit3.fittedvalues = fitted.values(fit3)
par(mfrow = c(2,2))
qqnorm(fit3.stdres, main="")
qqline(fit3.stdres)
plot(fit3.res, xlab = "Index", ylab = "Residual")
plot(fit3.fittedvalues, fit3.res, xlab = "Fitted value", ylab = "Residual")
lines(lowess(fit3.res ~ fit3.fittedvalues), col = "red")
plot(fit3.stdres, xlab = "Index", ylab = "Standardized residual", ylim = c(-5,5))
abline(h = -2.5, lty = 2)
abline(h = 2.5, lty = 2)

#---------------#
# Nonparametric #
#---------------#

# Local linear
par(mfrow=c(1,2))
plot(age, strontium.ratio, main = "local linear regression")
s <- c(1/2, 2/3)
colors <- c("red", "blue")
for (i in 1:length(s)) lines(age, predict(loess(strontium.ratio ~ age,
                                                span = s[i], degree = 1), 
                                          data = fossil), col = colors[i])
legend(85,.70728, c("span = 1/2", "span = 2/3"), lty = 1, col = colors, bty = "n")

# local quadratic
plot(age, strontium.ratio, main="Local quadratic regression")
for (i in 1:length(s)) lines(age, predict(loess(strontium.ratio ~ age,
                                                span = s[i], degree = 2), 
                                          data = fossil), col = colors[i])
legend(85,.70728, c("span = 1/2", "span = 2/3"), lty = 1, col = colors, bty = "n")

# loess model
fit.loess = loess(strontium.ratio~ age, span = 2/3, degree =2)
fit.loess

par(mfrow=c(1,1))
plot(age, strontium.ratio)
lines(loess.smooth(age, strontium.ratio, span = 2/3, degree = 2))

# Assumptions
par(mfrow=c(1,3))
scatter.smooth(residuals(mymodel), span = 1, degree = 1, main = "Residuals Vs Index")
abline(h=0,lty =2)
scatter.smooth(fitted(mymodel), sqrt(abs(residuals(mymodel))), span = 1, degree = 1, main = "Residuals Vs Fitted Values")
qqnorm(residuals(mymodel))
qqline(residuals(mymodel))

# Test for linearity
traceS <- fit.loess$trace.hat
SSE0 <- sum(residuals(fit1)^2)
SSE1 <- sum(residuals(fit.loess)^2)
n <- dim(fossil)[1]
Fvalue <- ((SSE0 - SSE1) / (traceS - 2)) / (SSE1 / (n - traceS))
Fvalue
Fcrit <- qf(0.95, traceS - 2, n - traceS)
Fcrit
1 - pf(Fvalue, traceS - 2, n - traceS)

# Comparison with quadratic
traceS <- fit.loess$trace.hat
SSE0 <- sum(residuals(fit2)^2)
SSE1 <- sum(residuals(fit.loess)^2)
n <- dim(fossil)[1]
Fvalue <- ((SSE0 - SSE1) / (traceS - 3)) / (SSE1 / (n - traceS))
Fvalue
Fcrit <- qf(0.95, traceS - 3, n - traceS)
Fcrit
1 - pf(Fvalue, traceS - 3, n - traceS)

# comparison with cubic
traceS <- fit.loess$trace.hat
SSE0 <- sum(residuals(fit3)^2)
SSE1 <- sum(residuals(fit.loess)^2)
n <- dim(fossil)[1]
Fvalue <- ((SSE0 - SSE1) / (traceS - 4)) / (SSE1 / (n - traceS))
Fvalue
Fcrit <- qf(0.95, traceS - 4, n - traceS)
Fcrit
1 - pf(Fvalue, traceS - 4, n - traceS)

detach(fossil)
