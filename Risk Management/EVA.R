######################
# Assignment II: EVT #
######################
library(ReIns)

setwd("~/Desktop/Statistical Tools for Quantitative Risk Management/Assignment")
load("A2data.RData")

#################
# A
#################

# Exponential QQ & mean excess plots
#par(mfrow=c(2,2))
#ExpQQ(A2data$Building, main = "Exp QQ Building")
#MeanExcess(A2data$Building, main = "Mean Excess Building")
#ExpQQ(A2data$Content, main = "Exp QQ Content")
#MeanExcess(A2data$Content, main = "Mean Excess Content")

# Pareto QQ
par(mfrow=c(1,2))
ParetoQQ(A2data$Building, main = "Pareto QQ Building")
ParetoQQ(A2data$Content, main = "Pareto QQ Content")

#----------------
# Slope estimates
#----------------
par(mfrow=c(1,2))
# Hill
H_Building = Hill(A2data$Building, plot = T, add = F, main = "EVI estimates Building", ylim = c(0,2))
# 'optimal' k
kopt_B = Hill.kopt(A2data$Building)
kopt_B$kopt
abline(v=kopt_B$kopt, col = 'red', lty=2)
# Bias reduced Hill
Hbr_B = Hill.2oQV(A2data$Building, plot=F, add = T, lty=3, col="blue", warnings = T)

legend(locator(1), legend = c('Hill','k_opt','Bias Reduced Hill')
       , col = c('black','red','blue')
       , lty =c(1,2,3), cex = 0.75, bty="n")

# gamma estimates for kopt
H_Building$gamma[kopt_B$kopt]
Hbr_B$gamma[kopt_B$kopt]

# Once more for content...
H_Content = Hill(A2data$Content, plot = T, add = F, main = "EVI Estimates Content", ylim = c(0,3))
kopt_C = Hill.kopt(A2data$Content)
kopt_C$kopt
abline(v=kopt_C$kopt, lty=2, col="red")
Hbr_C = Hill.2oQV(A2data$Content, plot=F, add = T, col="blue", warnings = T, lty=3)

# legend
legend(locator(1), legend = c('Hill','k_opt','Bias Reduced Hill')
       , col = c('black','red','blue')
       , lty =c(1,2,3), cex = 0.75, bty="n")

# gamma estimates for kopt
H_Content$gamma[kopt_C$kopt]
Hbr_C$gamma[kopt_C$kopt]

#-------------------
# Quantile estimates
#-------------------
par(mfrow=c(2,2))
# 1/1500 Building
# Weissman
W_q = Weissman.q(A2data$Building, p=1/1500, gamma = H_Building$gamma
                 , plot=T, ylim=c(700000, 40000000), main = "Building: Q(1 - 1/1500)")
# non-parametric
abline(h=sort(A2data$Building)[ceiling((1-1/1500)*length(A2data$Building))], lty=2) 
# Bias reduced Weissman
W_br = Weissman.q.2oQV(A2data$Building, p=1/1500, gamma = Hbr_B$gamma,b=Hbr_B$b, beta=Hbr_B$beta, add=T, col="blue")
# kopt
abline(v=kopt_B$kopt, lty=2, col="red")

legend(locator(1), legend = c('Weissman','Non-parametric','Weissman BR')
       , col = c('black','black','red'), lty = c(1,2,1))

# quantiles for optimal k
W_q$Q[kopt_B$kopt]
W_br$Q[kopt_B$kopt]

# 1/2000 Building
W_q = Weissman.q(A2data$Building, p=1/2000, gamma = H_Building$gamma
                 , plot=T, ylim=c(700000, 40000000), main = "Building: Q(1 - 1/2000)")
abline(h=sort(A2data$Building)[ceiling((1-1/2000)*length(A2data$Building))], lty='dashed') 
W_br = Weissman.q.2oQV(A2data$Building, p=1/2000, gamma = Hbr_B$gamma,b=Hbr_B$b, beta=Hbr_B$beta, add=T, col="blue") 
abline(v=kopt_B$kopt, col = 'red', lty=2)
legend(locator(1), legend = c('Weissman','Non-parametric','Weissman BR')
       , col = c('black','black','red'), lty = c(1,2,1))

W_q$Q[kopt_B$kopt]
W_br$Q[kopt_B$kopt]

# 1/1500 Content
W_q = Weissman.q(A2data$Content, p=1/1500, gamma = H_Content$gamma
                 , plot=T, ylim=c(9000000,300000000),main = "Content: Q(1 - 1/1500)")
abline(h=sort(A2data$Content)[ceiling((1-1/1500)*length(A2data$Content))])
W_br = Weissman.q.2oQV(A2data$Content, p=1/1500, gamma = Hbr_C$gamma,
                b=Hbr_C$b, beta=Hbr_C$beta, add=T, col="blue")
abline(v=kopt_C$kopt, col = 'red', lty=2)
legend(locator(1), legend = c('Weissman','Non-parametric','Weissman BR')
       , col = c('black','black','red'), lty = c(1,2,1))

W_q$Q[kopt_C$kopt]
W_br$Q[kopt_C$kopt]

# 1/2000 Content
W_q = Weissman.q(A2data$Content, p=1/2000, gamma = H_Content$gamma
                 , plot=T, ylim=c(9000000,300000000), main = "Content: Q(1 - 1/2000)")
abline(h=sort(A2data$Content)[ceiling((1-1/2000)*length(A2data$Content))]) 
W_br = Weissman.q.2oQV(A2data$Content, p=1/2000, gamma = Hbr_C$gamma,
                b=Hbr_C$b, beta=Hbr_C$beta, add=T, col="blue")
abline(v=kopt_C$kopt, col = 'red', lty=2)
legend(locator(1), legend = c('Weissman','Non-parametric','Weissman BR')
       , col = c('black','black','red'), lty = c(1,2,1))

W_q$Q[kopt_C$kopt]
W_br$Q[kopt_C$kopt]

#################
# B
#################
par(mfrow=c(1,2))
# mean excess plot building
MeanExcess(A2data$Building)
# cut-off based on k-opt (section A)
abline(v=sort(A2data$Building)[(1502-515)], col='red', lty=2)
# heuristic cut-off
abline(v=sort(A2data$Building)[(1502-20)], lty=2)

# mean excess plot content
MeanExcess(A2data$Content)
# cut-off based on optimal k (section A)
abline(v=sort(A2data$Content)[(1502-55)], col="red", lty=2)
# heuristic cut-off
abline(v=sort(A2data$Content)[(1502-25)], col="black", lty=2)

# models for Building
splicefit_B = SpliceFitPareto(A2data$Building
                              ,tsplice=c(sort(A2data$Building)[(1502-515)],sort(A2data$Building)[(1502-20)])
                              ,criterium = c("BIC","AIC"))
splicefit_B2 = SpliceFitPareto(A2data$Building, tsplice=sort(A2data$Building)[(1502-515)]
                               ,criterium = c("BIC","AIC"))
splicefit_B3 = SpliceFitPareto(A2data$Building, tsplice=sort(A2data$Building)[(1502-20)]
                               ,criterium = c("BIC","AIC"))

# Information Criteria
splicefit_B$IC
splicefit_B2$IC
splicefit_B3$IC

splicefit_B2

#Models for Content
splicefit_C = SpliceFitPareto(A2data$Content
                              ,tsplice=c(sort(A2data$Content)[(1502-55)], sort(A2data$Content)[(1502-25)])
                              ,criterium = c("BIC","AIC"))
splicefit_C2 = SpliceFitPareto(A2data$Content, tsplice=sort(A2data$Content)[(1502-55)]
                               ,criterium = c("BIC","AIC"))
splicefit_C3 = SpliceFitPareto(A2data$Content, tsplice=sort(A2data$Content)[(1502-25)]
                               ,criterium = c("BIC","AIC"))

#information criteria
splicefit_C$IC
splicefit_C2$IC
splicefit_C3$IC

splicefit_C2

# Fitted survival function and empirical survival function 
#par(mfrow=c(1,1))
#x_B = seq(range(A2data$Building)[1], range(A2data$Building)[2], 100000)
#x_C = seq(range(A2data$Content)[1], range(A2data$Content)[2], 100000)
#SpliceECDF(x_B, A2data$Building, splicefit_B)
#SpliceECDF(x_C, A2data$Content, splicefit_C)

par(mfrow=c(2,2))
# (log) PP-plots of empirical survival function and fitted survival function
SplicePP(A2data$Building, splicefit_B2, main="PP-plot Building ")
SplicePP(A2data$Building, splicefit_B2, log=T, main="log PP-plot Building")
SplicePP(A2data$Content, splicefit_C2, main="PP-plot Content")
SplicePP(A2data$Content, splicefit_C2, log=T, main="log PP-plot Content")

#------
# C
#------
library(copula)
library(MASS)
library(fGarch)
library(fCopulae)

# rank
n = length(A2data$Building)
data = cbind(rank(A2data$Building)/(n+1), rank(A2data$Content)/(n+1))

# omega estimate by means of spearman's rho
omega_t = function(X,Y){
  rho_S = 6/pi*asin(cor(X,Y)/2)
  return(rho_S)
}
omega = omega_t(A2data$Building, A2data$Content)

# t copula for df = {1,10,25}
cop_t_1 = tCopula(omega, dim = 2, df = 1)
#cop_t_2 = tCopula(omega, dim = 2, df = 10)
#cop_t_3 = tCopula(omega, dim = 2, df = 25)

# fitcopula objects for copulas derived above
ft_1 = fitCopula(cop_t_1, data, optim.method="L-BFGS-B", method="ml",
                start=c(omega,5),lower=c(0,2.5),upper=c(.5,15))
#ft_2 = fitCopula(cop_t_2, data, optim.method="L-BFGS-B", method="ml",
 #              start=c(omega,5),lower=c(0,2.5),upper=c(.5,15))
#ft_3 = fitCopula(cop_t_3, data, optim.method="L-BFGS-B", method="ml",
  #             start=c(omega,5),lower=c(0,2.5),upper=c(.5,15))

# Gumbel copula
fgumbel = fitCopula(data=data,optim.method="BFGS",method="mpl",
                    copula=gumbelCopula(1.175,dim=2))

# evaluation points
# u = data[,1:2]
u1 = data[,1]
u2 = data[,2]
dem = pempiricalCopula(u1,u2)

#dem = C.n(u, pobs(A2data[,2:3]))

#Contour plots
par(mfrow=c(1,3))
contour(dem$x,dem$y,dem$z,main="Empirical")
contour(tCopula(param=ft_1@estimate[1],df=round(ft_1@estimate[2])),
        pCopula,main="t")
contour(gumbelCopula(fgumbel@estimate,dim=2),pCopula,
        main="Gumbel")

# rho for Building and content
rho = ft_1@copula@parameters[1]

# lambda for different df
lambda_t1 = 2*dt(-(sqrt(((1+1)*(1-rho))/1+rho)), df=2)
#lambda_t10 = 2*dt(-(sqrt(((10+1)*(1-rho))/1+rho)), df=11)
#lambda_t25 = 2*dt(-(sqrt(((25+1)*(1-rho))/1+rho)), df=26)

# theta Gumbel
theta = fgumbel@copula@parameters
# lamba_u Gumbel
lambda_G = 2-2^(1/theta)

# AIC values for both copulas
# as to determine the best fit (Gumbel)
AIC(ft_1)
AIC(fgumbel)

#------
# D
#------
library(zoo)
library(ggplot2)
library(gridExtra)
library(dplyr)
library(ggridges)
library(viridis)
library(hrbrthemes)

# Extract seasons, years and months
A2data$year = format(base::as.Date(A2data$Date, format="%Y/%m/%d"),"%Y")
A2data$month = format(base::as.Date(A2data$Date, format="%Y/%m/%d"),"%m")
yq = as.yearqtr(as.yearmon(A2data$Date, "%Y/%m/%d") + 1/12)
A2data$Season = factor(format(yq, "%q"), levels = 1:4
                       , labels = c("Winter", "Spring", "Summer", "Fall"))

# log data
A2data$log_B = log(A2data$Building)
A2data$log_C = log(A2data$Content)

# log losses over time, yearly indication
p1= ggplot(A2data, aes(x=Date, y=log_B, group=year,color=year)) +
  geom_line() +
  labs(y="log(Building)")
p2 =ggplot(A2data, aes(x=Date, y=log_C, group=year,color=year)) +
  geom_line() +
  labs(y="log(Content)")
grid.arrange(p1, p2, nrow = 1)

# monthly losses
ggplot(A2data, aes(x = month, y = log_B, color=log_C, size=log_C)) + 
    geom_point(alpha=0.7) + 
    scale_color_continuous(type = "viridis") +
    ggtitle("Monthly losses") +
    labs(y="Building")

# seasonal distributions
p1 = ggplot(A2data, aes(x = `log_B`, y = `Season`, fill = ..x..)) +
  geom_density_ridges_gradient(scale = 2) +
  scale_fill_viridis(name = "log_B", option = "B") +
  labs(title = 'Building distrbution per season') +
  theme_ipsum() +
  theme(
    legend.position="none",
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 8)
  )

p2 = ggplot(A2data, aes(x = `log_C`, y = `Season`, fill = ..x..)) +
  geom_density_ridges_gradient(scale = 2) +
  scale_fill_viridis(name = "log_C", option = "B") +
  labs(title = 'Content distrbution per season') +
  theme_ipsum() +
  theme(
    legend.position="none",
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 8)
  )
grid.arrange(p1,p2, nrow = 2)




