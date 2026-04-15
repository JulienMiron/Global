#############################################
## Simulation examples for GLM diagnostics ##
#############################################

#-----------
# Bernoulli
#-----------
n<-1000;
m<-1
x <- runif(n)
lp <- -1 + 3*x
mu <- binomial()$linkinv(lp)
y <- rbinom(1:n,m,mu)
fit.bern <- glm(y~x,family=binomial(link="logit"))
# fit.bern <- glm(y/m~x,family=binomial,weights=rep(m,n))
summary(fit.bern)
#
par(mfrow=c(2,2))
plot(fit.bern)
par(mfrow=c(1,1))
#
plot(residuals(fit.bern, type = "deviance")~x)
abline(h=0, ,lwd =2)
lines(smooth.spline(x, residuals(fit.bern, type = "deviance")), col="red", lwd = 2)
#
library(statmod)
qqnorm(qresid(fit.bern))
abline(a=0,b=1)

#-----------
# Binomial
#-----------
n<-1000;
m<-100
x <- runif(n)
lp <- 3*x-1
mu <- binomial()$linkinv(lp)
y <- rbinom(1:n,m,mu)
fit.bin <- glm(cbind(y,m-y)~x,family=binomial(link="logit"))
#   fit.bin <- glm(y/m~x,family=binomial,weights=rep(m,n))
summary(fit.bin)
#
par(mfrow=c(2,2))
plot(fit.bin)
par(mfrow=c(1,1))
#
plot(residuals(fit.bin, type = "deviance")~x)
abline(h=0, ,lwd =2)
lines(smooth.spline(x, residuals(fit.bin, type = "deviance")), col="red", lwd = 2)
#
library(statmod)
qqnorm(qresid(fit.bin))
abline(a=0,b=1)

#-----------
# Poisson
#-----------
n<-1000;
x <- runif(n)
lp <- -1 + 3*x
mu <- poisson()$linkinv(lp)
y <- rpois(n,lambda=mu)
fit.pois <- glm(y~x,family=poisson)
summary(fit.pois)
#
par(mfrow=c(2,2))
plot(fit.pois)
par(mfrow=c(1,1))
#
plot(residuals(fit.pois, type = "deviance")~x)
abline(h = 0)
lines(smooth.spline(x, residuals(fit.pois, type = "deviance")), col="red", lwd = 3)
#
library(statmod)
qqnorm(qresid(fit.pois))
abline(a=0,b=1)

#-----------
# Gamma (WIP, lack of time and weird parametrization in R)
#-----------
# 
n<-1000;
x <- runif(n)
lp <- 1 + 3*x

mu <- Gamma()$linkinv(lp)
mu <- exp(mu)

y <- rgamma(n,shape = mu)
fit.gamma.inv <- glm(y~x,family=Gamma(link = "inverse"))
fit.gamma.log <- glm(y~x,family=Gamma(link = "log"))
summary(fit.gamma.inv)
summary(fit.gamma.log)
par(mfrow=c(2,2))
plot(fit.gamma.inv)
plot(fit.gamma.log)
par(mfrow=c(1,1))

plot(residuals(fit.gamma.inv, type = "deviance")~x)
abline(h = 0)
lines(smooth.spline(x, residuals(fit.gamma.inv, type = "deviance")), col="red", lwd = 3)

plot(residuals(fit.gamma.log, type = "deviance")~x)
abline(h = 0)
lines(smooth.spline(x, residuals(fit.gamma.log, type = "deviance")), col="red", lwd = 3)

