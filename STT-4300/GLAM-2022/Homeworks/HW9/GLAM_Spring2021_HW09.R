

##################### GLAM Spring 2021 HW09 ##################### 


##################### 
# Ex.1
##################### 
# As introduction: tent basis for piecewise linear function of the covariate x.
# The dataset "engine" collects the capacity and the wear of 19 engines.

#--a From the package "gamair", import the data set "engine". After exploratory 
# analysis, propose a model to describe the dependence among the the data.

#rm(list=ls)
require(gamair)
data(engine)

layout(1)
plot(engine$size, engine$wear, xlab = "Engine capacity", ylab ="Wear index")
mod1 <- lm(wear ~size, data  = engine)
summary(mod1)
par(mfrow=c(2,2))
plot(mod1)
par(mfrow=c(1,1))

s <- seq(min(engine$size), max(engine$size), length.out = 200)
plot(engine$size, engine$wear)
lines(s, cbind(rep(1, length(s)), s) %*% coef(mod1), col = "red")

# Square term

engine2 <- cbind(engine, engine$size ^ 2)

mod2 <- lm(wear ~ 1 + 
             size + 
             I(size ^ 2), data  = engine)
summary(mod2)
par(mfrow=c(2,2))
plot(mod2)
par(mfrow=c(1,1))

s <- seq(min(engine$size), max(engine$size), length.out = 200)
plot(engine$size, engine$wear)
lines(s, cbind(rep(1, length(s)), 
               s, 
               s ^ 2) %*% coef(mod2), col = "red", type = "l")

lines(s, cbind(rep(1, length(s)), 
               s) %*% coef(mod1), col = "green", type = "l")

# Cubic term


mod3 <- lm(wear ~ 1 + 
             size + 
             I(size ^ 2) + 
             I(size ^ 3), data  = engine)
summary(mod3)
par(mfrow = c(2, 2))
plot(mod3)
par(mfrow = c(1, 1))

s <- seq(min(engine$size), max(engine$size), length.out = 200)
plot(engine$size, engine$wear)
lines(s, cbind(rep(1, length(s)), 
               s, 
               s ^ 2, 
               s ^ 3) %*% coef(mod3), col = "red", type = "l")
lines(s, cbind(rep(1, length(s)), 
               s) %*% coef(mod1), col = "green", type = "l")


# 4th


mod4 <- lm(wear ~ 1 + 
             size + 
             I(size ^ 2) + 
             I(size ^ 3) + 
             I(size ^ 4), data  = engine)
summary(mod4)
par(mfrow = c(2, 2))
plot(mod4)
par(mfrow = c(1, 1))

s <- seq(min(engine$size), max(engine$size), length.out = 200)
plot(engine$size, engine$wear)
lines(s, cbind(rep(1, length(s)), 
               s, 
               s ^ 2, 
               s ^ 3,
               s ^ 4) %*% coef(mod4), col = "red", type = "l")
lines(s, cbind(rep(1, length(s)), 
               s, 
               s ^ 2, 
               s ^ 3) %*% coef(mod3), col = "green", type = "l")

# 5th


mod5 <- lm(wear ~ 1 + 
             size + 
             I(size ^ 2) + 
             I(size ^ 3) + 
             I(size ^ 4) + 
             I(size ^ 5), data  = engine)
summary(mod5)
par(mfrow = c(2, 2))
plot(mod5)
par(mfrow = c(1, 1))

s <- seq(min(engine$size), max(engine$size), length.out = 200)
plot(engine$size, engine$wear)
lines(s, cbind(rep(1, length(s)), 
               s, 
               s ^ 2, 
               s ^ 3,
               s ^ 4,
               s ^ 5) %*% coef(mod5), col = "red", type = "l")


# 6th


mod6 <- lm(wear ~ 1 + 
             size + 
             I(size ^ 2) + 
             I(size ^ 3) + 
             I(size ^ 4) + 
             I(size ^ 5) + 
             I(size ^ 6), data  = engine)
summary(mod6)
par(mfrow = c(2, 2))
plot(mod6)
par(mfrow = c(1, 1))

s <- seq(min(engine$size), max(engine$size), length.out = 200)
plot(engine$size, engine$wear)
lines(s, cbind(rep(1, length(s)), 
               s, 
               s ^ 2, 
               s ^ 3,
               s ^ 4,
               s ^ 5,
               s ^ 6) %*% coef(mod6), col = "green", type = "l")

# 7th


mod7 <- lm(wear ~ 1 + 
             size + 
             I(size ^ 2) + 
             I(size ^ 3) + 
             I(size ^ 4) + 
             I(size ^ 5) + 
             I(size ^ 6) + 
             I(size ^ 7), data  = engine)
summary(mod7)
par(mfrow = c(2, 2))
plot(mod7)
par(mfrow = c(1, 1))

s <- seq(min(engine$size), max(engine$size), length.out = 200)
plot(engine$size, engine$wear)
lines(s, cbind(rep(1, length(s)), 
               s, 
               s ^ 2, 
               s ^ 3,
               s ^ 4,
               s ^ 5,
               s ^ 6,
               s ^ 7) %*% coef(mod7), col = "red", type = "l")




# structure in residuals


#--b As the linear model fails to capture the complex structure of the data, 
# we go for a non-parametric approach. 
# We assume that the model's function f(xi) is now piecewise linear.

setwd(...) #Your directory
source("HW09Functions.R")

k <- 5;
x <- seq(0, 10, 0.01)
sj <- seq(min(x), max(x), length.out = k)
X <- tf.X(x, sj)
plot(x = x, X[, 1], type = "l")
for(i in 2:k){
  lines(x = x, X[, i])
}

#--b1 Using the "tent" functions previously defined, build your "design matrix" 
# based on a grid of evenly spaced knots:

k <- 4
sj <- seq(min(engine$size), max(engine$size), length.out = k)
X <- tf.X(engine$size, sj)

#--b2 Estimate the coefficients in the linear combination of the basis. 
# As you only observe at yi and f(xi), you are going to minimize the residuals 
# sum of square sum(yi-f(xi))^2. You can simply use the lm() function without 
# the intercept.

b <- lm(engine$wear ~ X - 1)
summary(b)
# par(mfrow=c(2, 2))
# plot(b)
# par(mfrow=c(1, 1))

#--b3 Try different number of knots, and plot the prediction. Explain the role 
# of this parameter, and the limitations of this basis.

s <- seq(min(engine$size),max(engine$size),length.out = 200)
Xp <- tf.X(s,sj)
# plot(engine$size,engine$wear, ylim = c(0, 5))
lines(s,Xp%*%coef(b), col = "red")
# lines(s,cbind(rep(1,length(s)),s,s^2,s^3,s^4,s^5,s^6)%*%coef(mod1), col = "red")

##################### 
# Ex. 2
##################### 

# We usually want f() to be smooth, and the piecewise linear model might be to 
# gross. Thus, a polynomial basis would be more suitable than the "tent" basis. 
# For computational stability, we would rather choose orthogonal bases functions.


#--a  Simulate data using the following code:
set.seed(1)
x <- sort(runif(40)*10)^0.5
y <- sort(runif(40))^0.1
data1 <- data.frame(y,x)

#--b
plot(x,y)

#--c Fit two parametric polynomial model of degree 5 and 10. First, 
# solve manually the OLS estimating equation.

form5 <- paste("I(x^", 1:5, ")", sep = " ", collapse = "+")
form5 <- as.formula(paste("y~", form5))
X <- model.matrix(object = form5, data = data1)
beta <- solve(t(X) %*% X, t(X) %*% y)
# max(svd(t(X)%*%X)$d)/min(svd(t(X)%*%X)$d) # condition number
model5 <- lm(formula = form5)
summary(model5)
data.frame(beta,model5$coef)

form10 <- paste("I(x^",1:10,")",sep = "", collapse = "+")
form10 <- as.formula(paste("y~",form10))
X <- model.matrix(object = form10, data = data1)
beta <- solve(t(X) %*% X, t(X) %*% y) # computationally singular design matrix 
ill.model10 <- lm(formula = form10) # no numerical problems.
summary(ill.model10)

#--d Use the function poly(), in order to define the (orthogonal) polynomial 
# basis, and the corresponding model matrix. Then, repeat point b) and explain 
# the change.

poly5 <- lm(y~poly(x,5)) 
summary(poly5)


X <- cbind(1,poly(x,5))
beta <- solve(t(X)%*%X,t(X)%*%y) # no computational singularity
poly5 <- lm(y~poly(x,5))
summary(poly5)
data.frame(beta,poly5$coef)



X <- cbind(1,poly(x,10))
beta <- solve(t(X)%*%X,t(X)%*%y) # no computational singularity
poly10 <- lm(y~poly(x,10))
summary(poly10)
data.frame(beta,poly10$coef)

#--e Plot your predictions on a fine grid (using predict()) along with the data, 
# what do you observe?
x.grid <- seq(min(x), max(x), by = 1e-3)
neu <- data.frame(x = x.grid)
plot(x, y, lwd = 2, ylim = c(0.65, 1.15))
lines(x.grid, predict(poly5, newdata = neu, 
                     type = "response"), type ="l", col = "magenta", lwd = 2)
lines(x.grid,predict(poly10,newdata = neu,
                     type = "response"), type ="l", col = "red", lwd = 2)
lines(x.grid,predict(model5,newdata = neu,
                     type = "response"), type ="l", col = "green", lwd = 2)
lines(x.grid,predict(ill.model10,newdata = neu,
                     type = "response"), type ="l", col = "blue", lwd = 2)
legend(x = 2, y = 0.85, legend = c("poly10","poly5"), 
       col = c("red","magenta"), lwd = 2)

# order of the polynomial and wiggliness, 
# equivalence between orthogonal and non-orthogonal definition.


#--f Use evenly spaced knots and a cubic spline basis
#    to fit a cubic regression spline.

basis.dim <- 11
k <- basis.dim - 2
Knots <- seq(min(x), max(x), length.out = k)

X <- cub_X(x, Knots)
cub_spline <- lm(y ~ X - 1)
summary(cub_spline)
#--g Overlay the cubic regression spline on your previous graph. 
# Comment the fit of the different approaches.
plot(x, y, lwd = 2, ylim = c(0.65, 1.15))
lines(x.grid, predict(poly5, newdata = neu, type = "response"), 
      type ="l", col = "magenta", lwd = 2)
lines(x.grid,predict(poly10,newdata = neu,type = "response"), 
      type = "l", col = "red", lwd = 2)
Xp <- cub_X(x.grid,Knots)
lines(x.grid, Xp%*%coef(cub_spline), col = "blue", lwd = 2)
legend(x = 2, y = 0.9, 
       legend = c("poly10", "poly5", paste("cub", basis.dim, sep = "")), 
       col = c("red", "magenta", "blue"), lwd = 2)


#==========================================================================================================================
# Ex. 3 GAM
#==========================================================================================================================
  
# The dataset "temperature.txt" records the temperature in January at different 
# localization, identified by their lattitude and longitude. 
# Fit a model to explain the temperature with longitude and lattitude. 
# Analyse the data with the gam() function.

setwd("H:/TEACH/GLAM/test_repos/HW9")
temp <- read.table(file = 'temperature.txt', header = T)
par(mfrow=c(1, 1))
plot(temp[-1])
hist(temp$JanTemp) # slightly skewed, very unlikely non-negative values
require(mgcv)

# default model "thin plate" regression splines
mod1 <- gam(JanTemp ~ s(Lat) + s(Long), data = temp)
par(mfrow = c(1, 2))
plot(mod1, residuals = TRUE, pch = 1)
par(mfrow = c(2, 2))
gam.check(mod1)
summary(mod1)
abline(0, 1)
mod1$sp

mod2 <- gam(JanTemp ~ s(Lat, k = 6) + s(Long, k = 10), data = temp)
par(mfrow = c(1, 2))
plot(mod2, residuals = TRUE, pch = 1)
par(mfrow = c(2, 2))
gam.check(mod2)
summary(mod2)
abline(0, 1)
mod2$sp

# can try with different families, but the gaussian seems suitable here

mod3 <- gam(JanTemp ~ s(Lat, sp = 0.1) + s(Long, sp = 0.11), data = temp)
par(mfrow=c(2, 2))
plot(mod3, residuals = TRUE, pch = 1)
plot(mod2, residuals = TRUE, pch = 1)
par(mfrow=c(2, 2))
gam.check(mod3)
summary(mod3)

# less degrees of freedom, but explains less deviance.
mod4 <- gam(JanTemp ~ s(Lat, sp = 0.001) + s(Long, sp = 0.001), data = temp)
par(mfrow = c(2, 2))
plot(mod4, residuals = TRUE, pch = 1)
plot(mod1, residuals = TRUE, pch = 1)
par(mfrow = c(2, 2))
gam.check(mod4)
summary(mod4)




mod5 <- gam(JanTemp ~ s(Lat, bs = "cr") + s(Long, bs = "cr"), data = temp)
par(mfrow = c(2, 2))
plot(mod5, residuals = TRUE, pch = 1)
plot(mod1, residuals = TRUE, pch = 1)
gam.check(mod5)
summary(mod5)
summary(mod1)

# slightly better explained deviance but the fit does not change significantly.


