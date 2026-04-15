#### GLAM Spring 2021 HW 08 ####

rm(list=ls()) # clear the environnment of all objects defined previously
# setwd("../datasets/")
   
#### Exercise 1 ####

# In the U.S.A., the food stamp program is a federal government program 
# that was created in order to help low-income people buy food. 
# The U.S. food stamp data were collected in order to study the relation 
# between participation to the food stamp program and various socioeconomic 
# indicators. This data set is well-known in the literature concerning robust 
# estimation and testing in generalized linear models


# ---
# Outcome  
# ---

# Participation : dummy indicating the participation to the food stamp program

# ---
# Covariates  
# ---

# Tenancy       : a dummy indicating home ownership
# Suppl.Income  : indicating whether some form of supplemental income is received
# Income        : in U.S. dollars

# N = 150

# There are 24 cases of participation out of the 150 observations in the sample.

#### a. Robust estimation with the glmrob() function #####

library(robustbase)

food <- read.table(file='foodstamp.dat',header=T)
str(food)

food$lincome <- log(food$Income)
food$lincome <- log(food$Income+1)

par(mfrow=c(1,2))
hist(food$Income,col='deepskyblue1',nclass=20,prob=T)
lines(density(food$Income),col='red',lwd=2)
hist(food$lincome,col='deepskyblue3',nclass=20,prob=T)
lines(density(food$lincome),col='red',lwd=2)
par(mfrow=c(1,1))

min(food$Income) # explains lonely point in log(Income+1)

pairs(food)

par(mfrow=c(1,3))
plot(as.factor(Participation)~as.factor(Tenancy), data=food)
plot(as.factor(Participation)~as.factor(Suppl.Income), data=food)
plot(as.factor(Participation)~lincome, data=food)
par(mfrow=c(1,1))

food.robglm.155 <- glmrob(Participation~Tenancy+Suppl.Income+lincome,
                          data=food,
                          family=binomial,
                          weights.on.x='robCov', 
                          acc = 1e-4, test.acc = "coef", maxit = 50, tcc = 1.55,
                          trace.lev = T) # tcc, robustness and efficiency
summary(food.robglm.155)
                                      
# Alternatively, weights on X can be based on the "Hat" matrix. (Then related to the leverage).
# food.robglm.155 <- glmrob(Participation~Tenancy+Suppl.Income+lincome,
#                           data=food,
#                           family=binomial,
#                           weights.on.x='hat',
#                           tcc=1.55)


# Similar to the output of other GLM
# Main difference : information on the robustness weights. 

summary(residuals(food.robglm.155, type = "deviance"))  
# deviance standardized residuals
summary(residuals(food.robglm.155, type = "pearson"))   
# Pearson residuals 
# => Useful for Robustness diagnostics

plot(food.robglm.155$resid) # one very large outlier
which.min(food.robglm.155$resid) # obs. 5
food[5,] 
# it is the subject that has 0 income and doesn't participate in the food stamps program

## Robustness Weights ####

colvec<- rep(1,dim(food)[[1]])
colvec[5] <- 2 # give color red to obs. 5
plot(food.robglm.155$w.r,
     main='Robustness weights',
     col=colvec,pch=19) 
# = > obs. 5 has weight (on resid) very close to 0
food.robglm.155$w.r[5] 
# Identified as outlier => Downplayed influence.

summary(food.robglm.155$w.x)
plot(food.robglm.155$w.x,col=colvec,pch=19, main="weights on x_i")
# obs.5 has weight (in X space) very close to 0 too, probably due to 0 Income and 0 Suppl.Income

summary(food.robglm.155)

# plot(food.robglm.155) # Not available 

# Manually 

#  Standardized Residuals vs. index
par(mfrow =c(1,3))
plot(residuals(food.robglm.155, type = "pearson"), 
     main = "Residuals vs Index") 
abline(h=c(-1.96, 1.96) , col = "blue", lty = 2) 

# Standardized Residuals vs. fitted
plot(x = fitted(food.robglm.155), y = residuals(food.robglm.155, type = "pearson"), main= "Residuals vs fitted") 
abline(h=c(-1.96, 1.96) , col = "blue", lty = 2) 

# Standardized Residuals vs. fitted
which(fitted(food.robglm.155)==max(fitted(food.robglm.155)))
qqnorm(residuals(food.robglm.155)) ; abline(a=0,b=1) 
par(mfrow =c(1,1))

# not great fit nevertheless, probably partly due to covariates.
plot(fitted(food.robglm.155),y = food$Participation)
 
#### b. Robust fit with $c=40$ and comparison with classical GLM. ####

food.robglm.40 <- glmrob(Participation~Tenancy+Suppl.Income+lincome,
                         data=food,
                         family=binomial,
                         weights.on.x='none',
                         tcc=40)

food.glm <- glm(Participation~Tenancy+Suppl.Income+lincome,data=food,family=binomial)

data.frame(
  tcc.5 = summary(food.robglm.5)$coef[,c(1,2)],
  tcc.40 = summary(food.robglm.40)$coef[,c(1,2)],
  glm = summary(food.glm)$coef[,c(1,2)]  # the same  
)


food.robglm.5 <- glmrob(Participation~Tenancy+Suppl.Income+lincome,
                         data=food,
                         family=binomial,
                         weights.on.x='none',
                         tcc=5)

#### c. Comparison of the significance of variables Supp Income and lincome ####

data.frame(
  tcc.155 = summary(food.robglm.155)$coef[,c(1,2)], 
  glm = summary(food.glm)$coef[,c(1,2)]
)
# Tenancy and Supp.Income are on the same order
# lincome and intercept change a lot (estimate and std err)


summary(food.robglm.155)$coef   # lincome is significant, but not Suppl.Income
summary(food.glm)$coef # For the GLM model, they are both not significant


#### d. Estimation of the final model ####

#  model without Suppl.Income, to compare with the full model
food.robglm.155.2 <- glmrob(Participation~Tenancy+lincome,data=food,
                            family=binomial,weights.on.x='robCov',tcc=1.55)

summary(food.robglm.155.2)$coef

plot(food.robglm.155.2$resid)
plot(food.robglm.155.2$resid[-5])

anova(food.robglm.155.2,food.robglm.155,test="QD")
# we cannot reject the hypothesis that quasideviances are equal 
# => beta of suppl.income = 0

