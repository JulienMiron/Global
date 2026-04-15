rm(list = ls())
require(lme4)
require(statmod)
require(gee)
require(mgcv)
require(gamm4)
attach(CO2)

loguptake <- log(uptake)
logconc   <- log(conc - 92)

# Question 1 : 

fit_1 <- glm(uptake ~ conc + Treatment, family = gaussian())
fit_2 <- glm(loguptake ~ logconc + Treatment, family = gaussian())





# Question 2 : 
AIC(fit_1)
AIC(fit_2)

# Compute AIC of second model by hand

# dispersion parameter : 
sd2 <- sigma(fit_2)

nb_parameters <- ...
# solution: nb_parameters <- 3 + 1

log_likelihood <- sum( ... )
# solution: log_likelihood <- sum(dlnorm(loguptake, 
#                                        meanlog = fit_2$fitted.values, 
#                                        sdlog =  sigma(lm_2), 
#                                        log = TRUE)) 

AIC_2 <- 2 * (nb_parameters - log_likelihood)


# Question 3 :
fit_3 <- lm(loguptake ~ Plant + Type + logconc + Treatment)
summary(fit_3)


# Question 4:
fit_4 <- glm(loguptake ~ logconc:Treatment, family = gaussian())
fit_5 <- glm(loguptake ~ logconc + Treatment + Type, family = gaussian())


# Question 5:
plot(qresid(fit_5), residuals(fit_5), 
     xlab = "Randomized residuals", 
     ylab = "Residuals", cex =0.3) 


# Question 6:
plot(NULL, 
     xlim = c(1, 7), 
     ylim = c(2,4), 
     xlab = "Log Concentration", 
     ylab = "Log uptake")

i <- 0
for (p in Plant) {
  i <- i + 1
  points(logconc[Plant == p], loguptake[Plant == p], 
         type= "b", pch = 19, col =  i)
}


# Question 8:
fit_8 <- lmer(loguptake ~ ... + (... | ...))
fit_9 <- lmer(loguptake ~ logconc + Treatment + Type + (1 | Plant))


fit_gee <- gee(loguptake ~ logconc + Treatment + Type, id = Plant, corstr = "exchangeable")
summary(fit_gee)





fit_gamm <- gamm4(loguptake ~ s(logconc, k = 7) + Treatment + Type , random = ~ (1|Plant))
summary(fit_gamm$gam)
summary(fit_gamm$mer)

