##############################################################
#
# Exercise 1 - June 2022 Exam
#
# Eva Cantoni
#
##############################################################

rm(list = ls())
require(lme4)
require(statmod)

data(CO2)

fit_1 <- glm(uptake ~ conc + Treatment + Type, family = gaussian(),data=CO2)
fit_1
logLik(fit_1)
sigma(fit_1)

fit_2 <- glm(log(uptake) ~ log(conc-92) + Treatment + Type, family = gaussian(),data=CO2)
fit_2
logLik(fit_2)
sigma(fit_2)

fit_3 <- glm(log(uptake) ~ log(conc-92) + Treatment + Type + Plant , family = gaussian(),data=CO2)
summary(fit_3)

fit_glmm <- lmer(log(uptake) ~ log(conc-92) + Treatment + Type + (1 | Plant),data=CO2)
summary(fit_glmm)
ranef(fit_glmm)

fit_2a <- glm(uptake ~ log(conc-92) + Treatment, family = gaussian(link="log"),data=CO2)
