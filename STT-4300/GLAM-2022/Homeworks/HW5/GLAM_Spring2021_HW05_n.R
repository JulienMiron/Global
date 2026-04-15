### GLAM Spring 2020 HW 05 ####

rm(list=ls()) # clear the environnment of all objects defined previously
setwd("../datasets/")

### Exercise 3 ####

# Import the \verb"docvisits.asc" data file, which is part of the 1977-1978 
# Australian Health Survey and contains information on the number of 
# consultations with a doctor or specialist, denoted \texttt{dvisits}, in the 
# two-weeks period before an interview. These counts represent our response 
# variable and we consider the following thirteen explanatory variables:

# - sex : coded 1 for female,
# - age : in years/100,
# - agesq: (age)^2/1000$,
# - income: annual income/1000,
# - illness: number of illnesses in past two weeks, 
#             with five or more coded as 5,
# - actdays: number of reduced activity days in past two weeks
#             due to illness or injury,
# - hscore: general health questionnaire score, 
#           with high scores indicating bad health,
# - chcond1: coded 1 if chronic condition(s) but not limited in activity,
# - chcond2: coded 1 if chronic condition(s) and limited in activity,
# - levyplus, freepoor and freerepa, three dummy variables 
#              for levels of health insurance, 
#   where ``levyplus'' represents a higher level of insurance cover 
#   while ``freepoor'' and ``freerepa'' are basic levels of 
#           state-provided insurances.

docvisits <- read.table(file = 'docvisits.asc', header=T)
hist(docvisits$dvisits)
barplot(table(docvisits$dvisits)) # a lot of zeros
sum(docvisits$dvisits == 0) / length(docvisits$dvisits)


# -------
# a) Two steps of the Poisson hurdle model
# -------
source("truncpoisson.R")

# We fit a binomial GLM to separate the 0's and 
# the "others" which will be coded as 1's
manual_hurdle <- glm((dvisits > 0) ~ sex + age + agesq + income + levyplus + 
              freepoor + freerepa + illness + actdays + hscore + 
              chcond1 + chcond2, 
            data = docvisits, family = binomial)
summary(manual_hurdle) # correspond to zero Hurdle model


# We fit a poisson GLM to the data > 0

manual_poisson <- glm(dvisits ~ sex + age + agesq + income + levyplus + 
                       freepoor + freerepa + illness + actdays + hscore + 
                       chcond1 + chcond2, 
                     data = docvisits, family = truncpoisson, 
                     subset = (dvisits > 0))
summary(manual_poisson)


#install.packages("pscl")
library(pscl)

# Hurdle Model
fit.hurdle <- hurdle(dvisits ~ sex + age + agesq + income + levyplus +
                       freepoor + freerepa + illness + actdays + hscore +
                       chcond1 + chcond2,
                     data = docvisits,
                     dist = "poisson", zero.dist = "binomial", link = "logit")
summary(fit.hurdle)


# Zero inflation (via hurdle)
round(data.frame(by.hand = coef(manual_hurdle),   
                 with.command = fit.hurdle$coefficients$zero), 4)

# Abundance
round(data.frame(by.hand = coef(manual_poisson), 
                 with.command = fit.hurdle$coefficients$count), 4)


# We see that the coefficients for the 0 part are exactly the same
# The coefficients for the poisson part are a little bit different
#    maybe this is due to the optimization method ?


# -------
# b) Poisson GLM and ZIP Model
# -------

# Poisson Model
poisson.glm <- glm(dvisits ~ sex + age + agesq + income + levyplus + 
                     freepoor + freerepa + illness + actdays + hscore + 
                     chcond1 + chcond2, family = poisson, data = docvisits)
summary(poisson.glm)

# ZIP Model
fit.zip <- zeroinfl(dvisits ~ sex + age + agesq + income + levyplus + 
                      freepoor + freerepa + illness + actdays + hscore + 
                      chcond1 + chcond2,
                    dist = "poisson",
                    link = "logit",
                    data = docvisits)
summary(fit.zip)


# Among the three models, which provides the best fit ?
# AIC 
extractAIC(fit.hurdle)
poisson.glm$aic
extractAIC(fit.zip) # smallest AIC

# The ZIP Model has the smallest AIC, so this is the best fit


# -------
# c) Do high insurance levels make the number of consultations increase?
# -------

# We will look at the coefficients for levyplus in the ZIP model.
# We first look at the coefficient for the Zero-inflated part:
#       Its p-value is 0.02, so it is significant. Since the coefficient is 
#       negative, we say that it decreases the probability of "crossing the 
#       hurdle".
# For the count part, its p-value is 0.726 so it is not significant. Even if
#       it were significant, its coefficient is again negative, so it would 
#       decrease the number of consultations.





