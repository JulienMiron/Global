# load libraries. Be sure that there are installed.
# If not, install them with install.packages()
library(statmod)
library(visreg)

#####
#### EXERCISE 1 
#####

# Load data (don't forget "header" and "sep" parameters)
baby_bern  <- read.table(file = 'babyfood-bernoulli.txt', header = T, sep = '')
baby_binom <- read.table(file = 'babyfood-binomial.txt' , header = T, sep = '')

# Fit a GLM for both datasets
fit_bern  <- glm(disease ~ sex + food, 
                 data = baby_bern, 
                 family = binomial(link = "logit"))

fit_binom <- glm(cbind(disease, nondisease) ~ sex + food, 
                 data = baby_binom,
                 family = binomial(link = "logit"))

# Check if the estimates are the same
fit_bern$coefficients
fit_binom$coefficients

# Check if standard errors of the estimates are the same
summary(fit_bern)$coefficients
summary(fit_binom)$coefficients

# The deviances are different
summary(fit_bern)$deviance
summary(fit_binom)$deviance
# (or alternatively)
deviance(fit_bern)
deviance(fit_binom)

# Akaike Information Criterions are different
summary(fit_bern)$aic
summary(fit_binom)$aic
# (or if you want to compute it "by hand" to review the formula)
-2 * (logLik(fit_bern)  - 4) 
-2 * (logLik(fit_binom) - 4)

# Default diagnosis plots 
par(mfrow = c(2, 2)) # allows to have 4 plots on the plot panel.
plot(fit_bern , main = "Bernoulli")
plot(fit_binom, main = "Binomial" )

# Back to 1 plot on the plot panel
layout(1)

# quantile residuals plots
layout(t(1:2)) # allows to have 2 plots on the plot panel
resid_bern <- qresid(fit_bern)
qqnorm(resid_bern, main = "Bernouilli quantile residuals QQ-plot")
qqline(resid_bern, col = "red")

resid_binom <- qresid(fit_binom)
qqnorm(resid_binom, main = "Binomial quantile residuals QQ-plot")
qqline(resid_binom, col = "red")

# Back to 1 plot on the plot panel
layout(1)

#####
#### Exercise 2
#####

# The dataset ("CpsWages.txt") consists of a random sample of 534 people 
# from the Current Population Survey, with information on wages 
# (wage, in dollars per hour) and other characteristics of the workers, 
# including
# - sex coded 1=female and 0=male,
# - age in years,
# - race coded 1=other, 2=hispanic and 3=white,
# - marr: marital status coded 1=married and 0=unmarried,
# - education: number of years of education,
# - experience: number of years of work experience,
# - occupation: occupational status coded 1=management, 2=sales, 3=clerical, 4=service, 5=professional and 6=other,
# - sector: work sector coded 0=other, 1=manufacturing and 2=construction,
# - south: region of residence coded 1=lives in the South and 0=lives in the North,
# - union: union membership coded 1=union member and 0=not a member.

# Two questions :
#  - Are wages related to these characteristics?
#  - Is there a gender gap in wages? => Is factor "sex" important/significant?

# Load data (again "header" and "sep"...)
wages <- read.table(file = 'CpsWages.txt', header = T, sep = "")
head(wages)
str(wages)

# Some numerical variables (occupation, sector, etc.) 
# are actually categorical variables. Let's change them into factors
wages$south       <- factor(wages$south,
                           levels = 0:1, 
                           labels = c("north", "south"))
wages$race       <- factor(wages$race,
                           levels = 1:3, 
                           labels = c("other", "hisp", "white"))
wages$occupation <- factor(wages$occupation,
                           levels = 1:6,
                           labels = c("mgmt", "sale", "cler", "serv", "prof", "othr"))
wages$sector     <- factor(wages$sector,
                           levels = 0:2,
                           labels = c("other", "manuf", "constr"))
wages$marr       <- factor(wages$marr,
                           levels = 0:1,
                           labels = c("unmarried", "married"))
wages$sex        <- factor(wages$sex,
                           levels = 0:1,
                           labels = c("male", "female"))

# Quick check of the wage distribution
hist(wages$wage, 
     xlab = "Wage ($/hr)", 
     main = "Histogram of wages")
# QQ-plot with normal. Often a good thing to check
qqnorm(wages$wage)
qqline(wages$wage)

# In logarithmic scale
hist(log(wages$wage), 
     xlab = "Log of wage", 
     main = "Histogram of log(wage)")
# Noraml QQ-plot
qqnorm(log(wages$wage))
qqline(log(wages$wage))

# Why it's a bad idea to use education, experience and age
plot(wages$education + wages$experience, wages$age, 
     xlab = "Education + experience", 
     ylab = "Age")

# A bit of exploratory data analysis
# There's correlation between sex and wage
boxplot(wage ~ sex, data = wages)

# It could be a confounder
# For instance, wage depends on occupation...
boxplot(wage ~ occupation, data = wages)

# ... and occupation depends on sex.
mosaicplot(occupation ~ sex, data = wages)

# We'll need a propoer analysis to answer the wage gap question.

# Try a log-normal model
fit_logn <- glm(I(log(wage)) ~ education + experience  + 
                  south + sex + union +  marr + 
                  race + occupation + sector, 
                  data = wages)
summary(fit_logn)
# Diagnostic plots
plot(fit_logn)

# Try a gamma model
fit_gamma <- glm(wage ~ education + experience  + 
                   south + sex + union +  marr + race + 
                   occupation + sector, 
                 data = wages, 
                 family = Gamma(link = "log"))
summary(fit_gamma)
# Diagnostic plots
plot(fit_gamma)

# Compare MSE of the two models
mean((wages$wage - fit_gamma$fitted.values)^2)
mean((wages$wage - exp(fit_logn$fitted.values))^2)

# Naive comparison of the AIC is irrelevent because 
# response of the fit_logn is log(wage) and not wage
AIC(fit_gamma)
AIC(fit_logn)

# Compute the AIC of log-normal model manually
# means of log(wage) given the covariates
logmu <- fit_logn$fitted.values
# sd of log(wages)
logsd <- sqrt(summary(fit_logn)$dispersion)
# individual log-likelihoods
log_liks <- dlnorm(wages$wage, meanlog = logmu, sdlog = logsd, log = T)
# total log Likelihood
log_Lik <- sum(log_liks)
# AIC of logn model is slightly smaller than AIC of gamma model:
AIC_logn <- -2 * (log_Lik + (16 + 1)) # 16 parameters + dispersion parameter


# Likelihood Ratio test for the "sector" variable using log-normal model
fit_logn_no_sector <- glm(I(log(wage)) ~ education + experience  + 
                            south + sex + union +  marr + race + 
                            occupation, 
                          data = wages)
# likelihood ratio statistic
lrt_stat <- 2 * (logLik(fit_logn) - logLik(fit_logn_no_sector))
# p-value
p_val <- 1 - pchisq(lrt_stat, df = 2) # df = 2, fit_logn_no_sector has 2 fewer param

# What about sex ?
fit_logn_no_sex <- glm(I(log(wage)) ~ education + experience  + 
                            south + union +  marr + race + 
                            occupation + sector, 
                          data = wages)
# likelihood ratio statistic
lrt_stat <- 2 * (logLik(fit_logn) - logLik(fit_logn_no_sex))
# p-value
p_val <- 1 - pchisq(lrt_stat, df = 1)


# Model selection using the step() function (select model with lowest AIC)
step(fit_logn)

# Fit the model indicated by the step() procedure
fit_reduce <- glm(log(wage) ~ sector + marr + south + union + 
                    sex + occupation + experience + education, 
                  data = wages)


# remove 171th and 200th observations
wages_no_outlier <- wages[-c(171, 200), ]

# fit without these obs
fit_logn_no_outlier <- glm(I(log(wage)) ~ education + experience  + 
                             south + sex + union +  marr + 
                             race + occupation + sector, 
                           data = wages_no_outlier)
summary(fit_logn_no_outlier)

# compare to previous fit
summary(fit_logn)

# Does it impact the step procedure ?
step(fit_logn_no_outlier)
# (no)

# Visualize if interaction of occupation and sex would be relevent
visreg(fit_logn, xvar = "occupation", by = "sex")

# Add occupation:sex interaction
fit_interact <- glm(log(wage) ~ sector + marr + south + union + 
                           sex + occupation:sex + experience + education, data = wages)

# This improves the AIC over the fit_reduce
AIC(fit_interact)
AIC(fit_reduce)


# All this could have been made using a gamma GLM fit instead of a log-normal 
# model, it would also have been relevent.