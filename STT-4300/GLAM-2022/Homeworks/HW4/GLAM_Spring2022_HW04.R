####
# GLAM Spring 2020 HW 04 ######
####

rm(list = ls())
# setwd("...")
library(statmod)
library(visreg)

# ===
# Exercise 1 # ===
# ===

# The proportion of children developing bronchitis or pneumonia in their first year of life 
# has been recorded by type of feeding (breast, bottle, breast with supplement) 
# and by the newborns sex

# ---
# a. ----
# ---

baby.bern <- read.table(file = 'babyfood-bernoulli.txt', header = T,sep = '')
baby.bin <- read.table(file = 'babyfood-binomial.txt', header = T,sep = '')

head(baby.bern)
head(baby.bin) # grouped by combination of sex and food

baby.bern.glm <- glm(disease ~ sex + food, data = baby.bern, family = binomial)
baby.bin.glm <- glm(cbind(disease, nondisease) ~ sex + food, data = baby.bin,
                    family = binomial)

# ---
# a.1. Estimates ----
# ---

data.frame(
  beta.bern = coef(baby.bern.glm), 
  beta.bin  = coef(baby.bin.glm)
  )

# Both are exactly the same

# ---
# a.2. Standard errors ----
# ---

# str(summary(baby.bern.glm))

data.frame(
  stderr.bern = summary(baby.bern.glm)$coef[, 2], 
  stderr.bin  = summary(baby.bin.glm)$coef[, 2]
)

# Both are exactly the same again

# ---
# a.3. Deviances ----
# ---

data.frame(
  dev.bern = summary(baby.bern.glm)$deviance, # much larger
  dev.bin  = summary(baby.bin.glm)$deviance
)

# There's a difference

# ---
# a.4. AIC criterion ----
# ---

AIC.bern = summary(baby.bern.glm)$aic
AIC.bin  = summary(baby.bin.glm)$aic

data.frame(
  AIC.bern,
  AIC.bin 
)

# Not "nested" because of the binomial coef in the Likelihood.

# ---
# a.5. Residual Plots ----
# ---

par(mfrow =c(2, 2))
plot(baby.bern.glm, which=c(1, 2), main = "Bernoulli")
plot(baby.bin.glm, which=c(1, 2), main = "Binomial")



# Residuals present the same type of structure as we would expect using both specifications,
# namely, bimodality of bernoulli residuals and approximate normality for binomial (as #trials increases).

par(mfrow=c(1, 1))
rqresid.bern <- qresiduals(baby.bern.glm)
qqnorm(rqresid.bern)
qqline(rqresid.bern, col = "red")
abline(a = 0, b = 1 , col = "green")
par(mfrow=c(1, 1))
rqresid.bin <- qresiduals(baby.bin.glm)
qqnorm(rqresid.bin)
qqline(rqresid.bin, col = "red")
abline(a = 0, b = 1, col = "green")
###########################################
# ---
# b. ----
# ---

# See the analytical solution. This is just verification of the theory. 

# Values of the log-likelihood

ll.bern = logLik(baby.bern.glm)
ll.bin  = logLik(baby.bin.glm)

data.frame(
  ll.bern,
  ll.bin
)

# Checking whether the difference in likelihood is right 

Y_i <- baby.bin$disease                        #"Successes"
m_i <- baby.bin$nondisease + baby.bin$disease  #"Trials"

ll.bern - ll.bin
-sum(log(choose(m_i, Y_i))) 

# Difference is the combinatory term.

AIC.bern  - AIC.bin 
2*sum(log(choose(m_i, Y_i))) # \times 2 since it is computed on the scale of the deviance.

# ---
# c. ----
# ---

# Deviance of the Bernoulli Model

y_i   <- baby.bern$disease
eta_i <- baby.bern.glm$lin 
p_i   <- baby.bern.glm$fitted

-2*sum(y_i*eta_i-log(1+exp(eta_i))) 
# -2 loglikelihood = deviance because the saturated is zero.
-2*sum(p_i*eta_i-log(1+exp(eta_i))) 
# the same because of the estimating equations

baby.bern.glm$dev # the same

# Deviance of the Binomial Model

p_i <- baby.bin.glm$fitted

2*sum((log((Y_i/(m_i*p_i))^(Y_i))+
         +log(((1-Y_i/m_i)/(1-p_i))^(m_i-Y_i)))) # deviance

baby.bin.glm$dev # the same

 
# Not problematic to use the AIC, since it is based on the loglikelihood and not the deviance
###########################################

# ===
# Exercise 2 ####
# ===

# The dataset ("CpsWages.txt") consists of a random sample of 534 people 
# from the Current Population Survey, 
# with information on wages (wage, in dollars per hour) 
# and other characteristics of the workers, including

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

# ---
# a. Specification ####
# ---

cps <- read.table(file = 'CpsWages.txt', header = T, sep = "")
head(cps)
str(cps)

# properly setting as factors

cps$south   <- as.factor(cps$south)
cps$sex     <- as.factor(cps$sex)
cps$union   <- as.factor(cps$union)
cps$race    <- as.factor(cps$race)
cps$occupation  <- as.factor(cps$occupation)
cps$sector      <- as.factor(cps$sector)
cps$marr        <- as.factor(cps$marr)

str(cps)

# ---
# a.1. Exploratory Analysis ####
# ---

par(mfrow=c(1, 2))
hist(cps$wage, col='lightblue', nclass=10, prob=T, ylim=c(0, 0.12))
lines(density(cps$wage), col = 'red')
hist(log(cps$wage), col = 'lightblue', prob=TRUE)
lines(density(log(cps$wage)), col = 'red')
par(mfrow=c(1, 1))
sum(cps$wage <= 0) 
# => right skewed and strictly positive responses
# => use of gamma?

par(mfrow=c(1, 2))
qqnorm(cps$wage, main = 'Normal Q-Q plot of cps$wage')
qqline(cps$wage, col = "red")
qqnorm(log(cps$wage), main = 'Normal Q-Q plot of log(cps$wage)')
qqline(log(cps$wage), col = "red")
par(mfrow=c(1, 1))
# => Not necessarily Normal in either case. Right skewed and 
# => heavy tailed when transformed
# => use of gamma!
par(mfrow=c(1, 2))
hist(cps$wage, col = 'lightblue', prob=TRUE)
lines(density(cps$wage), col = 'red')
plot(density(rgamma(1000, shape = 1)))

par(mfrow=c(2, 2))
plot(cps$experience, cps$age)
plot(cps$experience, cps$education)
plot(cps$education, cps$age)
par(mfrow=c(1, 1))
# => However, linear correlation between covariates can be seen

cor(data.frame(cps$experience, cps$age,cps$education)) 
# => Use either age or experience, to protect against colinearity.

# ---
# b. Fit the proposed forms ####
# ---

# ---
# b.1. fit 1 - gamma glm with log link and without experience ####
# ---

cps.fit1 <- glm(wage ~ education + south + sex + age + union + race + 
                  occupation + sector + marr,
                family = Gamma(link = log),
                data = cps)
summary(cps.fit1)
# education : positive and significant at 5% level
# south     : negative 
# sex       : negative
# etc. etc. 

# Default residual plots ----

par(mfrow=c(2, 2))
plot(cps.fit1)


# RQResiduals ----

par(mfrow = c(1, 2))
rqresid.cps.fit1 <- qresiduals(cps.fit1)
qqnorm(rqresid.cps.fit1)
qqline(rqresid.cps.fit1)
# => Seems like a reasonable fit. 

a <- rnorm(1000)
qqnorm(a)
qqline(a)
# Residuals vs. Covariates ----

cps.fit1.res <- residuals(cps.fit1, type = 'deviance')

par(mfrow=c(1, 1))
plot(cps$education, cps.fit1.res, main = "education")
abline(h = 0, lty = 2, col = "red")
lines(smooth.spline(cps$education, cps.fit1.res), col = 'limegreen', lwd = 2)
# Polynomial structure? 
# Perhaps illusion due to isolated point with low education. 

plot(cps$age, cps.fit1.res, main = "age") 
lines(smooth.spline(cps$age, cps.fit1.res, df = 4), col = 'limegreen', lwd = 2)
abline(h = 0, lty = 2, col = "red")
# Quadratic structure?


par(mfrow=c(1,1))

#
pchisq(cps.fit1$dev,
       df = cps.fit1$df.res,
       lower.tail = F) # model seems as good as saturated 

pchisq(cps.fit1$null.dev-cps.fit1$dev,
       df = cps.fit1$df.null-cps.fit1$df.res,
       lower.tail = F) # model better than intercept only

# ---
# b.2. fit 2 gamma glm without experience and quadratic term for age ####
# ---

cps.fit2 <- glm(wage ~ education + south + sex +
                  age + I(age ^ 2) +
                  union + race + occupation + sector + marr, 
                family = Gamma(link = log), data = cps)
summary(cps.fit2)
cps.fit2.res <- residuals(cps.fit2, type = 'deviance')

# Residuals  vs Age ----
par(mfrow =c(1, 1))
plot(cps$age, cps.fit2.res, main = "age")
abline(h = 0, lty = 2)
lines(smooth.spline(cps$age, cps.fit2.res, df = 4), col = 'limegreen', lwd = 4)

# => seems ok now

# Default residual plots ----
par(mfrow = c(2, 2))
plot(cps.fit2)
par(mfrow = c(1, 1))

# Randomized Quantile Residuals ----
rqresid.cps.fit2 <- qresid(cps.fit2)
qqnorm(rqresid.cps.fit2)
qqline(rqresid.cps.fit2)

par(mfrow = c(1, 1))

summary(cps.fit2)

anova(cps.fit1, cps.fit2, test = "Chisq") 

# significant, so adding I(age^2) is useful

# final model :
# wage ~ education + south + sex + age + I(age ^ 2) + 
#        union + race + occupation + sector + marr

# ---
# c. Checking parameter estimates and significance of sector ####
# ---

summary(cps.fit2)

# Likelihood Ratio test on a model fitted without sector. ####

cps.fit3 <- glm(wage ~ education + south + sex + age + union + race + 
                       occupation + marr + I(age^2),
                family = Gamma(link = log),
                data = cps)
anova(cps.fit2, cps.fit3, test = "Chisq")
# not significant, sector is not necessary

# ---
# d. Global approach of model selection ####
# ---

step(cps.fit2) # global (but not exhaustive stepwise) approach

# ---
# e. Estimate the final model proposed by the previous step ####
# ---

cps.fit4 <- glm(wage ~ education + south + sex + age + I(age^2) + union + 
                       occupation, family = Gamma(link = log), data = cps)

summary(cps.fit4)
pchisq(cps.fit4$dev, df = cps.fit4$df.res, lower.tail = F) 
# as good as saturated model

par(mfrow = c(2, 2))
plot(cps.fit4, which = 1:4)
par(mfrow = c(1, 1))

rqresid.cps.fit4 <- qresid(cps.fit4)
qqnorm(rqresid.cps.fit4)
qqline(rqresid.cps.fit4)

cps.fit4.res <- residuals(cps.fit4, type = 'deviance')

par(mfrow = c(1,1))

plot(cps.fit4.res)

which(abs(cps.fit4.res) > 1.5) # obs. 171 and 200

par(mfrow = c(2, 1))
acf(cps.fit4.res)
pacf(cps.fit4.res)
par(mfrow = c(1, 1))

# ---
# f. Robustness ####
# ---

cps.omit <- cps[-c(171,200), ]
cps.fit5 <- glm(wage ~ education + south + sex + age + I(age^2) + union +
                  occupation, family = Gamma(link = log), data = cps.omit)

data.frame(with_obs = summary(cps.fit4)$coef[, 1], 
           without_obs = summary(cps.fit5)$coef[, 1])
# => estimates are similar
data.frame(with_obs = summary(cps.fit4)$coef[, 2],
           without_obs = summary(cps.fit5)$coef[, 2])
# => std dev are smaller 
data.frame(with_obs = summary(cps.fit4)$coef[,4],
           without_obs = summary(cps.fit5)$coef[, 4])
# => p-values are smaller, consequently

# ---
# g. Interaction ####
# ---

cps.fit6 <- glm(wage ~ education * sex + south + age + I(age ^ 2) + union + 
                  occupation, family = Gamma(link = log), data = cps.omit)
summary(cps.fit6)
anova(cps.fit6, cps.fit5, test = "Chisq") # worth it

# ---
# h. Visualization ####
# ---

visreg(fit = cps.fit6, xvar = "education", by = "sex", type = "conditional", 
       scale = "response")
visreg(fit = cps.fit6, xvar = "education", by = "sex", type = "conditional", 
       scale = "response", overlay = TRUE)
visreg(fit = cps.fit6, xvar = "education", type = "conditional", 
       scale = "response")

x11()
par(mfrow=c(2, 1),mar=c(1, 1, 1, 1))
visreg2d(fit = cps.fit6, xvar = "age", yvar = "education", type = "conditional", 
         scale = "response", plot.type = "persp", cond = list("sex"=1), 
         main = "female")
visreg2d(fit = cps.fit6, xvar = "age", yvar = "education", type = "conditional", 
         scale = "response", plot.type = "persp", cond = list("sex"=0), 
         main = "male")
# better to condition by yourself, by default it chooses the mode for factors

