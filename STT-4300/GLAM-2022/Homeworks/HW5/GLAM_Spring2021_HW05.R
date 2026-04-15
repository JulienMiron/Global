### GLAM Spring 2020 HW 05 ####

rm(list=ls()) # clear the environnment of all objects defined previously
setwd("../datasets/")

### Exercise 3 ####

# Import the \verb"docvisits.asc" data file, which is part of the 1977-1978 
# Australian Health Survey and contains information on the number of consultations with 
# a doctor or specialist, denoted \texttt{dvisits}, in the two-weeks period before an interview. 
# These counts represent our response variable and we consider the following thirteen explanatory variables:

# - sex : coded 1 for female,
# - age : in years/100,
# - agesq: (age)^2/1000$,
# - income: annual income/1000,
# - illness:, number of illnesses in past two weeks, with five or more coded as 5,
# - actdays:, number of reduced activity days in past two weeks due to illness or injury,
# - hscore:, general health questionnaire score, with high scores indicating bad health,
# - chcond1: coded 1 if chronic condition(s) but not limited in activity,
# - chcond2: coded 1 if chronic condition(s) and limited in activity,
# - levyplus , freepoor and freerepa, three dummy variables for levels of health insurance, 
#   where ``levyplus'' represents a higher level of insurance cover 
#   while ``freepoor'' and ``freerepa'' are basic levels of state-provided insurances.

doc <- read.table(file = 'docvisits.asc', header=T)
hist(doc$dvisits)
barplot(table(doc$dvisits)) # a lot of zeros
sum(doc$dvisits == 0) / length(doc$dvisits)

# a. ======

source('truncpoisson.R')
library(pscl)


# a.1. Hurdle Poisson (separated inference) -----

# zeros modeling
doc.hurdle.0 <- glm((dvisits > 0) ~ sex + age + agesq + income + levyplus + 
                      freepoor + freerepa + illness + actdays + hscore + 
                      chcond1 + chcond2, 
                    data = doc, family = binomial)
summary(doc.hurdle.0)

# positive counts modeling (family truncpoisson)
doc.hurdle.pos <- glm(dvisits ~ sex + age + agesq + income + levyplus + 
                        freepoor + freerepa + illness + actdays + hscore + 
                        chcond1 + chcond2, 
                    data = doc, family = truncpoisson, subset = (dvisits > 0))
summary(doc.hurdle.pos)


round(summary(doc.hurdle.0)$coefficients, 4)
round(summary(doc.hurdle.pos)$coefficients, 4)

# a.2. with the available command -----

# with the hurdle command
doc.hurdle <- hurdle(dvisits ~ sex + age + agesq + income + levyplus + 
                       freepoor + freerepa + illness + actdays + hscore + 
                       chcond1 + chcond2,
                     data = doc,
                     dist = "poisson", zero.dist = "binomial", link = "logit")
summary(doc.hurdle)

round(summary(doc.hurdle)$coefficients$count, 4)
round(summary(doc.hurdle)$coefficients$zero, 4)

extractAIC(doc.hurdle)

# a.3. comparison of coefficients -----

# Abundance
round(data.frame(by.hand = coef(doc.hurdle.pos), 
                 with.command = doc.hurdle$coefficients$count), 4)
# Zero inflation (via hurdle)
round(data.frame(by.hand = coef(doc.hurdle.0),   
                 with.command = doc.hurdle$coefficients$zero), 4)

vcov(doc.hurdle)

# a.4. (Attempts) at residual analysis -----

doc.hurdle.res <- residuals(doc.hurdle) 

# = > predictions (fitted values) are only available on the response scale 
# = > no 'plot' methods implemented in either of the classes hurdle or zip. 

## (Pearson) Residuals vs. (Response) Predicted (by hand)
plot(doc.hurdle$fitted, doc.hurdle.res)
abline(h = 0, col = "red")
lines(ksmooth(x = doc.hurdle$fitted, 
              y = doc.hurdle.res, kernel = "normal"), col = "blue")

## We could try checking separately the fit for abundance and zero inflation
par(mfrow = c(2, 2))
plot(doc.hurdle.0)
library(statmod)
par(mfrow = c(2, 2))
for(k in 1:4){
  qqnorm(qresiduals(doc.hurdle.0))
  abline(0, 1, col = "red")
  } 
# => satisfying fit

# Analysis is trickier for the counts
par(mfrow = c(2, 2))
plot(doc.hurdle.pos)
library(statmod)
par(mfrow = c(2, 2))
for(k in 1:4){
  qqnorm(qresiduals(doc.hurdle.pos))
  qqline(qresiduals(doc.hurdle.pos))
} 
# => still quite skewed

# a.5. Hurdle Negative binomial -----

doc.hurdle.nb <- hurdle(dvisits ~ sex + age + agesq + income + levyplus + 
                          freepoor + freerepa + illness + actdays + hscore + 
                          chcond1 + chcond2,
                        dist = "negbin",
                       zero.dist = "binomial", link = "logit", data = doc)
summary(doc.hurdle.nb)
extractAIC(doc.hurdle.nb)
doc.hurdle.nb.res <- residuals(doc.hurdle.nb)

par(mfrow = c(1, 2))
plot(doc.hurdle.nb$fitted, doc.hurdle.res, main = "Hurdle NB")
abline(h=c(0, 2, -2), lty = 3)
lines(ksmooth(x = doc.hurdle.nb$fitted, 
              y = doc.hurdle.nb.res, kernel = "normal"), col = "blue")

qq.hurdle.nb = qqnorm(doc.hurdle.nb.res, main = "Hurdle NB")
qqline(doc.hurdle.nb.res, lty = 3)

extractAIC(doc.hurdle)
extractAIC(doc.hurdle.nb)

# => All we can do, since we haven't coded the "truncnegbin" family

# b. Fit a simple Poisson GLM and a ZIP model. =====

# Poisson GLM -----

doc.glm <- glm(dvisits ~ sex + age + agesq + income + levyplus + 
                 freepoor + freerepa + illness + actdays + hscore + 
                 chcond1 + chcond2, family = poisson, data = doc)

par(mfrow = c(2, 2))
plot(doc.glm)
par(mfrow = c(1, 1))

library(statmod)
par(mfrow = c(2, 2))
for (i in 1:4){
  rqresid.fit.pois <- qresid(doc.glm)
  qqnorm(rqresid.fit.pois) # heavy right tail
  qqline(rqresid.fit.pois, col = 'red')
}
# => Slightly right-skewed : not very satisfying 

# Comparing all models from the (attemptive) residual analysis -----

par(mfrow = c(1, 1))
plot(predict(doc.hurdle, type = "response"), 
     residuals(doc.hurdle, type = "pearson"), 
     col = "blue", pch = 2, cex = 0.75, main = "Hurdle",
     xlab = "Fitted", ylab = "Pearson Residuals")
points(doc.hurdle.nb$fitted, doc.hurdle.nb.res, 
       col = "red", pch = 3, cex = 0.75)
points(predict(doc.glm, type = "response"), 
       residuals(doc.glm,type = "pearson"), 
       col = "black", pch = 4, cex = 0.75)
legend("topright", legend = c("Hurdle Poisson", "Hurdle NB", "Poisson"),
       pch = c(2, 3, 4), col = c("blue", "red", "black"))
abline(h = c(0, 2, -2), lty = 3)

par(mfrow = c(1, 1))
qq.poisson = qqnorm(residuals(doc.glm,type = "pearson"), plot.it = FALSE)
qq.hurdle.poisson = qqnorm(doc.hurdle.res, plot.it = FALSE)
plot(qq.hurdle.poisson, col = "blue", pch = 2, cex = 0.75, main = "Hurdle")
points(qq.hurdle.nb, col = "red", pch = 3, cex = 0.75)
points(qq.poisson, col = "black", pch = 4, cex = 0.75)
qqline(residuals(doc.glm, type = "pearson"))
qqline(doc.hurdle.nb.res, lty = 3, col = "red")
qqline(doc.hurdle.res, lty = 3, col = "blue")
legend("topleft", legend = c("Hurdle Poisson", "Hurdle NB", "Poisson"),
       pch = c(2, 3, 4), col = c("blue", "red", "black"))

# ZIP -----

doc.zip <- zeroinfl(dvisits ~ sex + age + agesq + income + levyplus + 
                      freepoor + freerepa + illness + actdays + hscore + 
                      chcond1 + chcond2,
                    dist = "poisson",
                    link = "logit",
                    data = doc)
summary(doc.zip)

doc.zip.res <- residuals(doc.zip,type='pearson')
# plot(doc.zip.res)
# plot(doc.zip$fitted,doc.zip.res)
qq.zip = qqnorm(doc.zip.res, plot.it = T)
abline(a = 0,b = 1,col = 'red')

doc.zinb <- zeroinfl(dvisits ~ sex + age + agesq + income + levyplus + 
                       freepoor + freerepa + illness + actdays + hscore + 
                       chcond1 + chcond2,
                     dist = "negbin", 
                     link = "logit", 
                     data = doc)
summary(doc.zinb)

doc.zinb.res <- residuals(doc.zinb, type = 'pearson')
# plot(doc.zinb.res)
# plot(doc.zinb$fitted,doc.zinb.res)
qq.zinb = qqnorm(doc.zinb.res, plot.it = T)
abline(a = 0, b = 1, col = 'red')

# back to back

par(mfrow = c(1, 2))
plot(doc.zip$fitted, doc.zip.res, col = "blue", pch = 2, cex = 0.75, 
     main = "Zero Inflated", xlab = "Fitted", ylab = "Pearson Residuals")
points(doc.zinb$fitted, doc.zinb.res, col = "red", pch = 3, cex = 0.75)
points(predict(doc.glm, type = "response"), 
       residuals(doc.glm, type = "pearson"), 
       col = "black", pch = 4, cex = 0.75)
abline(h = 0, lty = 3)
abline(h = 2, lty = 3)
abline(h = -2, lty = 3)
legend("topright", legend = c("zip", "zinb", "Poisson"), pch = c(2, 3, 4), 
       col = c("blue", "red", "black"))

qq.poisson = qqnorm(residuals(doc.glm, type = "pearson"), plot.it = FALSE) 
plot(qq.zip, col = "blue", pch = 2, cex = 0.75, main = "Zero Inflated")
points(qq.zinb, col = "red", pch = 3, cex = 0.75)
points(qq.poisson, col = "black", pch = 4, cex = 0.75)
qqline(residuals(doc.glm, type = "pearson"), lty = 3)
qqline(doc.zip.res, lty = 3, col = "blue")
qqline(doc.zinb.res, lty = 3, col = "red")
legend("topleft", legend = c("zip", "zinb", "Poisson"), 
       pch = c(2, 3, 4), col = c("blue", "red", "black"))


par(mfrow = c(1, 2))
plot(doc.zinb$fitted, doc.zinb.res, col = "blue", pch = 2, cex = 0.75, 
     main = "Zero Inflated", xlab = "Fitted", ylab = "Pearson Residuals")
points(predict(doc.hurdle.nb, type = "response"), 
       residuals(doc.hurdle.nb, type = "pearson"), col = "red",
       pch = 3, cex = 0.75)
abline(h = 0, lty = 3)
abline(h = 2, lty = 3)
abline(h = -2, lty = 3)
legend("topright", legend = c("zinb", "Hurdle NB"), pch = c(2, 3), 
       col = c("blue", "red"))

plot(qq.zinb, col = "blue", pch = 2, cex = 0.75, main = "Zero Inflated")
points(qq.hurdle.nb, col = "red", pch = 3, cex = 0.75)
qqline(doc.zinb.res, lty = 3, col = "blue")
qqline(doc.hurdle.nb.res, lty = 3, col = "red")
legend("topleft", legend = c("zinb", "Hurlde NB"), 
       pch = c(2, 3), col = c("blue", "red"))

extractAIC(doc.hurdle.nb)
extractAIC(doc.zinb)

# c. Do high insurance levels make the number of consultations increase? =======

# Hurdle Poisson and NB -----

summary(doc.hurdle)$coef$zero[6:8, ]
#           Estimate Std. Error   z value    Pr(>|z|)
# levyplus  0.2670067  0.1006176  2.653678 0.007961979
# freepoor -0.6803834  0.2610695 -2.606139 0.009156941
# freerepa  0.4162405  0.1398714  2.975879 0.002921502

# => a high level insurance increases the odds of going to the doctor 

summary(doc.hurdle)$coef$count[6:8, ]
#             Estimate Std. Error    z value    Pr(>|z|)
# levyplus -0.15243205  0.1192158 -1.2786229 0.201029890
# freepoor  0.03506028  0.2643832  0.1326116 0.894500549
# freerepa -0.43890101  0.1458957 -3.0083202 0.002626962

# => however, among those who go, they don't necessarely go more often 
# (except for those under epa)

summary(doc.hurdle.nb)$coef$zero[6:8, ]
#            Estimate Std. Error   z value    Pr(>|z|)
# levyplus  0.2670067  0.1006176  2.653678 0.007961979
# freepoor -0.6803834  0.2610695 -2.606139 0.009156941
# freerepa  0.4162405  0.1398714  2.975879 0.002921502

# => same estimates (given separate inference)

summary(doc.hurdle.nb)$coef$count[6:8, ]
#             Estimate Std. Error     z value   Pr(>|z|)
# levyplus -0.30189418  0.2088168 -1.44573675 0.14825105
# freepoor  0.04586581  0.5180557  0.08853452 0.92945185
# freerepa -0.49542150  0.2683576 -1.84612435 0.06487415

# => none of these variables seems to have an impact. 

# ZIP, ZINB ----

summary(doc.zip)$coef$zero[6:8, ] # Zeroes
#            Estimate Std. Error    z value     Pr(>|z|)
# levyplus -0.4331837  0.1967258 -2.2019669 0.0276676470
# freepoor  0.3080610  0.5078183  0.6066362 0.5440923713
# freerepa -1.1490451  0.3049692 -3.7677420 0.0001647308

summary(doc.zip)$coef$count[6:8, ] # Abundance
#             Estimate Std. Error    z value   Pr(>|z|)
# levyplus -0.03376854 0.09646909 -0.3500452 0.72630480
# freepoor -0.37698697 0.23896278 -1.5775970 0.11465822
# freerepa -0.21525770 0.11718929 -1.8368376 0.06623387

summary(doc.zinb)$coef$zero[6:8, ] 
#            Estimate Std. Error    z value    Pr(>|z|)
# levyplus -0.6401250  0.2642640 -2.4222931 0.015422903
# freepoor  0.1106485  0.6590044  0.1679026 0.866659928
# freerepa -1.3751539  0.4473238 -3.0741803 0.002110818

summary(doc.zinb)$coef$count[6:8, ]
#             Estimate Std. Error    z value   Pr(>|z|)
# levyplus -0.09510214  0.1144313 -0.8310852 0.40592552
# freepoor -0.48126988  0.2825930 -1.7030493 0.08855884
# freerepa -0.18945002  0.1401850 -1.3514285 0.17655821

