#### GLAM Spring 2020 HW 11 ####

rm(list = ls()) # clear the environnment of all objects defined previously
setwd("../datasets") # set your working directory containing the data files

library(mgcv)

#### Exercise 1 ####

# To illustrate the use of a GAM on a logistic example, we consider 
# the vasoconstriction data set. The data consist of 39 binary 
# responses denoting the presence or absence (coded 1/0 respectively) 
# of vasoconstriction ("vasoconstr") on the fingers' skin 
# after inspiration of a volume of air ("vol") at a rate 
# reported in the variable called "rate".

# --
# Outcome  
# --
# vasoconstr : the presence or absence (coded 1/0 respectively) 
#              of vasoconstriction on the fingers' skin

# --
# Covariates  
# --
# vol         : volume of air inhaled
# rate        : rate of inhalation 

# N = 39

vaso <- read.table(file = 'vaso.txt', header = T)

plot(as.data.frame(cbind(vasoconstr = jitter(vaso$vasoconstr), 
                         vol  = (vaso$vol), 
                         rate = (vaso$rate))))


plot(as.data.frame(cbind(vasoconstr = jitter(vaso$vasoconstr), 
                         log.vol  = log(vaso$vol), 
                         log.rate = log(vaso$rate))))
par(mfrow = c(1, 1))


#### 1.a. Estimate a logistic additive model ####
vasolog.gam <- gam(vasoconstr ~ s(log(vol)) + s(log(rate)), 
                   family = binomial, 
                   data = vaso)

summary(vasolog.gam)

plot(vasolog.gam, cex = 3, residuals = T, pages = 1)
# both functions seem close to linear

gam.check(vasolog.gam) # is k for log(rate) too low?
# see help(choose.k) for more info on choosing the k value


## Extracted from the help of gam.check

# [...] it is useful to be able to check the choice of k informally. 
# If the effective degrees of freedom for a model term are estimated to be 
# much less than k-1 then this is unlikely to be very worthwhile, 
# but as the EDF approach k-1, checking can be important.

# A useful general purpose approach goes as follows: 
#   (i)  fit your model and extract the deviance residuals; 
#   (ii) for each smooth term in your model, fit an equivalent, 
#        single, smooth to the residuals, using a substantially increased k 
#        to see if there is pattern in the residuals that could potentially be 
#        explained by increasing k.

#  a low p-value coupled with high EDF (close to k) suggests k may be too low. ##

par(mfrow = c(2, 2))
plot(gam(residuals(vasolog.gam, type = "deviance") ~ s(log(rate)), 
         data = vaso), residuals = TRUE, cex = 3)
plot(gam(residuals(vasolog.gam, type = "deviance") ~ s(log(vol)), 
         data = vaso), residuals = TRUE, cex = 3)
plot(gam(residuals(vasolog.gam, type = "deviance") ~ s(log(rate), k = 20), 
         data = vaso), residuals = TRUE, cex = 3)
plot(gam(residuals(vasolog.gam, type = "deviance") ~ s(log(vol), k = 20), 
         data = vaso), residuals = TRUE, cex = 3)
par(mfrow = c(1, 1))
# Not really evidence of the need of adjusting k's 

plot(residuals(vasolog.gam, type = "pearson"))
abline(h = -2, col = "red")
which.min(residuals(vasolog.gam, type = "pearson")) 
# obs 38 (perhaps influential). 

#### 1.b. Based on the AIC criteria, would a GLM be sufficient? ####

vasolog.glm <- glm(vasoconstr ~ log(vol) + log(rate), 
                   family = binomial, data = vaso)
summary(vasolog.glm)

par(mfrow = c(2, 2))
plot(vasolog.glm)
par(mfrow = c(1, 1))
# usual structure for logit model
# observations 4 and 18 could be influential

library(statmod)
par(mfrow = c(2, 3))
for (i in 1:6){
  rqresid.vasolog.glm <- qresid(vasolog.glm)
  qqnorm(rqresid.vasolog.glm)
  qqline(rqresid.vasolog.glm, col = 'red') 
  # Good already, but could be better
}
par(mfrow = c(1, 1))


options(digits = 5)
vasolog.glm$aic
vasolog.gam$aic # lower, but on the same order
# since functions from GAM are close to straight lines, 
# fit is almost the same

#### 1.c. Is the variable "rate" useful in our model? ####

# Heuristic approach for variable selection as proposed in 
# Wood and Augustin (2002) when the degrees 
# of freedom are estimated automatically:

# Three questions need to be asked:

# 1. Are the estimated degrees of freedom for the term close to 
#    their lower limit (e.g. equal 1)?
# 2. Does the confidence region for the smooth include zero 
#    everywhere?
# 3. Does the GCV (or UBRE) score for the model go down if the term is 
#    removed from the model?

# If the answer to all three of these is yes then the term should be dropped. 
# If the answer to 2 is no, then it probably should not be. 
# Other cases will require judgment.

summary(vasolog.gam)
# 1. edf close to 1 for log(rate) - check

plot(vasolog.gam, cex = 3, residuals = T, ylim = c(-50, 50), pages = 2)
abline(h = 0, col = "red")
par(mfrow = c(1, 1))
# 2. 0 not so much included everywhere in the confidence "bands" - not check

vasolog.gam2 <- gam(vasoconstr ~ s(log(vol)), family = binomial, data = vaso) 
# without "rate"
vasolog.gam$gcv.ubre
vasolog.gam2$gcv.ubre
vasolog.gam$aic
vasolog.gam2$aic
# 3. UBRE (and AIC) increased  - not check

# = > it seems better to keep log(rate)


vasolog.gam3 <- gam(vasoconstr ~ s(log(vol)) + log(rate), 
                   family = binomial, 
                   data = vaso)
vasolog.gam$gcv.ubre
vasolog.gam3$gcv.ubre
#### 2. Fit now the same model but without transforming the covariates. ####

vaso.gam <- gam(vasoconstr ~ s(vol) + s(rate), family = binomial, data = vaso)
summary(vaso.gam)

plot(vaso.gam, residuals = T, pages = 1, pch = 1)
# quite different from from vasolog.gam

gam.check(vaso.gam) 
# low p-value for rate and low edf, we could try increasing k.

vaso.gam3 <- gam(vasoconstr ~ s(vol, k = 15) + s(rate, k = 15), 
                 family = binomial, data = vaso)
summary(vaso.gam3)
gam.check(vaso.gam3)

plot(vaso.gam3, pages = 1, residuals = T, cex = 3)
# No systematic departures, seems overall good, yet very linear! 

#### 3. Estimate finally a bivariate surface and 
#       compare it with the two previous models. ####

vaso.gam4 <- gam(vasoconstr ~ te(vol, rate), family = binomial, data = vaso)
summary(vaso.gam4)

gam.check(vaso.gam4, cex = 1)
# near saturated, all residuals very close to 0

plot(vaso.gam4, pers = T, theta = 5, phi = 10) 
# not linear, some interaction. Hard to tell b/c lack of data

plot(residuals(vaso.gam4, type = 'pearson'))


#### Exercise 2 ####

# Data were collected on 83 patients who underwent corrective 
# spinal surgery (Bell et al., 1989). The objective of the study is 
# to determine the important risk factors for the curvature of the 
# spine (at least 40 degrees with respect to the vertical), following surgery.

# --
# Outcome  
# --
# Kyphosis  : the presence or absence (coded 1/0 respectively) of 
              # curvature in the spine

# --
# Covariates  
# --
# Age       : age of the patient in months
# Start     : vertebra level of the surgery (1-12 : thoracic vertebrae; 
#             13-17 : lumbar ones)
# Number    : vertebrae levels involved

# N = 83 patients

kyph <- read.table(file = 'kyphosis.txt', header = T)

#### a. Produce the pairwise scatter plots of all three predictors.

source(file = 'PairsLab.R')

PairsLab(data = kyph[, 1:3], label = kyph$Kyphosis) 
# covariates only, response represented by 0s and 1s
# 1s and 0s regions overlap (check "Number", for example)
# lack of separability with respect to these covariates.
# Number = 14, Age =  250

#### b. Fit an additive logistic model using all three predictors. ####

kyph.gam <- gam(Kyphosis ~ s(Age) + s(Number) + s(Start), 
                family = binomial, data = kyph)
summary(kyph.gam)

#### c. any values in "Age" and "Number" having a large effect? ####

plot(kyph.gam, residuals = TRUE, pch = 1)


# Number seems almost linear
# observation with Age near 250 may have a large impact
# observation with Number = 14 may have a large impact

summary(residuals(kyph.gam, type = "pearson")) 
boxplot(residuals(kyph.gam, type = "pearson"))
abline(h = 0)
# median not very close to 0 = > more negative res than positive ones
plot(residuals(kyph.gam, type = "pearson"))
abline(h = 0, col = 'red', lty = 2)
abline(h = median(residuals(kyph.gam, type = "pearson")), col = 'blue')
abline(h = c(-1.96, 1.96), col = 'grey', lty = 2)
# high positive and negative residuals.

which(abs(residuals(kyph.gam, type = "pearson")) > 1.96) # obs 11, 79 and 45

gam.check(kyph.gam)
kyph[kyph$Age > 240, ] # obs 15
kyph[kyph$Number == 14, ] # obs 28

#### d. Do the same fit with those observations removed. What happens ~ ?####

kyphsub <- kyph[-c(15, 28), ]
kyphsub.gam <- gam(Kyphosis ~ s(Age) + s(Number, k = 8) + s(Start), 
                   family = binomial, data = kyphsub)
gam.check(kyphsub.gam)
summary(kyph.gam)
summary(kyphsub.gam)

kyph.gam$aic
kyphsub.gam$aic
# Slight changes. Some improvement in AIC

par(mfrow = c(1, 2))
plot(kyph.gam, cex = 3, residuals = T, select = 1, 
     main = 'full data', ylim = c(-6, 5))
plot(kyphsub.gam, cex = 3, residuals = T, select = 1, 
     ylim = c(-6, 5), main = 'without obs 15 and 28')
# not a big difference in the shape of curves

plot(kyph.gam, cex = 3, residuals = T, select = 2, 
     main = 'full data', ylim = c(-3, 6))
plot(kyphsub.gam, cex = 3, residuals = T, select = 2, 
     ylim = c(-3, 6), main = 'without obs 15 and 28')
# a curve appears without obs 28

plot(kyph.gam, cex = 3, residuals = T, select = 3, 
     main = 'full data', ylim = c(-5, 5))
plot(kyphsub.gam, cex = 3, residuals = T, select = 3, 
     ylim = c(-5, 5), main = 'without obs 15 and 28')
par(mfrow = c(1, 1))
# no big difference



summary(residuals(kyphsub.gam, type = "pearson")) 
boxplot(residuals(kyphsub.gam, type = "pearson"))
abline(h = 0)
# median not very close to 0 = > more negative res than positive ones
plot(residuals(kyphsub.gam, type = "pearson"))
abline(h = 0, col = 'red', lty = 2)
abline(h = median(residuals(kyphsub.gam, type = "pearson")), col = 'blue')
abline(h = c(-1.96, 1.96), col = 'grey', lty = 2)
which(abs(residuals(kyph.gam, type = "pearson")) > 1.96) # obs 11, 79 and 45


# Try without obs 11, 45, 79 ?

kyphsub <- kyph[-c(11, 45, 79), ]
kyphsub.gam <- gam(Kyphosis ~ s(Age) + s(Number, k = 8) + s(Start), 
                   family = binomial, data = kyphsub)
gam.check(kyphsub.gam)
summary(kyph.gam)
summary(kyphsub.gam)

kyph.gam$aic
kyphsub.gam$aic


par(mfrow = c(1, 2))
plot(kyph.gam, cex = 3, residuals = T, select = 1, 
     main = 'full data', ylim = c(-6, 5))
plot(kyphsub.gam, cex = 3, residuals = T, select = 1, 
     ylim = c(-6, 5), main = 'without obs 11, 45 and 79')
# big difference in the shape of curves

plot(kyph.gam, cex = 3, residuals = T, select = 2, 
     main = 'full data', ylim = c(-3, 6))
plot(kyphsub.gam, cex = 3, residuals = T, select = 2, 
     ylim = c(-3, 6), main = 'without obs 11, 45 and 79')
# still linear but, difference

plot(kyph.gam, cex = 3, residuals = T, select = 3, 
     main = 'full data', ylim = c(-5, 5))
plot(kyphsub.gam, cex = 3, residuals = T, select = 3, 
     ylim = c(-5, 5), main = 'without obs 11, 45 and 79')
par(mfrow = c(1, 1))


summary(residuals(kyphsub.gam, type = "pearson")) 
boxplot(residuals(kyphsub.gam, type = "pearson"))
abline(h = 0)
# median not very close to 0 = > more negative res than positive ones
plot(residuals(kyphsub.gam, type = "pearson"))
abline(h = 0, col = 'red', lty = 2)
abline(h = median(residuals(kyphsub.gam, type = "pearson")), col = 'blue')
abline(h = c(-1.96, 1.96), col = 'grey', lty = 2)
which(abs(residuals(kyphsub.gam, type = "pearson")) > 2) # 

# we still have points outside [-2, 2], but they are much closer and the median
# is closer to 0


#### e. Are all the three predictors important? Test that $f(Number) = 0$ ####

kyphsub2.gam <- gam(Kyphosis ~ s(Age, sp = kyphsub.gam$sp["s(Age)"], k = 10) + 
                      s(Start, sp = kyphsub.gam$sp["s(Start)"], k = 10), 
                    family = binomial, data = kyphsub)
gam.check(kyphsub2.gam)
summary(kyphsub2.gam)
summary(kyphsub.gam)
gam.check(kyphsub.gam)
# 1. Number edf close to 1 - check

plot(kyphsub.gam, cex = 3, residuals = T, select = 2, ylim = c(-3, 6), 
     main = 'without obs 11, 45 and 79')
abline(h = 0, col = 'blue')
# 2. on res plots: 0 not always included in bands - not check

kyphsub.gam$gcv.ubre
kyphsub2.gam$gcv.ubre

kyphsub.gam$aic
kyphsub2.gam$aic
# 3. UBRE and AIC increase - not check


library(mgcv)

#  =  =  =  =  =  =  =  =  =  =  =  =  =  =  = 
# Exercise 3
#  =  =  =  =  =  =  =  =  =  =  =  =  =  =  = 

cig <- read.table(file = 'cigarettes.txt', header = T)
head(cig)
str(cig)

## a)

hist(cig$cigs, col = 'deeppink') # counts, right-skewed as usual


barplot(table(cig$cigs), col = 'deeppink', las = 2) 
# peaks at 10, 15, 20, 30 and 40

## b) and c)

source('truncpoisson_gam.r')

table(cig$educ) # only 8 different values observed

cig.gam <- bam(cigs ~ s(educ, k = 8) + s(cigpric, k = 8) + s(age) + s(income) + 
                 restaurn + white, 
               family = truncpoisson, data = cig)

summary(cig.gam)


plot(cig.gam, pages = 1) 
# effect of age close to quadratic, max around 40 years old

## d)
ciglog.gam <- bam(cigs ~ s(educ, k = 8) + s(cigpric, k = 8) + s(age) + 
                    s(log(income)) + 
                    restaurn + white, 
                  family = truncpoisson, data = cig)
par(mfrow = c(1, 2))
plot(cig$income, cig$cigs)
plot(cig.gam, select = 4) # log?
plot(ciglog.gam, select = 4) # log?

## e)

summary(cig.gam) # edf far from 1

plot(cig.gam, select = 2)
abline(h = 0, col = 'blue') # 0 always included in bands

cig.gam.sub <- bam(cigs ~ s(educ, k = 8) + s(age) + s(income) + 
                     restaurn + white, 
                   family = truncpoisson, data = cig)
cig.gam$gcv.ubre
cig.gam.sub$gcv.ubre
# UBRE decreased a little when removing cigpric
#  = > we could drop cigpric


