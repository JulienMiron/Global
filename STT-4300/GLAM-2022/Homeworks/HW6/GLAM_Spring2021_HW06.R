### 
# GLAM Spring 2021 HW 06 #####
### 

rm(list = ls())
# setwd("../datasets/")

library(gee)

# ==== Exercise 1 ====

# Out of many features of an ongoing LEI, we would like to identify which ones 
# predict its success.

# ---
# Outcome  
# ---

# SUCCESS   : ( = 0 or  =1) trainee i successfully performs a complete LEI 
#             in less than 30 seconds during trial $j$

# ---
# Covariates : 
# ---

# NECKFLEX  : neck flexion
# EXTOA     : atlanto-occipital extension
# PROPLGSP  : whether the trainee inserts the scope properly
# PROPLIFT  : whether the lift is performed successfully 
# ASKASSIS  : whether there is appropriate request for help 
# HELP      : whether there is unsolicited intervention by the attending 
#             anesthesiologist
# COMPS     : whether there are complications
# TRHAND    : the trainee's handedness
# TRGEND    : and the trainee's gender.

# ---
# Grouping identificator : 
# ---

# TRAINEE   : identifies the 19 trainees, who performed between 18 to 33 trials 
#             each. (n_i 's)
# TRIALCAT  : the trial's chronological position. 
#             = 1 trials 1-5, 
#             = 2 trials 6-10, etc.

# N = 436 LEI performed during this longitudinal study; 
# the file contains 443 rows but some observations are missing


# --- a. Suggest a model and estimate it. ----

lei.ori <- read.table('LEI.dat', header = T)
head(lei.ori)
str(lei.ori)

summary(lei.ori)

sum(is.na(lei.ori)) # some missing values
lei <- na.omit(lei.ori) # excluding them

table(lei$TRAINEE) # Not exactly balanced
summary(lei$TRIAL)
head(lei)

# Visualize the trajectories per subject.
library(lattice) # for specific graphics
xyplot(factor(SUCCESS) ~ TRIAL | TRAINEE,
       data = lei, type = 'b', scale = list(c(tick.number = 2)), 
       as.table = TRUE) # we can see that once a trainee has a success, the 
                        # successes are more frequent afterwards

lei.gee1 <- gee(
  SUCCESS ~ EXTOA + PROPLGSP + PROPLIFT + ASKASSIS + HELP + COMPS + NECKFLEX + 
    TRHAND + TRGEND + factor(TRIALCAT),
  id = TRAINEE, # which one is the indep index?
  corstr = "exchangeable",
  family = binomial,
  data = lei)
summary(lei.gee1)


lei.gee1$res 
# except these are raw residuals = lei$SUCCESS - lei.gee1$fitted
head(cbind(lei.gee1$res, lei$SUCCESS - lei.gee1$fitted.values))

options(digits = 5)
summary(lei.gee1)
library(lattice) # for the bwplot function
bwplot(lei$TRAINEE ~ lei.gee1$res,
       xlab = 'raw residuals',
       ylab = 'Trainee') # raw residuals

res.lei.gee1 <- (lei$SUCCESS - lei.gee1$fitted) / 
  sqrt(lei.gee1$fitted * (1 - lei.gee1$fitted)) 
# construct Pearson res manually
bwplot(lei$TRAINEE ~ res.lei.gee1, 
       xlab = 'Pearson residuals', 
       ylab = 'Trainee', 
       panel = function(...) {
         panel.abline(v = c(0, -2, 2) , col = "black", lty = 2)
         panel.bwplot(...)
       }
       ) 

# pearson residuals this time
# median around 0 => ok
# however large residual values for trainee 1 and 13 => 
# could affect estimation
# different variances for each trainee => 
# not a problem since different mu_i estimates.


# --- b. More parsimonious model According to the Robust $z$-statistic ----

data.frame(
  summary(lei.gee1)$coef, 
  cand = ifelse(abs(summary(lei.gee1)$coef[, 5]) == 
                  min(abs(summary(lei.gee1)$coef[, 5])), "<--", "")
)

lei.gee2 <- gee(SUCCESS ~ EXTOA + PROPLGSP + PROPLIFT + ASKASSIS + HELP + 
                  COMPS + NECKFLEX + TRGEND + factor(TRIALCAT),
                id = TRAINEE, 
                corstr = "exchangeable",
                family = binomial, 
                data = lei, 
                na.action = na.omit)
data.frame(
  summary(lei.gee2)$coef,
  cand = ifelse(abs(summary(lei.gee2)$coef[, 5]) == 
                  min(abs(summary(lei.gee2)$coef[, 5])), "<--", "")
  )
# already droped TRHAND, now drop TRGEND

lei.gee3 <- gee(SUCCESS ~ EXTOA + PROPLGSP + PROPLIFT + ASKASSIS + HELP + 
                  COMPS + NECKFLEX + factor(TRIALCAT),
                id = TRAINEE,
                corstr = "exchangeable",
                family = binomial,
                data = lei, 
                na.action = na.omit)
data.frame(
  summary(lei.gee3)$coef, 
  cand = ifelse(abs(summary(lei.gee3)$coef[, 5]) == 
                  min(abs(summary(lei.gee3)$coef[, 5])), "<--", "")
)
# now drop EXTOA

lei.gee4 <- gee(SUCCESS ~ PROPLGSP + PROPLIFT + ASKASSIS + HELP + COMPS + 
                  NECKFLEX + factor(TRIALCAT),
                id = TRAINEE,
                corstr = "exchangeable",
                family = binomial,
                data = lei,
                na.action = na.omit)
data.frame(
  summary(lei.gee4)$coef,
  cand = ifelse(abs(summary(lei.gee4)$coef[, 5]) == 
                  min(abs(summary(lei.gee4)$coef[, 5])), "<--", "")
)
# now drop NECKFLEX

lei.gee5 <- gee(SUCCESS ~ PROPLGSP + PROPLIFT + ASKASSIS + HELP + COMPS + 
                  factor(TRIALCAT),
                id = TRAINEE,
                corstr = "exchangeable",
                family = binomial,
                data = lei,
                na.action = na.omit)
data.frame(
  summary(lei.gee5)$coef,  
  cand = ifelse(abs(summary(lei.gee5)$coef[, 5]) == 
                  min(abs(summary(lei.gee5)$coef[, 5])), "<--", "")
)
# drop ASKASSIS

lei.gee6 <- gee(SUCCESS ~ PROPLGSP + PROPLIFT + HELP + COMPS + factor(TRIALCAT),
                id = TRAINEE,
                corstr = "exchangeable",
                family = binomial,
                data = lei,
                na.action = na.omit)
data.frame(
  summary(lei.gee6)$coef,  
  cand = ifelse(abs(summary(lei.gee6)$coef[, 5]) == 
                  min(abs(summary(lei.gee6)$coef[, 5])), "<--", "")
)
# drop COMPS

lei.gee7 <- gee(SUCCESS ~ PROPLGSP + PROPLIFT + HELP + factor(TRIALCAT),
                id = TRAINEE,
                corstr = "exchangeable",
                family = binomial,
                data = lei,
                na.action = na.omit)
data.frame(
  summary(lei.gee7)$coef,  
  cand = ifelse(abs(summary(lei.gee7)$coef[, 5]) == 
                  min(abs(summary(lei.gee7)$coef[, 5])), "<--", "")
)
# stop here

res.lei.gee7 <- (lei$SUCCESS - lei.gee7$fitted) / 
  sqrt(lei.gee7$fitted * (1 - lei.gee7$fitted))
bwplot(lei$TRAINEE ~ res.lei.gee7,
       xlab = 'Pearson residuals',
       ylab = 'Trainee',
       panel = function(...) {
         panel.abline(v = c(0, -1.96, 1.96) , col = "black", lty = 2)
         panel.bwplot(...)
       }
)

# --- c. Trying independence working correlation structure ----

lei.gee8 <- gee(SUCCESS ~ PROPLGSP + PROPLIFT + HELP + factor(TRIALCAT),
                id = TRAINEE,
                corstr = "independence",
                family = binomial,
                data = lei,
                scale.fix = TRUE)
summary(lei.gee8)

res.lei.gee8 <- (lei$SUCCESS - lei.gee8$fitted) / 
  sqrt(lei.gee8$fitted * (1 - lei.gee8$fitted))
bwplot(lei$TRAINEE ~ res.lei.gee8, 
       xlab = 'Pearson residuals', 
       ylab = 'Trainee',
       panel = function(...) {
         panel.abline(v = c(0, -1.96, 1.96), col = "black", lty = 2)
         panel.bwplot(...)
       })
# results change a little

options(digits = 6)
data.frame(
  lei.gee8$coef,
  lei.gee7$coef  
)

# Naive S.E

data.frame(
  summary(lei.gee8)$coef[, 2],
  summary(lei.gee7)$coef[, 2]
)

# Robust S.E

data.frame(
  summary(lei.gee8)$coef[, 4],
  summary(lei.gee7)$coef[, 4]
)

head(lei.gee7$working.corr)
head(lei.gee8$working.corr)

# 0.037 is very close to 0, so assuming "independence" might be coherent

# --- d. Compare with GLM ----

lei.glm <- glm(SUCCESS ~ PROPLGSP + PROPLIFT + HELP + factor(TRIALCAT),
               family = binomial,
               data = lei,
               na.action = na.omit)
summary(lei.glm)
par(mfrow = c(2, 2))
plot(lei.glm)
par(mfrow = c(1, 1))

library(statmod)
par(mfrow=c(2, 2))
for (i in 1:4){
  rqresid.fit <- qresid(lei.glm)
  qqnorm(rqresid.fit)
  abline(a = 0, b = 1, col = "red")
}
par(mfrow = c(1, 1))

options(digits = 6)
data.frame(
  beta.glm = lei.glm$coef,  
  beta.gee = lei.gee8$coef
  )
# One and the same

data.frame(
  s.e_glm = summary(lei.glm)$coef[, 2],
  s.e_gee = summary(lei.gee8)$coef[, 2]
)
# Similar up to roundings

# ==== Exercise 2 ====

library(lattice) # for specific graphics

# ---
# Outcome  
# ---
# follicles :  number of ovarian follicles greater than 10mm in diameter

# ---
# Covariates 
# ---
# Time      : Day of the measure. The ovarian follicles were recorded daily from 
#             three days before ovulation until three days after the following 
#             ovulation. 
#             The measurement times were scaled so that ovulation for each 
#             mare occurs at times 0 and 1

# ---
# Grouping identificator : 
# ---
# Mare      : identifier of each of the 11 mares 

# ---
# Analysis 
# ---

ovary <- read.table('Ovary.dat', header = T)
head(ovary)
str(ovary)

plot(ovary)

table(ovary$follicles)

boxplot(follicles ~ Mare, names = 1:max(ovary$Mare),
        xlab = 'Mare',
        ylab = 'Number of follicles',
        col = 'cyan', 
        data = ovary)

# Let's try an exchangeable correlation structure 
ov.gee <- gee(follicles ~ Time, 
              id = Mare, 
              corstr = "exchangeable",
              family = poisson,
              data = ovary) # Estimates \phi (by default)
summary(ov.gee)

ov.gee <- gee(follicles ~ Time,
              id = Mare,
              corstr = "exchangeable",
              family = poisson,
              data = ovary, 
              scale.fix = TRUE)
summary(ov.gee)

res.ov.gee <- (ovary$follicles - ov.gee$fitted) / 
  sqrt(ov.gee$fitted) # construct Pearson residuals manually
bwplot(ovary$Mare ~ res.ov.gee,
       xlab = 'Pearson residuals',
       ylab = 'Mare',       
       panel = function(...) {
         panel.abline(v = c(-1.96, 0, 1.96), lty = 2)
         panel.bwplot(...)
         }
       )
# most residuals between -2 and +2

head(ov.gee$working.corr)
# independence of observations does not seem to be a good idea, however we try 
# it to see the diff.
ov.gee2 <- gee(follicles ~ Time, 
               id = Mare,
               corstr = "independence",
               family = poisson,
               data = ovary,
               scale.fix = T)
summary(ov.gee2)

res.ov.gee2 <- (ovary$follicles - ov.gee2$fitted) / 
  sqrt(ov.gee2$fitted)
bwplot(ovary$Mare ~ res.ov.gee2,
       xlab = 'Pearson residuals',
       ylab = 'Mare',       
       panel = function(...) {
         panel.abline(v = c(-1.96, 0, 1.96), lty = 2)
         panel.bwplot(...)
       }
)

summary(ov.gee)$coef 
summary(ov.gee2)$coef # coeff don't change much, neither inference, but huge 
                      # difference for the intercept's Naive z.

