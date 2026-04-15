rm(list = ls()) 
library(mgcv)
library(statmod)

#### Exercise 1 ####
vaso <- read.table(file = 'vaso.txt', header = T)
head(vaso)


boxplot(vol ~ vasoconstr, data = vaso)
boxplot(rate ~ vasoconstr, data = vaso)

boxplot(log(vol) ~ vasoconstr, data = vaso)
boxplot(log(rate)~ vasoconstr, data = vaso)

# Fit a GAM
fit_gam <- gam(vasoconstr ~ s(log(vol)) + s(log(rate), k = 20), 
               family = binomial, 
               data = vaso)

summary(fit_gam)
gam.check(fit_gam)

plot(fit_gam, residuals = T, pages = 1)
#Seems linear...

AIC(fit_gam)

# Fit a GLM
fit_glm <- glm(vasoconstr ~ log(vol) + log(rate), 
               family = binomial, 
               data = vaso)
plot(fit_glm)
AIC(fit_glm) # worse

# Actually, the GAM fit wasn't so linear:
plot(fit_gam, cex = 3, residuals = T, pages = 1, ylim = c(-1000,1000))

# Fit GAM without rate
fit_gam2 <- gam(vasoconstr ~ s(log(vol), k = 20), 
                family = binomial, 
                data = vaso)
gam.check(fit_gam2)
AIC(fit_gam2) # way worse!

# GAM without logs
fit_gam3 <- gam(vasoconstr ~ s(vol) + s(rate), 
                family = binomial, 
                data = vaso)
gam.check(fit_gam3)
plot(fit_gam3)
AIC(fit_gam3)

# This one is equivalent to a GLM : 
fit_glm2 <- glm(vasoconstr ~ vol + rate, 
                family = binomial, 
                data = vaso)
AIC(fit_glm2)
plot(fit_glm2$fitted.values, fit_gam3$fitted.values)
abline(0, 1)


# bivariate surface 
fit_gam4 <- gam(vasoconstr ~ te(vol, rate), family = binomial, data = vaso)
summary(fit_gam4)

gam.check(fit_gam4, cex = 1) # residuals close to 0

layout(1)
plot(fit_gam4, pers = T, theta = 5, phi = 30) 
# not linear, some interaction.



#### Exercise 2 ####
kyph <- read.table(file = 'kyphosis.txt', header = T)
head(kyph)

source(file = 'PairsLab.R')
PairsLab(data = kyph[, 1:3], label = kyph$Kyphosis) 
# 1s and 0s regions overlap (check "Number", for example)
# potential outliers : Number = 14, Age =  250

# Fit a full GAM
fit_gam <- gam(Kyphosis ~ s(Age) + s(Number) + s(Start), 
                family = binomial, data = kyph)
summary(fit_gam)

plot(fit_gam, residuals = TRUE, pch = 1)
# Number seems almost linear

hist(residuals(fit_gam, type = "pearson"), 25, col = "grey80")
abline(v = 0, col = "red")

# observations with high residuals may be outliers. 
out_res <- which(abs(residuals(fit_gam, type = "pearson")) > 1.96)


kyph[out_res, ]


gam.check(fit_gam)
kyph[kyph$Age > 240, ] # obs 15
kyph[kyph$Number == 14, ] # obs 28
out_x <- c(15, 28)

#### Do the same fit with those observations removed.

kyphsub <- kyph[-c(15, 28), ]
fit_gam_nox <- gam(Kyphosis ~ s(Age) + s(Number, k = 8) + s(Start), 
                   family = binomial, data = kyph[-out_x, ])
gam.check(fit_gam_nox)
summary(fit_gam_nox)
summary(fit_gam)

fit_gam$aic
fit_gam_nox$aic


par(mfrow = c(1, 2))
plot(fit_gam, cex = 3, residuals = T, select = 1, 
     main = 'full data', ylim = c(-6, 5))
plot(fit_gam_nox, cex = 3, residuals = T, select = 1, 
     ylim = c(-6, 5), main = 'without obs 15 and 28')
# not a big difference in the shape of curves

plot(fit_gam, cex = 3, residuals = T, select = 2, 
     main = 'full data', ylim = c(-3, 6))
plot(fit_gam_nox, cex = 3, residuals = T, select = 2, 
     ylim = c(-3, 6), main = 'without obs 15 and 28')
# more curvature without obs 28

plot(fit_gam, cex = 3, residuals = T, select = 3, 
     main = 'full data', ylim = c(-5, 5))
plot(fit_gam_nox, cex = 3, residuals = T, select = 3, 
     ylim = c(-5, 5), main = 'without obs 15 and 28')
par(mfrow = c(1, 1))
# no big difference



# Try without obs 11, 45, 79 ?


fit_gam_nores <- gam(Kyphosis ~ s(Age) + s(Number, k = 8) + s(Start), 
                   family = binomial, data = kyph[-out, ])
gam.check(fit_gam_nores)

fit_gam_nores$aic
fit_gam_nox$aic
fit_gam$aic

par(mfrow = c(1, 2))
plot(fit_gam, cex = 3, residuals = T, select = 1, 
     main = 'full data', ylim = c(-6, 5))
plot(fit_gam_nores, cex = 3, residuals = T, select = 1, 
     ylim = c(-6, 5), main = 'without obs 11, 45 and 79')
# big difference in the shape of curves

plot(fit_gam, cex = 3, residuals = T, select = 2, 
     main = 'full data', ylim = c(-3, 6))
plot(fit_gam_nores, cex = 3, residuals = T, select = 2, 
     ylim = c(-3, 6), main = 'without obs 11, 45 and 79')
# still linear but, difference

hist(residuals(fit_gam, type = "pearson"), 20)
abline(v = 0)
plot(fit_gam, cex = 3, residuals = T, select = 3, 
     main = 'full data', ylim = c(-5, 5))
plot(fit_gam_nores, cex = 3, residuals = T, select = 3, 
     ylim = c(-5, 5), main = 'without obs 11, 45 and 79')
par(mfrow = c(1, 1))



#### e. Are all the three predictors important? Test that $f(Number) = 0$ ####

fit_gam_nores_2 <- gam(Kyphosis ~ s(Age, sp = fit_gam_nores$sp["s(Age)"], k = 10) + 
                      s(Start, sp = fit_gam_nores$sp["s(Start)"], k = 10), 
                    family = binomial, data = kyph[-out, ])
gam.check(fit_gam_nores_2)
summary(fit_gam_nores_2)

plot(fit_gam_nores, cex = 3, select = 2, residuals = T,  ylim = c(-3, 6), 
     main = 'without obs 11, 45 and 79')
abline(h = 0, col = 'blue')
# 2. on res plots: 0 not always included in bands - not check

fit_gam_nores_2$gcv.ubre
fit_gam_nores$gcv.ubre

fit_gam_nores_2$aic
fit_gam_nores$aic
# 3. UBRE and AIC increase - not check


library(mgcv)


# Exercise 3

cig <- read.table(file = 'cigarettes.txt', header = T)
head(cig)
str(cig)

## a)

hist(cig$cigs) # counts, right-skewed as usual. Poisson maybe


plot(table(cig$cigs)) 
# peaks at 10, 15, 20, 30 and 40

## b) and c)

table(cig$educ) # only 8 different values observed

fit_gam <- gam(cigs ~ s(educ, k = 8) + s(cigpric, k = 8) + s(age) + s(income) + 
                 restaurn + white, 
               family = poisson, data = cig)

summary(fit_gam)


plot(fit_gam, pages = 1) 
# effect of age close to quadratic, max around 40 years old

## d)
fit_gam_log <- gam(cigs ~ s(educ, k = 8) + s(cigpric, k = 8) + s(age) + 
                    s(log(income)) + 
                    restaurn + white, 
                  family = poisson, data = cig)
plot(cig$income, cig$cigs)
par(mfrow = c(1, 2))
plot(fit_gam, select = 4) # log?
plot(fit_gam_log, select = 4) # log?
AIC(fit_gam)
AIC(fit_gam_log)


plot(fit_gam, select = 2)
abline(h = 0, col = 'blue') # 0 always included in bands

fit_gam_2 <- bam(cigs ~ s(educ, k = 8) + s(age) + s(income) + 
                     restaurn + white, 
                   family = truncpoisson, data = cig)
fit_gam$gcv.ubre
fit_gam2$gcv.ubre

AIC(fit_gam)
AIC(fit_gam2)