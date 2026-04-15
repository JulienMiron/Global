rm(list = ls())
# install.packages("robustbase")
library(robustbase)
food <- read.table("C:/Users/Benjamin/Dropbox/GLAM-2022/Homeworks/HW8/foodstamp.dat", 
                   header = TRUE)
head(food)

hist(log(food$Income))

boxplot(log(1 + Income) ~ Participation, data = food)
mosaicplot(Tenancy ~ Participation, data = food)
mosaicplot(Suppl.Income ~ Participation, data = food)

boxplot(Income ~ Participation, data = food)
boxplot(log(1 + Income) ~ Participation, data = food)

glmrob()

# Why change to log(Income) ? 
bins <- quantile(food$Income, 0:10 / 10, na.rm = T)

plot(as.factor(Participation) ~ Income, data = food)
plot(as.factor(Participation) ~ log(1 + Income), data = food)

food$lincome <- log(food$Income + 1)

# Robust fit
fit_1 <- glmrob(Participation ~ Tenancy + Suppl.Income + lincome,
                data = food,
                family = binomial,
                weights.on.x='robCov', 
                tcc = 1.55) 
summary(fit_1)

# Non robust fit
fit_2 <- glmrob(Participation ~ Tenancy + Suppl.Income + lincome,
                data = food,
                family = binomial,
                weights.on.x='robCov', 
                tcc = 40) 
summary(fit_2)


plot(fit_1$fitted.values, fit_1$residuals, 
     xlab = "fitted values", ylab = "residuals", 
     col = 1 + (fit_1$residuals^2 > 4), pch = 19, , main = "robust fit")
plot(fit_1$fitted.values, fit_1$residuals, 
     xlab = "fitted values", ylab = "residuals", 
     col = 1 + (fit_1$residuals^2 > 4), pch = 19, 
     ylim = c(-2, 2),  , main = "robust fit")


plot(fit_1$fitted.values, fit_1$residuals, 
     xlab = "fitted values", ylab = "residuals", 
     col = 1 + (fit_1$residuals^2 > 4), pch = 19)
plot(fit_1$fitted.values, fit_1$residuals, 
     xlab = "fitted values", ylab = "residuals", 
     col = 1 + (fit_1$residuals^2 > 4), pch = 19, 
     ylim = c(-2, 2), main = "non robust fit")


plot(fit_2$fitted.values, fit_2$residuals, 
     xlab = "fitted values", ylab = "residuals", 
     col = 1 + (fit_1$residuals^2 > 4), pch = 19)



plot(fit_1$fitted.values, fit_1$w.r, 
     xlab = "fitted values", ylab = "residuals", 
     col = 1 + (fit_1$residuals^2 > 4), pch = 19)
plot(fit_2$fitted.values, fit_2$w.r, 
     xlab = "fitted values", ylab = "residuals", 
     col = 1 + (fit_1$residuals^2 > 4), pch = 19)



summary(residuals(fit_1, type = "pearson")) 
summary(residuals(fit_2, type = "pearson")) 

# Impact of removing one observation :
out <- 5 # 22

fit_2_out <- glmrob(Participation ~ Tenancy + Suppl.Income + lincome,
                data = food[-out, ],
                family = binomial,
                weights.on.x='robCov', 
                tcc = 40) 

fit_1_out <- glmrob(Participation ~ Tenancy + Suppl.Income + lincome,
                    data = food[-out, ],
                    family = binomial,
                    weights.on.x = 'robCov', 
                    tcc = 1.55) 

fit_1_out$coefficients
fit_1$coefficients

fit_2_out$coefficients
fit_2$coefficients

# Final model

final_fit <- glmrob(Participation~Tenancy+lincome,
                    data = food,
                    family = binomial,
                    weights.on.x = 'robCov',
                    tcc = 1.55)