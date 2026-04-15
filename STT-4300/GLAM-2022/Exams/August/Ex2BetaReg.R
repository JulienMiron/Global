# GLAM exam,  August 2022
# Exercice 2 on beta regression

rm(list=ls())
require(betareg)
require(mgcv)

data("FoodExpenditure", package = "betareg")

plot(I(food/income)~income,data=FoodExpenditure,
     xlab="income",ylab="proportion of food expenditure")

fit_beta <- gam(I(food/income) ~ income + persons,family=betar, data = FoodExpenditure)
summary(fit_beta)
logLik(fit_beta)


predict(fit_beta, type="response",newdata = data.frame(income=60,persons=4))
exp( -0.621147+  -0.012254*60 +  0.118022*4)/(1+exp( -0.621147+  -0.012254*60 +  0.118022*4))
predict(fit_beta, type="response",newdata = data.frame(income=60,persons=5))
exp( -0.621147+  -0.012254*60 +  0.118022*5)/(1+exp( -0.621147+  -0.012254*60 +  0.118022*5))


AIC(fit_beta)


fit_beta2 <- betareg(I(food/income) ~ income + persons | persons,data = FoodExpenditure)
summary(fit_beta2)
AIC(fit_beta2)


require(mgcv)
fit_beta_gam <- gam(I(food/income) ~ s(income) + s(persons,k=7), family=betar, data = FoodExpenditure)
summary(fit_beta_gam)
AIC(fit_beta_gam)

gam.check(fit_beta_gam)

par(mfrow=c(1,2))
plot(fit_beta_gam)


require(ggplot2)
p <- ggplot(data=FoodExpenditure, aes(x=income, y=I(food/income), size=persons) )+ geom_point(alpha=0.5, col='blue')
p
p+ xlab("income") + ylab("proportion of food expenditure")


