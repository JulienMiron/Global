rm(list=ls())

library(MASS)
library(vcd)
data(quine) 
fit <- goodfit(quine$Days) 
summary(fit) 
rootogram(fit)

Ord_plot(quine$Days)
distplot(quine$Days, type="poisson")

mod1 <- glm(Days~Age+Sex, data=quine, family="poisson")
summary(mod1)
anova(mod1, test="Chisq")

library(AER)
deviance(mod1)/mod1$df.residual
dispersiontest(mod1)

res <- residuals(mod1, type="deviance")
plot(log(predict(mod1)), res)
abline(h=0, lty=2)
qqnorm(res)
qqline(res)

plot(Days~Age, data=quine) 
prs  <- predict(mod1, type="response", se.fit=TRUE)
pris <- data.frame("pest"=prs[[1]], "lwr"=prs[[1]]-prs[[2]], "upr"=prs[[1]]+prs[[2]])
points(pris$pest ~ quine$Age, col="red")
points(pris$lwr  ~ quine$Age, col="pink", pch=19)
points(pris$upr  ~ quine$Age, col="pink", pch=19)