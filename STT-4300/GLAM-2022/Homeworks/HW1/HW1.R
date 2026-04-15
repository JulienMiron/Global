?read.table
help(read.table)

bost <- read.table("BostonHousing.txt", header = TRUE)
View(bost)

cancer <- read.table("breast-cancer-wiconsin.txt", header = TRUE)
View(cancer)

cardia <- read.table("CARDIA.dat", header = TRUE)
View(cardia)

summary(bost)
boxplot(bost)
plot(bost[, c("crim", "indus", "medv")], col = 1 + bost$chas, pch = 19)

lin_fit1 <- lm(log(medv) ~ .,
              data = bost)
hist(lin_fit1$residuals)
plot(lin_fit1$fitted.values, bost$medv)
MSE1 <- mean(lin_fit1$residuals^2)


lin_fit2 <- lm(log(medv) ~ chas + crim + zn + indus + 
               I(nox^2) + I(rm^2) + age + log(dis) + 
               log(rad) + tax + ptratio + b + log(lstat),
              data = bost)

hist(lin_fit2$residuals)
plot(lin_fit2$fitted.values, bost$medv)
MSE1 <- mean(lin_fit2$residuals^2)
plot(lin_fit2$fitted.values, bost$medv)
