
require(boot)
require(mgcv)
fit0 <- glm(y ~ year + quarter + time + delay, data = aids, family = poisson())

fit1 <- glm(y ~ time + delay, data = aids, family = poisson())


fit2 <- glm(y ~ time, data = aids, family = poisson())


fit3_bin <- glm(I(y > 0) ~ delay + time, data = aids, family = binomial)
fit3_trunc <- glm(y ~ delay + time, data = aids, family = truncpoisson, subset = (y > 0))


fit_gam <- gam(y ~ s(time) + s(delay, k = 14), data = aids, family = poisson)
fit_gam_quasi <- gam(y ~ s(time) + s(delay), data = aids, family = quasipoisson)


boxplot(log(aids$y)~ aids$delay, xlab = "delay", ylab = "log(y)")
bins = c(0:10 - 0.5, 2:19 * 10 -0.5)
y_new = aids$y
y_new[y_new > 10] <- 10
bins <- 0:11 - 0.5
hist(y_new, breaks = bins, axes = F, col = "grey90", 
     main = "Histogram of y", xlab = "y")
axis(side = 1, at = -1:11, labels = c("", as.character(0:9), " > 10", ""))
axis(side = 2)
plot(y~time,data=aids)