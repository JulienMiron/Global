rm(list = ls())
require(mgcv)


# load data
temp <- read.table(file='temperature.txt',header=T)
attach(temp)
n <- 56
plot(Long,JanTemp, 
     xlab = "Longitude", ylab = "Latitude")

gam_1 <- gam(JanTemp~s(Long))
points(Long, gam_1$fitted.values, col = "red", pch = 19)

gam.check(temp.spline)
abline(0, 1)



sp_list <- seq(from = 0, to = 2, by = 0.01)
gcv = 0 * sp_list
i = 0
for (sp in sp_list) {
  i = i + 1
  gcv[i] = gam(JanTemp ~ s(Long), data = temp, sp = sp)$gcv.ubre
}


sp_opt <- sp_list[which.min(gcv)]

plot(sp_list, gcv, pch = 19, xlab = "smoothing parameter", ylab = "GCV")
abline(v = sp_opt)
sp_1 = 0.05
sp_2 = 0.5
sp_3 = 1

fit_1 <- gam(JanTemp~s(Long), sp = sp_1)
fit_2 <- gam(JanTemp~s(Long), sp = sp_2)
fit_3 <- gam(JanTemp~s(Long), sp = sp_3)
fit_opt <- gam(JanTemp~s(Long), sp = sp_opt)


x = seq(from = min(Long), to = max(Long), length.out = 200)


plot(Long, JanTemp, pch = 19, ylim = c(0, 80))
points(x, predict(fit_1, newdata = data.frame(Long = x)), 
       col = "red", pch = 19, type ="l", lwd = 2)
points(x, predict(fit_2, newdata = data.frame(Long = x)),
       col = "blue", pch = 19, type ="l", lwd = 2)
points(x, predict(fit_3, newdata = data.frame(Long = x)), 
       col = "forestgreen", pch = 19, type ="l", lwd = 2)
points(x, predict(fit_opt, newdata = data.frame(Long = x)), 
       col = "black", pch = 19, type ="l", lwd = 2)

legend("topright", legend = c("sp = 0.05", "sp = 0.5", "sp = 1", "optimal sp"), 
       col = c("red", "blue", "forestgreen", "black"), lwd = 1, bty = "n")


##### Ex 3

require(MASS)
n <- nrow(mcycle)

plot(mcycle)

# Fit a gam model
mc <- gam(accel ~ s(times, k=20 ), data = mcycle)

# save the smoothing parameter tuned by gam( )
sp_0 <- mc$sp
# plot the fit
points(mcycle$times, mc$fitted.values, col ="red", pch = 19)
# summary
summary(mc)
# check
gam.check(mc)
abline(0,1)

# Compute matriw A.
A = matrix(nrow = n, ncol = n)
for (j in 1:n) {
  mcycle$y = 0
  mcycle$y[j] = 1
  A[, j]  = gam(y ~ s(times, k = 20), sp = sp_0, data = mcycle)$fitted.values
}

A[1:5, 1:5]

rowSums(A)
# If y was equal to a constant c, the fit should give the same constant c as fitted values. 
# This mean that A c = c. That's because there is no penalization on the intercept.




# 

i  = 65

plot(mcycle$times, A[i, ], pch = 19)
abline( h = 0)

for (i in 1:n) {
  plot(mcycle$times, A[i, ], pch = 19)
  abline( h = 0)
}


sp_1 = 5 * sp_0
i  = 65

mcycle$y = 0
mcycle$y[i] = 1

A1_i  = gam(y ~ s(times, k = 20), sp = sp_1, data = mcycle)$fitted.values

sp_2 = sp_0 / 5
A2_i  = gam(y ~ s(times, k = 20), sp = sp_2, data = mcycle)$fitted.values

plot(mcycle$times, A[i, ], pch = 19, "l", ylim = c(-0.02, 0.13))
points(mcycle$times, A1_i, col = "red", pch = 19, "l")
points(mcycle$times, A2_i, col = "blue", pch = 19, "l")
legend("topright", col = c("black", "red", "blue"), legend = c("original sp", "sp 5 times bigger", "sp 5 times smaller"), lwd = 1, bty = "n")
