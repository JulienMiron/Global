#======================================================= GLAM Spring 2019 HW10 =====================================================

# a) Fit a GAM model to describe the behavior of January temperature with respect to the Longitude.

rm(list = ls())
require(mgcv)
temp <- read.table(file='temperature.txt',header=T)
attach(temp)
n <- 56
par(mfrow=c(1,1))
plot(Long,JanTemp)
hist(temp[,2])
temp.spline <- gam(JanTemp~s(Long))
par(mfrow=c(2,2))
gam.check(temp.spline)
abline(0,1)
par(mfrow=c(1,1))
plot(x = Long, y = JanTemp, ylab = paste("s(long,",round(sum(temp.spline$edf)-1,digits = 2),")"))
x <- seq(from = min(Long), to = max(Long), by = 1e-2)
lines(x = x, y = predict(temp.spline, newdata = data.frame(Long = x)))
# temp.spline$sp
# plot(temp.spline,residuals=TRUE,se=FALSE,pch=1)


# b) Code a cross-validation procedure to select the best bandwidth parameter, according to the GCV criterion. Then plot the
#    curve of the criterion vs. the value of the parameter.

fine <- 100
GCV <- 1:fine
for (i in 1:fine){
  GCV[i] <- gam(JanTemp~s(Long), sp=i/fine, data=temp)$gcv.ubre  # gcv
}
opt <- which.min(GCV)/fine
plot(1:fine/fine, GCV, xlab = "Smoothing Parameter", type = "l", col = "blue")
abline(v=opt, col = "magenta")

# b) Fit three different models with varying smoothing parameter and the optimal one. Plot the corresponding curve along with each other.
#    Does the optimal smoothing parameter yield a sensibly better curve than the other values?

temp.spline0.025 <- gam(JanTemp~s(Long),sp=0.025)
temp.spline0.2 <- gam(JanTemp~s(Long),sp=0.2)
temp.spline0.8 <- gam(JanTemp~s(Long),sp=0.8)
temp.spline.optim <- gam(JanTemp~s(Long),sp=opt)

plot(x = Long, y = JanTemp)
lines(x = x, y = predict(temp.spline0.025, newdata = data.frame(Long = x)), col = "lightblue")
lines(x = x, y = predict(temp.spline0.2, newdata = data.frame(Long = x)), col = "deepskyblue")
lines(x = x, y = predict(temp.spline0.8, newdata = data.frame(Long = x)), col = "blue")
lines(x = x, y = predict(temp.spline.optim, newdata = data.frame(Long = x)), col = "magenta")
legend(x = 103, y = 65,legend=c(paste('sp = ', opt),'sp = 0.025','sp = 0.2','sp = 0.8'), col=c('magenta','lightblue','deepskyblue','blue'),lwd=2,lty=1,bty='n')


