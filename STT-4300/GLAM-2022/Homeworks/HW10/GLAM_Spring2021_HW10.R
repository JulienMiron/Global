
#======================================================= GLAM Spring 2021 HW10 =====================================================

#--- Ex. 1 As in HW09, we are interested in the "temperature.txt" dataset.

# a) Fit a GAM model to describe the behavior of January temperature with respect to the Longitude.

rm(list = ls())
require(mgcv)
temp <- read.table(file='temperature.txt',header=T)
attach(temp)
n <- 56
par(mfrow=c(1,1))
plot(Long,JanTemp)

temp.spline <- gam(JanTemp~s(Long))
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
  GCV[i] <- gam(JanTemp~s(Long), sp=i/fine, data=temp)$gcv.ubre
}
opt <- which.min(GCV)/fine
plot(1:fine/fine, GCV, xlab = "Smoothing Parameter", type = "l", col = "blue")
abline(v=opt, col = "magenta")

# c) Fit three different models with varying smoothing parameter and the optimal one. Plot the corresponding curve along with each other.
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
legend(x = 103, y = 65,legend=c(paste('sp opt = ', opt),'sp = 0.025','sp = 0.2','sp = 0.8'), col=c('magenta','lightblue','deepskyblue','blue'),lwd=2,lty=1,bty='n')

#--- Ex. 2

# See analytical derivation of the projection matrix onto the spline basis.


#--- Ex. 3
#
#          The "mcycle" dataset (packages "MASS") records a series of measurements of head acceleration in a simulated motorcycle accident,
#          used to test crash helmets. The variables are "times" in milliseconds after impact, and "accel" in G. The influence matrix "A" of a
#          linear smoother allows linear representation of the model through \hat{y} = Ay, given the smoothing parameter lambda.


# a) Fit a gam model to capture the dependence of "accel" on "times".

require(MASS)
plot(mcycle)
mc <- gam(accel~s(times, k=20),data=mcycle)
plot(mc,residuals=TRUE,se=FALSE,pch=1)
summary(mc)
gam.check(mc)
abline(1,1)

# b) 
#     Fitting the model to I_j and evaluating the fitted values \hat{\mu}_i, is equivalent to forming \hat{\mu}_i = A*I_j,
#     the jth column of A. Making use of the columns of the identity matrix, find the influence matrix A corresponding to your model model in a).

n <- nrow(mcycle)
A <- matrix(0,n,n)
for (i in 1:n){
  mcycle$y <- mcycle$accel*0
  mcycle$y[i] <- 1
  A[,i] <- fitted(gam(y~s(times, k=20),data=mcycle,sp=mc$sp))
}

# c) What values do the rows of A sum to? How is it related to the intercept of your model?

rowSums(A) # sum up to 1, the model does not penalize the constant, we need b = Ab when b is a vector of constant.

# d) the linear representation of the smoother show that our model replaces each values y_i by a weighted sum of the
#    neighbouring y_i values. Plot the weights used to compute the 65th predicted value, with "times" in abcissa. This is known as
#    the "equivalent kernel".

plot(mcycle$times,A[65,],type="l",ylim=c(-0.05,0.15))
mcycle$times[65]

# e) Plot all the equivalent kernels on the same frame. Why do their peak heights vary?

for (i in 1:n){
  lines(mcycle$times,A[i,]) # vary with the number of data in the neighborhood (fewer data => higher kernel => sum up to 1)
}

# f) Finally, vary the smoothing parameter arround the GCV slected value. What happens to the equivalent kernel for the 65th
#    observation?

par(mfrow=c(2,2))
mcycle$y <- mcycle$accel*0
mcycle$y[65] <- 1
for (k in 1:4){
  plot(mcycle$times,fitted(gam(y~s(times,k=20),data=mcycle,sp=mc$sp*10^(k-1.5))),type="l",ylab="A[65,]",ylim=c(-0.01,0.12))
}
par(mfrow=c(1,1))

# smoothness and width of the kernel.
