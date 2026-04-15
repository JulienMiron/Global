################################################################################
###                                                                          ###
###                              Statistics I                                ###
###                               Practical 5                                ###
###                               Fall 2015                                  ###
###                                                                          ###
################################################################################

# ---------------------------------------------------
# Prof. Eva Cantoni
# Fall 2015
# ---------------------------------------------------

par(mfrow = c(1,2))

s.size = 100

# standard normal sample
s1  = rnorm(n = s.size) 
# shift in mean
s2  = rnorm(n = s.size, mean = 1.5)  
# shift in mean and sigma2
s3  = rnorm(n = s.size, mean  = 1.5, sd = 2)
# t distribution with fat tails
s4  = rt(n = s.size, df = 3)
# right-skewed distribution 
s5  = rexp(n = s.size, rate = 1)  
# left-skewed distribution 
s6  = 1-rexp(n = s.size)  
# bimodal distribution 
mix = rbinom(n = s.size, size = 1, prob = 0.5)
s7  = rnorm(n = s.size)*mix + rnorm(n = s.size, mean = 6)*(1-mix)  

minmax.x.s1 = range(s1, s1)
minmax.x.s2 = range(s1, s2)
minmax.x.s3 = range(s1, s3)
minmax.x.s4 = range(s1, s4)
minmax.x.s5 = range(s1, s5)
minmax.x.s6 = range(s1, s6)
minmax.x.s7 = range(s1, s7)

ds1  = density(s1)
ds2  = density(s2)
ds3  = density(s3)
ds4  = density(s4)
ds5  = density(s5)
ds6  = density(s6)
ds7  = density(s7)

xs1  = seq(from = minmax.x.s1[1], to = minmax.x.s1[2], by = 0.01)
xs2  = seq(from = minmax.x.s2[1], to = minmax.x.s2[2], by = 0.01)
xs3  = seq(from = minmax.x.s3[1], to = minmax.x.s3[2], by = 0.01)
xs4  = seq(from = minmax.x.s4[1], to = minmax.x.s4[2], by = 0.01)
xs5  = seq(from = minmax.x.s5[1], to = minmax.x.s5[2], by = 0.01)
xs6  = seq(from = minmax.x.s6[1], to = minmax.x.s6[2], by = 0.01)
xs7  = seq(from = minmax.x.s7[1], to = minmax.x.s7[2], by = 0.01)

ys1  = dnorm(xs1)
ys2  = dnorm(xs2)
ys3  = dnorm(xs3)
ys4  = dnorm(xs4)
ys5  = dnorm(xs5)
ys6  = dnorm(xs6)
ys7  = dnorm(xs7)

minmax.y.s1 = range(ys1, ds1$y)
minmax.y.s2 = range(ys2, ds2$y)
minmax.y.s3 = range(ys3, ds3$y)
minmax.y.s4 = range(ys4, ds4$y)
minmax.y.s5 = range(ys5, ds5$y)
minmax.y.s6 = range(ys6, ds6$y)
minmax.y.s7 = range(ys7, ds7$y)

# standard normal sample
par(mfrow=c(1,1))
plot(minmax.x.s1, minmax.y.s1, main = "Kernel Density", type = "n", 
     xlab = "x", ylab = "density")
lines(x = ds1$x , y= ds1$y)          # Simulation d'une normale standard 
lines(x = xs1, y = ys1, col ="red")  # Evaluation d'une normale standard 
legend("topright", 
       lty=c(1,1), legend = c("sim","stdNorm"), col=c("black", "red"), 
       cex = .75)
qqnorm(s1)
qqline(s1, lwd = 2)
abline(a=0, b=1, col = "red", lwd = 2)
legend("bottomright", 
       lty=c(1,1), legend = c("qqline","abline"), col=c("black", "red"), 
       cex = .75)


# normal sample with mean = 1.5 , var = 1 (sd = 1)
par(mfrow=c(1,1))
plot(minmax.x.s2, minmax.y.s2, main = "Kernel Density", type = "n", 
     xlab = "x", ylab = "density")
lines(x = ds2$x , y= ds2$y)
lines(x = xs2, y = ys2, col ="red")
legend("topright", 
       lty=c(1,1), legend = c("sim","stdNorm"), col=c("black", "red"), 
       cex = .75)
qqnorm(s2)
qqline(s2, lwd = 2)
abline(a=0, b=1, col = "red", lwd = 2)
legend("bottomright", 
       lty=c(1,1), legend = c("qqline","abline"), col=c("black", "red"), 
       cex = .75)


## Normal Sample mean = 1.5, var=4 (sd=2)
plot(minmax.x.s3, minmax.y.s3, main = "Kernel Density", type = "n", 
     xlab = "x", ylab = "density")
lines(x = ds3$x , y= ds3$y)
lines(x = xs3, y = ys3, col ="red")
legend("topright", 
       lty=c(1,1), legend = c("sim","stdNorm"), col=c("black", "red"), 
       cex = .75)
qqnorm(s3)
qqline(s3, lwd = 2)
abline(a=0, b=1, col = "red", lwd = 2)
legend("bottomright", 
       lty=c(1,1), legend = c("qqline","abline"), col=c("black", "red"), 
       cex = .75)


# Fat tails : Student df = 3 
plot(minmax.x.s4, minmax.y.s4, main = "Kernel Density", type = "n", 
     xlab = "x", ylab = "density")
lines(x = ds4$x , y= ds4$y)
lines(x = xs4, y = ys4, col ="red")
legend("topleft", 
       lty=c(1,1), legend = c("sim","stdNorm"), col=c("black", "red"), 
       cex = .75)
qqnorm(s4)
qqline(s4, lwd = 2)
abline(a=0, b=1, col = "red", lwd = 2)
legend("bottomright", 
       lty=c(1,1), legend = c("qqline","abline"), col=c("black", "red"), 
       cex = .75)


# Right Skewness
plot(minmax.x.s5, minmax.y.s5, main = "Kernel Density", type = "n", 
     xlab = "x", ylab = "density")
lines(x = ds5$x , y= ds5$y)
lines(x = xs5, y = ys5, col ="red")
legend("topright", 
       lty=c(1,1), legend = c("sim","stdNorm"), col=c("black", "red"), 
       cex = .75)
qqnorm(s5)
qqline(s5, lwd = 2)
abline(a=0, b=1, col = "red", lwd = 2)
legend("topleft", 
       lty=c(1,1), legend = c("qqline","abline"), col=c("black", "red"), 
       cex = .75)

# Left Skewness
plot(minmax.x.s6, minmax.y.s6, main = "Kernel Density", type = "n", 
     xlab = "x", ylab = "density")
lines(x = ds6$x , y= ds6$y)
lines(x = xs6, y = ys6, col ="red")
legend("topleft", 
       lty=c(1,1), legend = c("sim","stdNorm"), col=c("black", "red"), 
       cex = .75)
qqnorm(s6)
qqline(s6, lwd = 2)
abline(a=0, b=1, col = "red", lwd = 2)
legend("bottomright", 
       lty=c(1,1), legend = c("qqline","abline"), col=c("black", "red"), 
       cex = .75)

# Bimodality
plot(minmax.x.s7, minmax.y.s7, main = "Kernel Density", type = "n", 
     xlab = "x", ylab = "density")
lines(x = ds7$x , y= ds7$y)
lines(x = xs7, y = ys7, col ="red")
legend("topright", 
       lty=c(1,1), legend = c("sim","stdNorm"), col=c("black", "red"), 
       cex = .75)
qqnorm(s7)
qqline(s7, lwd = 2)
abline(a=0, b=1, col = "red", lwd = 2)
legend("topleft", 
       lty=c(1,1), legend = c("qqline","abline"), col=c("black", "red"), 
       cex = .75)

###########

# Right Skewness

s = rexp(100) #sample coming from an exponential distruibution
head(s)

p = seq(0.0, 0.95 , by = 0.05 )
q.norm = qnorm(p)
q.smpl = quantile(x = s, probs = p)

qqnorm.data = data.frame(cbind(p, q.norm, q.smpl))
qqnorm.data

ds.s = density(s)
ns.s = seq(min(-3,s), max(q.norm, s), by  = 0.01)
dn.s = dnorm(ns.s) 

par(mfrow=c(1,2))
plot(x = range(-3, ds.s$x) ,  y= range(dn.s, ds.s$y), type = "n")
lines(ns.s, dn.s, col = "red", lwd = 3)
lines(ds.s, col = "blue", lwd = 3)
legend("topright",legend = c("std normal","sample"), lwd = c(3,3), col = c("red", "blue"), cex = 0.5)
plot(q.norm , q.smpl)
qqline(s)

p.to.plot = 0.35
xvals.norm = seq(-3,qnorm(p.to.plot),length=50)
yvals.norm = dnorm(xvals.norm)
# xvals.smpl = seq(min(ds.s$x, quantile(x = s, probs = p.to.plot), length=50)
# yvals.smpl = seq(min(ds.s$y),ds.s$y[which(ds.s$x==quantile(x = s, probs = p.to.plot))])
par(mfrow=c(1,2))
plot(x = range(-3, ds.s$x) ,  y= range(dn.s, ds.s$y), type = "n")
lines(ns.s, dn.s, col = "red", lwd = 3)
polygon(c(xvals.norm,rev(xvals.norm)),c(rep(0,50),rev(yvals.norm)),col="red")
lines(ds.s, col = "blue", lwd = 3)

legend("topright",legend = c("std normal","sample"), lwd = c(3,3), col = c("red", "blue"), cex = 0.5)
plot(q.norm , q.smpl)
qqline(s)




