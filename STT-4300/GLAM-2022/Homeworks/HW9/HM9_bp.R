library(gamair)
library(robustbase)
library(mgcv)


# Load and plot data
eng <- data(engine)

head(eng)

plot(wear ~ size, data = eng, xlab = "Size",  ylab = "Wear", pch = 19)

# Fit parametric models
# linear model
fit_norm <- glm(wear ~ size, family = gaussian(), data = eng)
points(eng$size, fit_norm$fitted.values, type = "l", col = "red")

# robust version
fit_rob <- lmrob(wear ~ size, family = gaussian(), data = eng)
points(eng$size, fit_rob$fitted.values, type = "l", col = "green")

# Log-normal model
fit_log_norm <- glm(log(wear) ~ size, family = gaussian(), data = eng)
points(eng$size, exp(fit_log_norm$fitted.values), col = "blue", type = "l")



# Non parametric model
# define a grid 

n_knots <- 20 # number of knots on the grid
knots <- seq(from = min(eng$size), 
             to = max(eng$size), 
             length.out = n_knots)
# design matrix with basis function
X <- tf.X(eng$size, knots)

# Fit : 
fit_np <- lm(eng$wear ~ X - 1)

# plot the non parametric fit
plot(eng$size, eng$wear, pch = 19, 
     xlab = "Size", ylab = "Wear")
points(eng$size, fit_np$fitted.values, type = "b", col = "red")


# Play a little with n_knots... optimal value seems to be 4 or 5 ...

# Exercise 2
# Make data
set.seed(1)
x <- sort(runif(40)*10)^0.5
y <- sort(runif(40))^0.1

# Plot it
plot(x, y, pch = 19)

# order 5 polynomial fit 

# Make the design matrix with columns x^i
X_5 <- rep(1, 40)
for (i in 1:5) {
  X_5 <- cbind(X_5, x^i)
}

# Fit by hand :
beta_5  <- solve(t(X_5)  %*% X_5) %*% (t(X_5)  %*% y)


# Fit with lm( )
# First turn data into a data.frame
# turn data into data.frame (not necessary fot the fit)
X_5_df  <- data.frame(cbind(y, X_5))
colnames(X_5_df) <- c("y", paste("x", 0:5, sep = ""))
# fit
fit_5  <- lm(y ~ x1 + x2 + x3 + x4 + x5, data = X_5_df)

# Compare both fits
beta_5
fit_5$coefficients # perfect!

# Plot the fit
plot(x, y, pch = 19) 
points(x, predict(fit_5), type = "b", lty = 3, col = "red")


# order 10 polynomial fit
# Let's do the same
X_10 <- rep(1, length(x))

for (i in 1:10) {
  X_10 <- cbind(X_10, x^i)
}

# Fit by hand
beta_10 <- solve(t(X_10) %*% X_10)%*% (t(X_10) %*% y) # error !
 
# Fit by using lm( )
X_10_df <- data.frame(cbind(y, X_10))
colnames(X_10_df) <- c("y", paste("x", 0:10, sep = ""))
fit_10 <- lm(y ~ . - 1, data = X_10_df)

# Plot fit:
plot(x, y, pch = 19)
points(x, predict(fit_10), col = "blue", type = "b", lty = 3)


# To avoid the error for the manual fit, let's use poly()

X_10_poly <- poly(x, degree = 10)
# fit manually
beta_10_poly <- solve(t(X_10_poly)%*% X_10_poly) %*% (t(X_10_poly) %*% y) # no error
# fit with lm( )
X_10_df <- data.frame(cbind(y, rep(1, length(x)), X_10_poly))
colnames(X_10_df) <- c("y", paste("x", 0:10, sep = ""))
fit_10_poly <- lm(y ~. -1, data = X_10_df)

# compare fits
beta_10_poly
fit_10_poly$coefficients # it's the same

# Although the design matrix is different, fit is the same as before.
plot(x, y, pch = 19)
points(x, predict(fit_10_poly), type = "b", lty = 3, col = "blue")







# These polynomial fits seem good, but ...
# Let's plot the fits not only on the x-data points

# Make a grid from 0 to 4
grid <- seq(from = 0, 
             to = 4, 
             by = 0.05)

# Make design matrices associated with this grid
grid_5 <- rep(1, length(grid))
for (i in 1:5) {
  grid_5 <- cbind(grid_5, grid ^ i)
}

# Transform it into a data frame in order to use predict( )
grid_5_df <- data.frame(grid_5)
colnames(grid_5_df) <- paste("x", 0:5, sep = "")

# plot the prediction
plot(x, y, pch = 19, xlim = c(0, 4))
points(grid, 
       predict(fit_5, newdata = grid_5_df), 
       type = "l", 
       col = "red")  # not super for extrapolation

# Do the same for the 10-th order polynomial fit
grid_10 <- poly(grid, degree = 10, coefs = attr(poly(x, degree = 10), "coefs"))


grid_10_df = data.frame(cbind(rep(1, length(x)), grid_10))
colnames(grid_10_df) = paste("x", 0:10, sep = "")

points(grid, predict(fit_10_poly, newdata = grid_10_df), type = "l", col = "blue")


# If too complicated, it can also be done by hand, using the fit without poly()

beta_10 <- fit_10$coefficients
points(xgrid, 
       beta_10[1] + 
         beta_10[2]  * xgrid + 
         beta_10[3]  * xgrid ^ 2 + 
         beta_10[4]  * xgrid ^ 3 + 
         beta_10[5]  * xgrid ^ 4 + 
         beta_10[6]  * xgrid ^ 5 + 
         beta_10[7]  * xgrid ^ 6 + 
         beta_10[8]  * xgrid ^ 7 + 
         beta_10[9]  * xgrid ^ 8 + 
         beta_10[10] * xgrid ^ 9 + 
         beta_10[11] * xgrid ^ 10, 
       type = "l", col = "deepskyblue")

# Anyway, these fits aren't smooth enough. Let's use splines instead


n_knots <- 3
knots <- seq(from = min(x), 
             to = max(x), 
             length.out = n_knots)

splines <- cub_X(x = x, xj = knots)
i <- 10
plot(x, splines[, i])
abline(v = knots[i - 2])
splines_df = data.frame(cbind(y, splines))
colnames(splines_df) <- c("y", paste("s", 1:(n_knots+2), sep = ""))



fit_splines <- lm(y~ . -1, data = splines_df)
plot(x, y, pch = 19, xlim = c(0, 4), ylim = c(0.65, 1.05))
points(x, fit_splines$fitted.values, type = "b", col = "forestgreen")

# plot on a finer grid
grid_s_df <- data.frame(cub_X(grid, knots))
colnames(grid_s_df) <- paste("s", 1:(n_knots + 2), sep ="")

points(grid, predict(fit_splines, newdata = grid_s_df), type = "l", col = "forestgreen")

# Play a little with n_knots : 3 or 4 knots seem to be good


# Let's make a nice final plot

plot(x, y, pch = 19, xlim = c(0, 4), ylim = c(0.6, 1.2))
points(grid, predict(fit_10_poly, newdata = grid_10_df), type = "l", col = "blue")
points(grid, predict(fit_5, newdata = grid_5_df), type = "l", col = "red")
points(grid, predict(fit_splines, newdata = grid_s_df), type = "l", col = "forestgreen")
legend("topleft", lwd = 1, legend = c("order 5 polynomial", "order 10 polynomial", "splines"), col = c("red", "blue", "forestgreen"), bty = "n")


# Exercise 3

temp <- read.table("C:/Users/Benjamin/Dropbox/GLAM-2022/Homeworks/HW9/temperature.txt", 
                   header = TRUE)
head(temp)

plot(JanTemp ~ Lat, data = temp)
plot(JanTemp ~ Long, data = temp)


fit_gam <- gam(JanTemp ~ s(Lat, sp = 1000) + s(Long), data = temp)
plot(fit_gam)
