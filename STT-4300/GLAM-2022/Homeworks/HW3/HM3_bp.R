######
#### EXERCISE 1
#####
mysummary <- function(x) {
  return(c(mean(x), var(x)))
}


######
#### EXERCISE 2
#####


# LOAD DATA
cancer <- na.omit(read.table("breast-cancer-wiconsin.txt"))


# CREATE VECTOR X
n <- nrow(cancer)
x <- cbind(rep(1, n), 
           cancer$nuclei)
y <- cancer$status / 2 - 1

# Function to update beta_t
update <- function(beta, x, y) {
  n <- length(y)
  prob <- 1 / (1 + exp(-x %*% beta))
  # score 
  score <- c(0, 0)
  for (i in 1:n) {
    score <- score + (y[i] - prob[i]) * x[i, ]
  }
  # J matrix
  J <- matrix(0, nrow = 2, ncol = 2)
  for (i in 1:n) {
    J <- J + prob[i] * (1 - prob[i]) * x[i, ] %*% t(x[i, ])
  }
  return(beta + solve(J, score))
  
}

# While loop
it_max <- 100
tol    <- 0.0001
it <- 0
beta0 <- c(0, 0)
beta1 <- update(beta0, x, y)
while(max(abs(beta0 - beta1)) > tol & it < it_max) {
  it <- it + 1
  beta0 <- beta1
  beta1 <- update(beta0, x, y)
}
print(beta1)

# Log-likelihood function
log_likelihood <- function(beta, x, y) {
  prob <- 1 / (1 + exp(-x %*% beta))
  return(sum(y * log(prob) + (1 - y) * log(1 - prob)))
}

# Optimization
optim(par = c(0, 0), fn = log_likelihood, x = x, y = y, 
      method = "BFGS",
      control = list(fnscale = -1))

# Using GLM function
glm(I(status / 2 - 1)~ 1 + nuclei, data = cancer, family = binomial(link = "logit"))



######
#### EXERCISE 3
#####

# Load data
goose.data <- read.table("goose.txt", header=TRUE)

# Fit GLM
goose.glm  <- glm( cbind(returned, offered - returned) ~ log(bid), 
                  data = goose.data, 
                  family = binomial(link = "logit"))
summary(goose.glm)


# Log-likelihood of the fit
l <- logLik(goose.glm)

# Cut-off values for profile-likelihood tests.
cut_off_value_5pc <- l - 0.5 * qchisq(1 - 0.05, df = 1)
cut_off_value_1pc <- l - 0.5 * qchisq(1 - 0.01, df = 1)

# confindence intervals using confint : 
confint(goose.glm, "log(bid)", level = 0.95)

# Function to compute profile likelihood
profile_likelihood <- function(beta1) {
  fit_constrained <- glm(cbind(returned, offered - returned) ~ 1 + offset(beta1 * log(bid)), 
                           data = goose.data,
                           family = binomial(link = "logit"))
  logl <- logLik(fit_constrained) 
  return(logl)
}


# Function to compute profile likelihood on a vector of beta1
profile_likelihood_1 <- function(beta1) {
  p <- length(beta1)
  logl <-vector("numeric", p)
  for (i in 1:p) {
   logl[i] <- profile_likelihood(beta1[i])
  }
  return(logl)
}

# list of beta1 to plot
beta1_list <- seq(0.5, 2, length.out = 100)

# plot profile likelihood
plot(beta1_list, profile_likelihood_1(beta1_list), type = "l", 
     xlab = "beta1", 
     ylab = "Profiled likelihood")
# add cut-off values
abline(h = cut_off_value_1pc, col = "red")
abline(h = cut_off_value_5pc, col = "green")

# add confidence intervals from confint function
abline(v = confint(goose.glm, "log(bid)", level = 0.99), col = "red", lty = 2)
abline(v = confint(goose.glm, "log(bid)", level = 0.95), col = "green", lty = 2)

