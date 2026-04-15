#################### EXERCISE 1 #################### 

mysummary <- function(x, mean = T, variance = T){
  avg <- mean(x)
  var <- var(x)
  if (mean == T & variance == F){
    output <- list("mean" = avg)
  }
  if (mean == F & variance == T){
    output <- list("variance" = variance)
    
  }
  if (mean == T & variance == T){
    output <- list("mean" = avg, "variance" = variance)
  }
  
  return(output)
}


x <- rnorm(1000)
mysummary(x, mean = T, variance = F)


#################### EXERCISE 2 #################### 

#################### a) #################### 

updatebeta <- function(beta_prev, x, y){
  n <- nrow(x)
  Jbeta <- 0
  Ubeta <- matrix(0, nrow = 2, ncol = 1)
  for(i in 1:n){
    etai <- -x[i, ] %*% beta_prev
    Jbeta <- Jbeta + as.vector(exp(etai) / 
             (1 + exp(etai)) ^ 2) *  (x[i, ] %*% t(x[i, ]))
    Ubeta <- Ubeta + t((y[i] - 1 / (1 + exp(etai))) %*% t(x[i, ]))
  }
  return(beta_prev + solve(Jbeta) %*% Ubeta)
}

# There are more efficient ways to do this, for example, without the for loop.
# Can you find how?

#################### b) #################### 

breast <- read.table("breast-cancer-wiconsin.txt", header=TRUE)
str(breast)
breast$status[which(breast$status == 4)] <- 1
breast$status[which(breast$status == 2)] <- 0
y <- na.omit(breast)$status # responses in the model
x <- cbind(rep(1, length(y)), na.omit(breast)$nuclei) 
#Construct the design matrix for our model

tol <- 1e-8 # we set tol to an arbitrary value
maxit <- 50 # same for maxit
beta0 <- c(0, 0) #initialize the beta, could be set to any values
beta1 <- updatebeta(beta0, x = x, y = y) # do one iteration to initialize beta1
beta_evol <- beta1 # initialize the beta_evol data
it <- 1 # set it to 1, because 1 update was done on line 63

while (max(abs(beta0 - beta1)) > tol & it < maxit){
  it <- it + 1 # at each iteration, it is augmented by 1
  beta0 <- beta1 # beta0 becomes beta1
  beta1 <- updatebeta(beta0, x = x, y = y) # update once
  beta_evol <- cbind(beta_evol, beta1) # add a column to beta_evol
}

beta_newton_raphson <- beta_evol[, it] # [, it] because 'it' is the last iteration

#################### c) #################### 

loglikelihood <- function(beta, x, y){
  n <- nrow(x)
  ll <- 0
  for (i in 1:n){
    etai <- -x[i, ] %*% beta
    ll <- ll + (1 - y[i]) * etai - log(1 + exp(etai))
  }
  return(-ll) # optim minimizes - we need to put a minus
}

#### more efficient way
loglkhd <- function(beta, y, x){
  eta <- as.vector(x %*% beta)
  -sum(y * eta - log(1 + exp(eta))) # minus sign because optim() minimizes by default
}   

#################### d) #################### 

beta_optim <- optim(par = c(0, 0), fn = loglikelihood,
                    method='BFGS', x = x, y = y)$par


beta_optim
beta_newton_raphson
glm(y ~ x[, -1], family = 'binomial')$coefficients
#################### EXERCISE 3 #################### 

#################### a) #################### 

goose.data <- read.table("goose.txt", header=TRUE)
goose.glm <-glm(cbind(returned, offered - returned) ~ log(bid), 
                data = goose.data, family = binomial(link = "logit"))

summary(goose.glm)
logLik(goose.glm)

goose.plci.05 = confint(goose.glm, level=.95) #default
goose.nlci.05 = confint.default(object = goose.glm)

cutoff_5 <- as.numeric(logLik(goose.glm)) - 0.5 * qchisq(p = 0.95, df = 1)
# cutoff_1 <- as.numeric(logLik(goose.glm)) - 0.5 * qchisq(p = 0.99, df = 1)

#################### b) #################### 

grid.beta1 = seq(-2, 2, by = 0.01)  # a grid of values of beta0 
                                      # for computation of PL for beta1

profLik.beta.1 <- vector()

for (i in 1:length(grid.beta1)){
  fit_PL <- glm(cbind(returned, offered - returned) ~ 1 +
                    offset(grid.beta1[i] * log(bid)), 
                      data = goose.data, family = binomial(link = "logit"))
  profLik.beta.1[i] <- as.numeric(logLik(fit_PL))
}


layout(1)
plot(grid.beta1[profLik.beta.1 > cutoff_5 - 2], 
     profLik.beta.1[profLik.beta.1 > cutoff_5 - 2], 
     type = "l", 
     main = "Profile likelihood for Beta 1",
     ylab = "Profile likelihood",
     xlab = "Beta 1")
abline(h = cutoff_5)
lines(x = c(goose.plci.05[2, ][1],
          goose.plci.05[2, ][2]), 
      y = c(cutoff_5, cutoff_5), 
      col = "red", lwd = 5)
text(x = goose.plci.05[2, ][1], 
     y = cutoff_5, 
     labels = as.character(signif(goose.plci.05[2, ][1], 3)), 
     pos =1)
text(x = goose.plci.05[2, ][2], 
     y = cutoff_5, 
     labels = as.character(signif(goose.plci.05[2, ][2], 3)), 
     pos = 1)
lines(x = c(goose.plci.05[2, ][1],
          goose.plci.05[2, ][1]), 
      y = c(-30, cutoff_5),
      col = "red", lty = 2)
lines(x = c(goose.plci.05[2, ][2],
          goose.plci.05[2, ][2]), 
      y = c(-30, cutoff_5),
      col = "red", lty = 2)












