#####
# GLAM Spring 2019 HW 03 #####
#####



rm(list=ls())

### setwd("...")
 
# Exercise 1 ####

# we create this function using already implemented methods like mean(), var(), range()

mysummary     <- function(x){
  res         <- list(mean(x),var(x),range(x))
  names(res)  <- c('mean of x','variance of x','min and max of x')
  return(res)
}

x <- rnorm(100)

my.sum <- mysummary(x)

my.sum

my.sum[[1]]

my.sum$'mean of x'
my.sum$'variance of x'
my.sum$'min and max of x'

mysummary(x)$'mean of x'

mysummary(x)[[2]]

#### Exercise 2 ####

## a)

updatebeta <- function(beta,y,X){
  eta <- as.vector(-X%*%beta)
  Ubeta <- t(X)%*%(y-exp(eta)/(1+exp(eta)))
  Jbeta <- t(X)%*%diag(exp(eta)/(1+exp(eta))^2)%*%X
  beta+solve(Jbeta,Ubeta)
}

## b)

breast <- read.table("breast-cancer-wiconsin.txt", header=TRUE)
str(breast)
breast$status[which(breast$status==4)] <- 1
breast$status[which(breast$status==2)] <- 0
# write.table(breast, file="breast-cancer-wiconsin-recoded.txt")

y <- na.omit(breast)$status
# y[which(y==2)] <- 0
# y[which(y==4)] <- 1
table(y)

X <- cbind(rep(1,length(y)),na.omit(breast)$nuclei) #Construct the \emph{design} matrix for our model

head(X)

# a first iteration by hand
bet <- c(0,0) # initial values for \beta
bet <- updatebeta(beta=bet,y=y,X=X)  
bet

# using a while loop
tol <- 1e-8           # Tolerance level 
maxit <- 50           # Maximum number of iterations
beta1 <- c(0,0)       # initial value for beta
beta0 <- beta1+tol+1  # initial value for beta1 (for the discrepancy criterion of convergence)
beta.evol <- beta1    # cumulates \beta values for all iterations
it <- 0
while (max(abs(beta0-beta1))>tol & it<maxit){
  beta0 <- beta1
  beta1 <- updatebeta(beta=beta0,y=y,X=X)
  beta.evol <- cbind(beta.evol,beta1)
  it <- it+1
}
dimnames(beta.evol)[[2]] <- 0:it
beta.evol

# c)

loglkhd <- function(beta,y,X){
  eta <- as.vector(X%*%beta)
  -sum(y*eta-log(1+exp(eta))) # minus sign because optim() minimizes by default
}   

# d)

optim(par=c(0,0),fn=loglikelihood,method='BFGS',x = x, y = y)

#### Exercise 3 ####

goose.data<-read.table("goose.txt", header=TRUE)
goose.glm <-glm(cbind(returned,offered-returned)~log(bid), 
                data=goose.data, family=binomial(link="logit"))

summary(goose.glm)

# With function confint() -----

goose.plci.05 = confint(goose.glm, level=.95) #default
goose.nlci.05 = confint.default(object=goose.glm)
goose.plci.01 = confint(goose.glm, level=.99)
goose.nlci.01 = confint.default(object=goose.glm,level=0.99)

# Retrieve the values of the parameter estimates ------

goose.beta=coef(goose.glm)
beta.0 = as.numeric(goose.beta[1])
beta.1 = as.numeric(goose.beta[2])

beta.0
beta.1

cutoff.rule.05=as.numeric(logLik(goose.glm)-qchisq(p=0.95, df=1, lower.tail=TRUE)/2) 
cutoff.rule.01=as.numeric(logLik(goose.glm)-qchisq(p=0.99, df=1, lower.tail=TRUE)/2)

# Grid for the possible values of the parameters (very wide, on purpose) -----

grid.beta0 = seq(-10,10, by=0.01)  #a grid of values of beta0 for computation of PL for beta1
grid.beta1 = seq(-10,10, by=0.01) #a grid of values of beta0 for computation of PL for beta1

profLik.beta.0=rep(0,times=length(grid.beta0))
profLik.beta.1=rep(0,times=length(grid.beta1))

for(i in 1:length(grid.beta1)){ # We use a grid of values  logLik of beta0 a grid of beta1
  profLik.beta.1[i]=as.numeric(logLik(glm(cbind(returned,offered-returned)~1+offset(grid.beta1[i]*log(bid)), 
# ECR: modfied offset() call.
                        data=goose.data, family=binomial(link="logit"))))
}

for(i in 1:length(grid.beta0)){ # We compute the profile logLik of beta1 a grid of beta0
  profLik.beta.0[i]=as.numeric(logLik(glm(cbind(returned,offered-returned)~+log(bid)-1+offset(grid.beta0[i]*rep(1,11)), 
# ECR: modfied offset() call.
                        data=goose.data, family=binomial(link="logit"))))
}

# We plot the PL for beta0 and beta1 

# PLCI for beta 0 
par(mfrow=c(1,2))
plot(grid.beta0[profLik.beta.0>cutoff.rule.05-2],
     profLik.beta.0[profLik.beta.0>cutoff.rule.05-2], type="l",
     main="PL CI Intercept", xlab="beta.0", ylab="Profile Likelihood")
abline(h=cutoff.rule.05)
min(grid.beta0[profLik.beta.0>cutoff.rule.05])
max(grid.beta0[profLik.beta.0>cutoff.rule.05])

lines(x=c(goose.plci.05[1,][1],goose.plci.05[1,][2]), y=c(cutoff.rule.05,cutoff.rule.05), col="red", lwd=5)
lines(x=c(goose.plci.05[1,][1],goose.plci.05[1,][1]), y=c(-30,cutoff.rule.05),col="red", lty=2)
lines(x=c(goose.plci.05[1,][2],goose.plci.05[1,][2]), y=c(-30,cutoff.rule.05),col="red", lty=2)
text(x=goose.plci.05[1,][1], y=cutoff.rule.05, labels=as.character(signif(goose.plci.05[1,][1], 3) ), pos =1)
text(x=goose.plci.05[1,][2], y=cutoff.rule.05, labels=as.character(signif(goose.plci.05[1,][2], 3)), pos =1)

abline(h=cutoff.rule.01)
min(grid.beta1[profLik.beta.1>cutoff.rule.01])
max(grid.beta1[profLik.beta.1>cutoff.rule.01])

lines(x=c(goose.plci.01[1,][1],goose.plci.01[1,][2]), y=c(cutoff.rule.01,cutoff.rule.01), col="blue", lwd=5)
lines(x=c(goose.plci.01[1,][1],goose.plci.01[1,][1]), y=c(-30,cutoff.rule.01),col="blue", lty=2)
lines(x=c(goose.plci.01[1,][2],goose.plci.01[1,][2]), y=c(-30,cutoff.rule.01),col="blue", lty=2)
text(x=goose.plci.01[1,][1], y=cutoff.rule.01, labels=as.character(signif(goose.plci.01[1,][1], 3) ), pos =1)
text(x=goose.plci.01[1,][2], y=cutoff.rule.01, labels=as.character(signif(goose.plci.01[1,][2], 3)), pos =1)

# PLCI for beta 1 
plot(grid.beta1[profLik.beta.1>cutoff.rule.05-2],
     profLik.beta.1[profLik.beta.1>cutoff.rule.05-2], type="l",
     xlab="beta.1", ylab="Profile Likelihood", main="PL CI log(bid)")
abline(h=cutoff.rule.05)
lines(x=c(goose.plci.05[2,][1],goose.plci.05[2,][2]), y=c(cutoff.rule.05,cutoff.rule.05), col="red", lwd=5)
text(x=goose.plci.05[2,][1], y=cutoff.rule.05, labels=as.character(signif(goose.plci.05[2,][1], 3) ), pos =1)
text(x=goose.plci.05[2,][2], y=cutoff.rule.05, labels=as.character(signif(goose.plci.05[2,][2], 3)), pos =1)
lines(x=c(goose.plci.05[2,][1],goose.plci.05[2,][1]), y=c(-30,cutoff.rule.05),col="red", lty=2)
lines(x=c(goose.plci.05[2,][2],goose.plci.05[2,][2]), y=c(-30,cutoff.rule.05),col="red", lty=2)

abline(h=cutoff.rule.01)
lines(x=c(goose.plci.01[2,][1],goose.plci.01[2,][2]), y=c(cutoff.rule.01,cutoff.rule.01), col="blue", lwd=5)
text(x=goose.plci.01[2,][1], y=cutoff.rule.01, labels=as.character(signif(goose.plci.01[2,][1], 3) ), pos =1)
text(x=goose.plci.01[2,][2], y=cutoff.rule.01, labels=as.character(signif(goose.plci.01[2,][2], 3)), pos =1)
lines(x=c(goose.plci.01[2,][1],goose.plci.01[2,][1]), y=c(-30,cutoff.rule.01),col="blue", lty=2)
lines(x=c(goose.plci.01[2,][2],goose.plci.01[2,][2]), y=c(-30,cutoff.rule.01),col="blue", lty=2)
legend("topright" ,legend=c("0.05", "0.01") , lty=c(1,1), col=c("red", "blue"))

rbind(
  Intercept.plci.95 = goose.plci.05[1,], 
  Intercept.manl.95 = c( 
    min(grid.beta0[profLik.beta.0>cutoff.rule.05]),
    max(grid.beta0[profLik.beta.0>cutoff.rule.05]))
)


rbind(
  Intercept.plci.99 = goose.plci.01[1,], 
  Intercept.manl.99 = c(
    min(grid.beta0[profLik.beta.0>cutoff.rule.01]), 
    max(grid.beta0[profLik.beta.0>cutoff.rule.01]))
)

rbind(
  log_bid.plci.95 = goose.plci.05[2,], 
  log_bid.manl.95 = c(
    min(grid.beta1[profLik.beta.1>cutoff.rule.05]), 
    max(grid.beta1[profLik.beta.1>cutoff.rule.05]))
)

rbind(
  log_bid.plci.99 = goose.plci.01[2,], 
  log_bid.manl.99 = c(
    min(grid.beta1[profLik.beta.1>cutoff.rule.01]),
    max(grid.beta1[profLik.beta.1>cutoff.rule.01]))
)

