###################################################
## The problem of the Deviance for Bernoulli GLM ##
###################################################

start.bin <- Sys.time()
N.samples <- 10000
obs.dev <- rep(0,N.samples)
for(k in 1 : N.samples){
  n<-100;
  m<-1
  # n<-1000; # see that it gets further away!
  # m<-1
  x <- runif(n)
  lp <- -4 + .005*x #xtb = -1 + 3x 
  mu <- binomial()$linkinv(lp)
  y <- rbinom(1:n,m,mu)
  fit.bern <- glm(y~x,family=binomial(link="logit"))
#   fit.bern <- glm(y/m~x,family=binomial,weights=rep(m,n))
  obs.dev[k] <- fit.bern$deviance
}
elapsed <- Sys.time()-start.bin
elapsed
hist(obs.dev, freq = FALSE)
lines(density(obs.dev))
lines(x = 1:max(obs.dev), y = dchisq(1:max(obs.dev), df =  fit.bern$df.residual), col="red")
summary(mu)

N.samples <- 10000
obs.dev <- rep(0,N.samples)
for(k in 1 : N.samples){
  n<-10 
  m<-10
#   n<-10 # try 100, 1000 and see that it stays pretty close!
#   m<-100
  x <- runif(n)
  lp <- -1 + 3*x
  mu <- binomial()$linkinv(lp)
  y <- rbinom(1:n,m,mu)
  fit.bin <- glm(cbind(y,m-y)~x,family=binomial)
  obs.dev[k] <- fit.bin$deviance
}
hist(obs.dev, freq = FALSE)
lines(density(obs.dev))
lines(x = 1:max(obs.dev), y = dchisq(1:max(obs.dev), df =  fit.bin$df.residual), col="red")
