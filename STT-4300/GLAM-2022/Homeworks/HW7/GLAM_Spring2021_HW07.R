#### GLAM Spring 2021 HW 07 ####

rm(list=ls())

setwd("../datasets/") # set your working directory containing the data files

library(gee)
library(MASS)

# packages for GLMM analysis

library(nlme)
library(glmmML)
library(lme4)

##### Exercise 1 #####

# --
# Outcome
# --

# y    :  presence or absence: a factor with levels n and y

# --
# Covariates
# --

# trt  :  a factor with levels placebo, drug and drug+, a re-coding of ap and hilo.
# ap   :  active/placebo. a factor with levels a and p.
# hilo :  hi/low compliance. a factor with levels hi amd lo.
# week :  numeric: week of test.

# --
# Grouping identificator
# --

# ID   : subject ID. a factor.

# N = 220 

data(bacteria)
head(bacteria)

par(mfrow=c(1,2))
plot(y~trt, data=bacteria, main="treatment")
plot(y~week, data=bacteria, main="week")
par(mfrow=c(1,1))


# --- a. Suggest models and estimate its parameters ----

# ---
# PQL
# ---

help(glmmPQL) # Penalized Quasi-Likelihood

bacteria.glmmPQL1 <- glmmPQL(y~trt+week,
                             random=~week|ID,family=binomial,data=bacteria) 
summary(bacteria.glmmPQL1)
# => treatment doesn't seem  to be effective
plot(bacteria.glmmPQL1)

bacteria.glmmPQL2 <- glmmPQL(y~trt+I(week>2),
                             random=~1|ID,family=binomial,data=bacteria) 
# week as dummy for >2
summary(bacteria.glmmPQL2)
# => treatment doesn't seem  to be effective

plot(bacteria.glmmPQL2)   # standardized residuals against fitted

# str(bacteria.glmmPQL2)

which(residuals(bacteria.glmmPQL2, type="normalized")<(-4)) # 33

head(bacteria.glmmPQL2$fitted) 
# two types of predictions: fixed effect only and full
head(fitted(bacteria.glmmPQL2)) # including random effects
 
qqnorm(y = bacteria.glmmPQL2,form = ~ranef(.,level=1), abline=c(0,1))   
# plotting random effects => Not necessarily useful

# ---
# Gauss Hermite
# ---

help(glmmML) # Gauss-Hermite Quadrature approximation of the likelihood.
bacteria.glmmML<-glmmML(y~trt+I(week>2),
                        cluster=ID,
                        family=binomial,data=bacteria)
summary(bacteria.glmmML)

bacteria.glmmML$posterior.modes

# estimated random effects for the 50 clusters
# no pre-programmed plot is available for glmmML
# the function resid is not available either

# => manually calculate the standardized Pearson residuals
# we need first to compute the "full" vector of random effects, 
# i.e. replicating the posterior modes for every measure per subject

rand.eff <- c()
rand.eff.freq <- matrix(table(bacteria$ID),ncol=1)
for (i in 1:length(bacteria.glmmML$posterior.modes)){
    rand.eff <- c(rand.eff,c(rep(bacteria.glmmML$posterior.modes[i],rand.eff.freq[i])))
}
table(rand.eff)

# building full predictions manually using inverse link
X.bacteria <- model.matrix(y~trt+I(week>2),data=bacteria)
Xbeta <- X.bacteria%*%bacteria.glmmML$coefficients
bacteria.fitted.glmmML <- exp(Xbeta+rand.eff)/(1+exp(Xbeta+rand.eff))

bacteriay <- as.numeric(bacteria$y)
bacteriay[which(bacteriay==1)] <- 0
bacteriay[which(bacteriay==2)] <- 1
plot(bacteria.fitted.glmmML,bacteriay)

bacteria.resid.glmmML <- (bacteriay-bacteria.fitted.glmmML)/sqrt(bacteria.fitted.glmmML*(1-bacteria.fitted.glmmML))
plot(bacteria.fitted.glmmML,bacteria.resid.glmmML,ylab='Pearson residuals')
abline(h=0, lty = 2)
abline(h=c (-1.96, 1.96) , lty = 2, col ="blue")

# ---
# Laplace
# ---

help(glmer)
bacteria.mer.lp<- glmer(y~trt+I(week>2) + (1|ID), 
                        family=binomial, data=bacteria )
summary(bacteria.mer.lp)
plot(bacteria.mer.lp)

# ---
# Adaptive GQ
# ---

bacteria.mer.ad<- glmer(y~trt+I(week>2) + (1|week), 
                        family=binomial, data=bacteria, 
                        nAGQ=24)
summary(bacteria.mer.ad)
plot(bacteria.mer.ad)
ranef(bacteria.mer.ad)

# --- b. Encouragement  ----

# The effect is deemed as non-significant 

# --- c. GLM vs GLMM  ----

bacteria.glm <- glm(y~trt+I(week>2),family=binomial,data=bacteria)
summary(bacteria.glm)

# the glm is nested within the GLMM. Moreover both have been estimated with ML
bacteria.glm$aic 
bacteria.glmmML$aic
# lower aic on the GLMM, fits better because it considers the grouping


##### Exercise 2 #####

# ---
# Outcome  
# ---
# y    :  number of seizures

# ---
# Covariates : 
# ---

# Time  : Time period in which the seizures were encoutered
# group : placebo or progabide

# ---
# Grouping identificator : 
# ---

# Subject : subject ID. a factor.

# N = 220 

# ---
# Data and (some) exploratory analysis
# ---

seiz <- read.table('seizure.txt',header=T)
str(seiz)

plot(seiz)

boxplot(seiz$y~seiz$Subject)  # big differences across subjects
# table(seiz$Subject)           # exactly 5 measures per subject, boxplots are basically meaningful
# plot(seiz$y~seiz$Subject,col=kronecker(1:10,rep(1,5)),pch=19) # better than boxplot

# 59*5 # 295 observations in total

#create new variable "post treatment"
seiz$post <- rep(0,295)
seiz$post[seiz$Time>8] <- 1
seiz$post # dummy: 0 = baseline, 1 = post-treatment

#create new variable "exposure"
table(seiz$Time) # time length between measures is 8,2,2,2,2
seiz$exposure <- rep(c(8,2,2,2,2),59)
seiz$exposure

#---- GLMM estimation ----

seiz.glmmPQL <- glmmPQL(y~factor(group)*post+offset(log(exposure)),random=~1|Subject,family=poisson,data=seiz)
summary(seiz.glmmPQL) # progabide not significant

#---- Comparison with GEE ----

seizure.gee <- gee(y~post*factor(group)+offset(log(rep(c(8,2,2,2,2),59))),
                   id=Subject,corstr="exchangeable",family=poisson,data=seiz)
summary(seizure.gee)

plot(seiz.glmmPQL)   # one large residual, otherwise structure of a poisson model
# plot(residuals(seiz.glmmPQL)~factor(group), data = seiz)
# plot(residuals(seiz.glmmPQL)~post, data = seiz)

which.max(seiz.glmmPQL$resid[,2]) # observation 124

colvec <- rep(1,295)
colvec[124] <- 2 # give red color to obs 124
plot(seiz$Subject,seiz.glmmPQL$resid[,2],col=colvec,pch=19) # apart from that point, all residuals look alright

plot(seiz$y~seiz$Subject,col=colvec,pch=19)

