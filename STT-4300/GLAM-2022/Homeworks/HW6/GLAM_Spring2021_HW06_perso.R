##############################
### GLAM Spring 2013 HW 06 ###
##############################


rm(list=ls()) # clear the environnment of all objects defined previously
setwd("C:/Users/WilliamAeberhard/Assistanat/Courses/Spring_2013/GLAM_Spring2013/datasets")

library(gee)

### Exercise 1

lei.ori <- read.table('LEI.dat',header=T)
head(lei.ori)
str(lei.ori)

sum(is.na(lei.ori)) # some missing values
lei <- na.omit(lei.ori)
nrow(lei)

lei.gee1 <- gee(SUCCESS~EXTOA+PROPLGSP+PROPLIFT+ASKASSIS+HELP+COMPS+
+NECKFLEX+TRHAND+TRGEND+factor(TRIALCAT),id=TRAINEE,corstr="exchangeable",
family=binomial,data=lei)

str(lei.gee1) # same strucure as glm object
lei.gee1$res # except these are raw residuals = lei$SUCCESS-lei.gee1$fitted

options(digits=2)
summary(lei.gee1)

library(lattice) # for the bwplot function
bwplot(lei$TRAINEE~lei.gee1$res,xlab='raw residuals',ylab='Trainee') # raw residuals

res.lei.gee1 <-(lei$SUCCESS-lei.gee1$fitted)/sqrt(lei.gee1$fitted*(1-lei.gee1$fitted)) # construct Pearson res manually
bwplot(lei$TRAINEE~res.lei.gee1,xlab='Pearson residuals',ylab='Trainee') # pearson residuals this time
# median around 0 => ok
# however large residual values for trainee 1 and 13 => bad fit
# different variances for each trainee => not a problem since different mu_i


## backward variable selection procedure using Robust z-statistic

lei.gee2 <- gee(SUCCESS~EXTOA+PROPLGSP+PROPLIFT+ASKASSIS+HELP+COMPS+
+NECKFLEX+TRGEND+factor(TRIALCAT),id=TRAINEE,corstr="exchangeable",
family=binomial,data=lei,na.action=na.omit)
summary(lei.gee2) # already droped TRHAND, now drop TRGEND

lei.gee3 <- gee(SUCCESS~EXTOA+PROPLGSP+PROPLIFT+ASKASSIS+HELP+COMPS+NECKFLEX+factor(TRIALCAT),
id=TRAINEE,corstr="exchangeable",family=binomial,data=lei,na.action=na.omit)
summary(lei.gee3) # now drop EXTOA

lei.gee4 <- gee(SUCCESS~PROPLGSP+PROPLIFT+ASKASSIS+HELP+COMPS+NECKFLEX+factor(TRIALCAT),
id=TRAINEE,corstr="exchangeable",family=binomial,data=lei,na.action=na.omit)
summary(lei.gee4) # now drop NECKFLEX

lei.gee5 <- gee(SUCCESS~PROPLGSP+PROPLIFT+ASKASSIS+HELP+COMPS+factor(TRIALCAT),
id=TRAINEE,corstr="exchangeable",family=binomial,data=lei,na.action=na.omit)
summary(lei.gee5) # drop ASKASSIS

lei.gee6 <- gee(SUCCESS~PROPLGSP+PROPLIFT+HELP+COMPS+factor(TRIALCAT),
id=TRAINEE,corstr="exchangeable",family=binomial,data=lei,na.action=na.omit)
summary(lei.gee6) # drop COMPS

lei.gee7 <- gee(SUCCESS~PROPLGSP+PROPLIFT+HELP+factor(TRIALCAT),
id=TRAINEE,corstr="exchangeable",family=binomial,data=lei,na.action=na.omit)
summary(lei.gee7) # stop here

res.lei.gee7 <-(lei$SUCCESS-lei.gee7$fitted)/sqrt(lei.gee7$fitted*(1-lei.gee7$fitted))
bwplot(lei$TRAINEE~res.lei.gee7,xlab='Pearson residuals',ylab='Trainee')



## Trying independence working correlation structure

lei.gee8 <- gee(SUCCESS~PROPLGSP+PROPLIFT+HELP+factor(TRIALCAT),
id=TRAINEE,corstr="independence",family=binomial,data=lei)
summary(lei.gee8)

lei.gee9 <- gee(SUCCESS~PROPLGSP+PROPLIFT+HELP+factor(TRIALCAT),
id=TRAINEE,corstr="unstructured",family=binomial,data=lei) # not feasible...

res.lei.gee8 <-(lei$SUCCESS-lei.gee8$fitted)/sqrt(lei.gee8$fitted*(1-lei.gee8$fitted))
bwplot(lei$TRAINEE~res.lei.gee8,xlab='Pearson residuals',ylab='Trainee')
# results change a little but not that much
head(lei.gee7$working.corr) # 0.037 is close to 0, so "independence" is not so irrelevant



## Compare with GLM

lei.glm <- glm(SUCCESS~PROPLGSP+PROPLIFT+HELP+factor(TRIALCAT),family=binomial,data=lei,na.action=na.omit)
summary(lei.glm)
par(mfrow=c(2,2))
plot(lei.glm)
par(mfrow=c(1,1))

library(statmod)
par(mfrow=c(2,2))
for (i in 1:4){
  rqresid.fit <- qresid(lei.glm)
  qqnorm(rqresid.fit)
  qqline(rqresid.fit,col='red')
}
par(mfrow=c(1,1))

options(digits=5)
lei.gee8$coef
lei.glm$coef 

range(lei.gee8$coef-lei.glm$coef) # identical up to roundings




### Exercise 2

ovary <- read.table('Ovary.dat',header=T)
head(ovary)
str(ovary)

plot(ovary)

table(ovary$follicles)

boxplot(ovary$follicles~ovary$Mare,names=1:max(ovary$Mare),xlab='Mare',ylab='Number of follicles',col='deeppink')


ov.gee <- gee(follicles~Time,id=Mare,corstr="exchangeable",family=poisson,data=ovary)
summary(ov.gee)

res.ov.gee <- (ovary$follicles-ov.gee$fitted)/sqrt(ov.gee$fitted) # construct Pearson res manually
bwplot(ovary$Mare~res.ov.gee,xlab='Pearson residuals',ylab='Mare')
# all residuals between -3 and +3

head(ov.gee$working.corr)# independence of observations does not seem to be a good idea, however we try it
ov.gee2 <- gee(follicles~Time,id=Mare,corstr="independence",family=poisson,data=ovary)
summary(ov.gee2)

summary(ov.gee)$coef
summary(ov.gee2)$coef # coeff don't change much, inference either


