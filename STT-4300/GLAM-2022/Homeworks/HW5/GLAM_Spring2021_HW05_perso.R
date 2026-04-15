##############################
### GLAM Spring 2013 HW 03 ###
##############################


rm(list=ls()) # clear the environnment of all objects defined previously
setwd("C:/Users/WilliamAeberhard/Assistanat/Courses/Spring_2013/GLAM_Spring2013/datasets")


### Exercise 3

doc <- read.table(file='docvisits.asc',header=T)
head(doc)
str(doc)

hist(doc$dvisits)
table(doc$dvisits) # a lot of zeros
sum(doc$dvisits==0)/length(doc$dvisits)

## a)
source('truncpoisson.R')
library(pscl)

# zeros modeling
doc.hurdle.0 <- glm((dvisits>0)~sex+age+agesq+income+levyplus+freepoor+freerepa+illness+
  actdays+hscore+chcond1+chcond2,data=doc,family=binomial)
summary(doc.hurdle.0)

# positive counts modeling
doc.hurdle.pos <- glm(dvisits~sex+age+agesq+income+levyplus+freepoor+freerepa+illness+
  actdays+hscore+chcond1+chcond2,data=doc,family=truncpoisson,subset=(dvisits>0))
summary(doc.hurdle.pos)

# with the hurdle command
doc.hurdle <- hurdle(dvisits~sex+age+agesq+income+levyplus+freepoor+freerepa+illness+
  actdays+hscore+chcond1+chcond2,dist="poisson",zero.dist="binomial",link="logit",data=doc)
summary(doc.hurdle)
extractAIC(doc.hurdle)

plot(as.vector(fitted(doc.hurdle)),doc$dvisits)
abline(0,1,col='red')

doc.hurdle.res <- residuals(doc.hurdle,type='pearson')

plot(doc.hurdle.res)

plot(doc$dvisits) # first positive counts, then zeros

plot(doc.hurdle$fitted,doc.hurdle.res)

qqnorm(doc.hurdle.res)
qqline(doc.hurdle.res,col='red')


## b)
# Poisson GLM
doc.glm <- glm(dvisits~sex+age+agesq+income+levyplus+freepoor+freerepa+illness+
actdays+hscore+chcond1+chcond2,family=poisson,data=doc)
summary(doc.glm)

par(mfrow=c(2,2))
plot(doc.glm)
par(mfrow=c(1,1))

library(statmod)
par(mfrow=c(2,2))
for (i in 1:4){
  rqresid.fit.pois <- qresid(doc.glm)
  qqnorm(rqresid.fit.pois) # heavy right tail
  qqline(rqresid.fit.pois,col='red')
}
par(mfrow=c(1,1))

plot(rqresid.fit.pois)

summary(doc.glm$fitted)
plot(doc.glm$fitted,doc$dvisits)

pchisq(doc.glm$dev,df=doc.glm$df.res,lower.tail=F) # we don't reject it's as good as the saturated model
qchisq(0.95,df=5177) # critical value very high
pchisq(doc.glm$null.dev-doc.glm$dev,df=doc.glm$df.null-doc.glm$df.res,lower.tail=F) # our model is better than the null model



# ZIP
doc.zip <- zeroinfl(dvisits~sex+age+agesq+income+levyplus+freepoor+freerepa+illness+
actdays+hscore+chcond1+chcond2,dist="poisson",link="logit",data=doc)
summary(doc.zip)

doc.zip.res <- residuals(doc.zip,type='pearson')

plot(doc.zip.res)

plot(doc.zip$fitted,doc.zip.res)

qqnorm(doc.zip.res)
qqline(doc.zip.res,col='red')





























