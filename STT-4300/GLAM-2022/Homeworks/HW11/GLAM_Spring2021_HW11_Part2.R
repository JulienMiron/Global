########################################
### GLAM Spring 2020 HW 11 : Part II ###
########################################

rm(list = ls()) # clear the environnment of all objects defined previously
setwd("../datasets") # set your working directory containing the data files

library(mgcv)

#  =  =  =  =  =  =  =  =  =  =  =  =  =  =  = 
# Exercise 3
#  =  =  =  =  =  =  =  =  =  =  =  =  =  =  = 

cig <- read.table(file = 'cigarettes.txt', header = T)
head(cig)
str(cig)

## a)

hist(cig$cigs, col = 'deeppink') # counts, right-skewed as usual


barplot(table(cig$cigs), col = 'deeppink', las = 2) 
# peaks at 10, 15, 20, 30 and 40

## b) and c)

source('truncpoisson_gam.r')

table(cig$educ) # only 8 different values observed

cig.gam <- bam(cigs ~ s(educ, k = 8) + s(cigpric, k = 8) + s(age) + s(income) + 
                 restaurn + white, 
               family = truncpoisson, data = cig)

summary(cig.gam)


plot(cig.gam, pages = 1) 
# effect of age close to quadratic, max around 40 years old

## d)
ciglog.gam <- bam(cigs ~ s(educ, k = 8) + s(cigpric, k = 8) + s(age) + 
                    s(log(income)) + 
                    restaurn + white, 
                  family = truncpoisson, data = cig)
par(mfrow = c(1, 2))
plot(cig$income, cig$cigs)
plot(cig.gam, select = 4) # log?
plot(ciglog.gam, select = 4) # log?

## e)

summary(cig.gam) # edf far from 1

plot(cig.gam, select = 2)
abline(h = 0, col = 'blue') # 0 always included in bands

cig.gam.sub <- bam(cigs ~ s(educ, k = 8) + s(age) + s(income) + 
                     restaurn + white, 
                   family = truncpoisson, data = cig)
cig.gam$gcv.ubre
cig.gam.sub$gcv.ubre
# UBRE decreased a little when removing cigpric
#  = > we could drop cigpric

