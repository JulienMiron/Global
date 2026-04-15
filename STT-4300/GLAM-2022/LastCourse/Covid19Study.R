#########################################################
#
#  Eva Cantoni - May 2021 - GLAM course
#  
#  Illustration of the methodology in 
# 
#  "COVINDEX based on a GAM beta regression model with an 
#  application to the COVID-19 pandemic in Italy"
#  by Luca Scrucca ( 	arXiv:2104.01344 )
#  
#  on Swiss data from OFSP (https://www.covid19.admin.ch/)
#
#########################################################

setwd("C:/Users/cantoni/Dropbox/GLAM-2021/LastCourse")
rm(list=ls())

##########################################################
# Plot of effective reproductive number
##########################################################


RindexAll <- read.csv(file="OFSPdata/COVID19Re_geoRegion.csv")
head(RindexAll)
RindexCH <- RindexAll[RindexAll$geoRegion=="CH",]
RindexCH$date <- as.Date(RindexCH$date)

require(ggplot2)
ggplot(data = RindexCH, aes(x = date)) + geom_line(aes(y=median_R_mean), size=1.05) +
  geom_line(aes(y=median_R_lowHPD)) +geom_line(aes(y=median_R_highHPD)) + 
  geom_hline(yintercept=1) +
  labs(x = "Date",
       y = "Effective Reproductive Number",
       title = "Switzerland")

##########################################
# Beta regression
##########################################

TestAll <- read.csv(file="OFSPdata/COVID19Test_geoRegion_all.csv")
#TestAll <- read.csv(file="OFSPdata/COVID19Test_geoRegion_PCR_Antigen.csv")
head(TestAll)
TestCH <- TestAll[TestAll$geoRegion=="CH",c("datum","entries","entries_pos")]
TestCH <- na.omit(TestCH)
TestCH$datum <- as.Date(TestCH$datum)
TestCH$time <- 1:362
TestCH$pos_rate <- TestCH$entries_pos/TestCH$entries


ggplot(data = TestCH, aes(x = datum, y= pos_rate)) + geom_point() +
  labs(x = "Date",
       y = "COVID Test Positive Rate",
       title = "Switzerland")

ggplot(data = TestCH, aes(x = datum, y= pos_rate)) + geom_point() +
  geom_point(aes(size = entries)) +
  labs(x = "Date",
       y = "COVID Test Positive Rate",
       title = "Switzerland")

# Modeling the positive rate
require(mgcv)
TestCHbetareg <- gam(pos_rate~s(time,k=100), weights=entries/mean(entries), family=betar, data=TestCH)
summary(TestCHbetareg)

ggplot(data = TestCH, aes(x = datum)) + geom_point(aes(y=pos_rate)) +
   geom_line(y=fitted(TestCHbetareg)) +
    labs(x = "Date",
       y = "COVID Test Positive Rate",
       title = "Switzerland")


# Modeling covindex
fitted.betareg <- fitted(TestCHbetareg)
covindexCH <- fitted.betareg[8:362]/fitted.betareg[1:355]

indexes.df <- data.frame(Re=RindexCH$median_R_mean[8:362], covindex=covindexCH, date=RindexCH$date[8:362])

ggplot(data = indexes.df, aes(x = date)) + geom_line(aes(y=Re), color="black", size=1) +
  geom_line(aes(y=covindex),color="blue",size=1) + 
  geom_hline(yintercept=1) + 
  labs(x = "Date",
       y = "Re and COVINDEX",
       title = "Switzerland")

# Adding a day of the week factor

ggplot(data = TestCH, aes(x = datum, y= pos_rate)) + geom_line() +
  labs(x = "Date",
       y = "COVID Test Positive Rate",
       title = "Switzerland")


TestCH$day <- c(rep(c("Sat", "Sun", "Mon","Tue","Wed","Thu","Fri"),51), c("Sat", "Sun", "Mon","Tue","Wed"))
TestCHbetareg.day <- gam(pos_rate~s(time,k=100) + factor(day), weights=entries/mean(entries), family=betar,data=TestCH)
summary(TestCHbetareg.day)
plot(TestCHbetareg.day)

ggplot(data = TestCH, aes(x = datum)) + geom_point(aes(y=pos_rate)) +
  geom_line(aes(y=fitted(TestCHbetareg.day)),size=0.8) +
  geom_line(y=fitted(TestCHbetareg),linetype="dashed",colour="blue",size=0.8) +
  labs(x = "Date",
       y = "COVID Test Positive Rate",
       title = "Switzerland")


fitted.betareg.day <- fitted(TestCHbetareg.day,"response")
covindexCH.day <- fitted.betareg.day[8:362]/fitted.betareg.day[1:355]

indexes.df <- data.frame(Re=RindexCH$median_R_mean[8:362], covindex=covindexCH, covindex.day= covindexCH.day, date=RindexCH$date[8:362])


ggplot(data = indexes.df, aes(x = date)) + geom_line(aes(y=Re, color="Re"), size=0.8) +
  geom_line(aes(y=covindex.day,color="COVINDEX"),size=0.8) + 
  geom_hline(yintercept=1) +
  scale_colour_manual(values=c("blue", "black")) +
  theme(legend.key.width=unit(2,"cm"),legend.key.height=unit(0.5,"cm"),
        legend.position = c(0.85, 0.75))+
  labs(x = "Date",
       y = "Index",
       title = "Switzerland", 
       color="Legend")

