################################################################################
#  Replace ... by the location where scripts and datasets are saved            #
################################################################################

filepath=".../"

################################################################################
#  Source/load all scripts, librairies and datasets                            #
################################################################################


library(lattice)

source(paste(filepath,"Chapter6_functions.r",sep=""))

load(paste(filepath,"Chapter6.rdata",sep=""))

################################################################################
#  GUIDE data                                                                  #
################################################################################


#
#       ##############
#       # Figure 6.1 #
#       ##############
#

xyplot(factor(bothered)~occur|factor(pract_id),data=GUIDE,xlab="occurence",ylab="bothered",as.table=TRUE)

#
#       ##############
#       # Figure 6.2 #
#       ##############
par("mar"=c(2,4,2,4))
par("cex"=0.5)
par("cex.lab"=1.2)

par(mfrow=c(5,2))
barplot(table(GUIDE$female[GUIDE$bothered==1])/54,main="female",ylim=c(min(table(GUIDE$female[GUIDE$bothered==1])/54, table(GUIDE$female[GUIDE$bothered==0])/83),max(table(GUIDE$female[GUIDE$bothered==1])/54, table(GUIDE$female[GUIDE$bothered==0])/83)),ylab="Proportions")

barplot(table(GUIDE$female[GUIDE$bothered==0])/83,main="female",ylim=c(min(table(GUIDE$female[GUIDE$bothered==1])/54, table(GUIDE$female[GUIDE$bothered==0])/83),max(table(GUIDE$female[GUIDE$bothered==1])/54, table(GUIDE$female[GUIDE$bothered==0])/83)),ylab="Proportions")

hist(GUIDE$age[GUIDE$bothered==1],xlab="(age-76)/10",breaks=c(0,0.2,0.4,0.6,0.8,1,1.2,1.4,1.6),main="(age-76)/10",ylim=c(0,max(hist(GUIDE$age[GUIDE$bothered==1],plot=FALSE)$density, hist(GUIDE$age[GUIDE$bothered==0],plot=FALSE)$density)),prob=TRUE)

hist(GUIDE$age[GUIDE$bothered==0],xlab="(age-76)/10",breaks=c(0,0.2,0.4,0.6,0.8,1,1.2,1.4,1.6),main="(age-76)/10",ylim=c(0,max(hist(GUIDE$age[GUIDE$bothered==1],plot=FALSE)$density, hist(GUIDE$age[GUIDE$bothered==0],plot=FALSE)$density)),prob=TRUE)

hist(GUIDE$dayacc[GUIDE$bothered==1],xlab="accidents",main="day accidents",ylim=c(0,max(hist(GUIDE$dayacc[GUIDE$bothered==1],plot=FALSE)$density, hist(GUIDE$dayacc[GUIDE$bothered==0],plot=FALSE)$density)),prob=TRUE,breaks=seq(0,18,by=2))

hist(GUIDE$dayacc[GUIDE$bothered==0],xlab="accidents",main="day accidents",ylim=c(0,max(hist(GUIDE$dayacc[GUIDE$bothered==1],plot=FALSE)$density,hist(GUIDE$dayacc[GUIDE$bothered==0],plot=FALSE)$density)),prob=TRUE,breaks=seq(0,18,by=2))

barplot(table(GUIDE$severe[GUIDE$bothered==1])/54,main="severe",ylim=c(min(table(GUIDE$severe[GUIDE$bothered==1])/54, table(GUIDE$severe[GUIDE$bothered==0])/83),max(table(GUIDE$severe[GUIDE$bothered==1])/54, table(GUIDE$severe[GUIDE$bothered==0])/83)),ylab="Proportions")

barplot(table(GUIDE$severe[GUIDE$bothered==0])/54,main="severe",ylim=c(min(table(GUIDE$severe[GUIDE$bothered==1])/83, table(GUIDE$severe[GUIDE$bothered==0])/54),max(table(GUIDE$severe[GUIDE$bothered==1])/83, table(GUIDE$severe[GUIDE$bothered==0])/54)),ylab="Proportions")

hist(GUIDE$toilet[GUIDE$bothered==1],xlab="visits",main="toilet visits",ylim=c(0,max(hist(GUIDE$toilet[GUIDE$bothered==1],plot=FALSE)$density, hist(GUIDE$toilet[GUIDE$bothered==0],plot=FALSE)$density)),prob=TRUE)

hist(GUIDE$toilet[GUIDE$bothered==0],xlab="visits",main="toilet visits",ylim=c(0,max(hist(GUIDE$toilet[GUIDE$bothered==1],plot=FALSE)$density, hist(GUIDE$toilet[GUIDE$bothered==0],plot=FALSE)$density)),prob=TRUE)


#
#       #############
#       # Table 6.1 #
#       #############
#
GUIDEclass <-robGEE(bothered~female+age+dayacc+severe+toilet,cluster=pract_id,data=GUIDE,tuningc.y=1000,kconst=1000)
GUIDEclass

GUIDEHuber <- robGEE(bothered~female+age+dayacc+severe+toilet,cluster=pract_id,data=GUIDE,tuningc.y=1.5,kconst=2.4)
GUIDEHuber

GUIDEMallows <- robGEE(bothered~female+age+dayacc+severe+toilet,cluster=pract_id,data=GUIDE,tuningc.y=1.5,weights.on.x="hat",kconst=2.4)
GUIDEMallows

#
#       ##############
#       # Figure 6.4 #
#       ##############
#
bypractice <- split(GUIDEHuber$w.r,GUIDE$pract_id)
minbycluster <- lapply(bypractice,min)
maxbycluster <- lapply(bypractice,max)

X11()
plot(GUIDE$pract_id,unlist(bypractice),xlab="Practice",ylab="Weigths w(r)")
segments(x0=unique(GUIDE$pract_id),y0=unlist(minbycluster),x1=unique(GUIDE$pract_id),y1=unlist(maxbycluster))
identify(GUIDE$pract_id,unlist(bypractice))


#
#       #############
#       # Table 6.2 #
#       #############
#

# CLASSICAL
GUIDEclassfemale <-robGEE(bothered~age+dayacc+severe+toilet,cluster=pract_id,data=GUIDE,tuningc.y=1000,kconst=1000)
anovarobGEE(GUIDEclass,GUIDEclassfemale,data=GUIDE)

GUIDEclassage <-robGEE(bothered~female+dayacc+severe+toilet,cluster=pract_id,data=GUIDE,tuningc.y=1000,kconst=1000)
anovarobGEE(GUIDEclass,GUIDEclassage,data=GUIDE)

GUIDEclassdayacc <-robGEE(bothered~female+age+severe+toilet,cluster=pract_id,data=GUIDE,tuningc.y=1000,kconst=1000)
anovarobGEE(GUIDEclass,GUIDEclassdayacc,data=GUIDE)

GUIDEclasssevere <-robGEE(bothered~female+age+dayacc+toilet,cluster=pract_id,data=GUIDE,tuningc.y=1000,kconst=1000)
anovarobGEE(GUIDEclass,GUIDEclasssevere,data=GUIDE)

GUIDEclasstoilet <-robGEE(bothered~female+age+dayacc+severe,cluster=pract_id,data=GUIDE,tuningc.y=1000,kconst=1000)
anovarobGEE(GUIDEclass,GUIDEclasstoilet,data=GUIDE)

# Remove age

GUIDEclassagefemale <- robGEE(bothered~dayacc+severe+toilet,cluster=pract_id,data=GUIDE,tuningc.y=1000,kconst=1000)
anovarobGEE(GUIDEclassage,GUIDEclassagefemale,data=GUIDE)

GUIDEclassagedayacc <- robGEE(bothered~female+severe+toilet,cluster=pract_id,data=GUIDE,tuningc.y=1000,kconst=1000)
anovarobGEE(GUIDEclassage,GUIDEclassagedayacc,data=GUIDE)

GUIDEclassagesevere <- robGEE(bothered~female+dayacc+toilet,cluster=pract_id,data=GUIDE,tuningc.y=1000,kconst=1000)
anovarobGEE(GUIDEclassage,GUIDEclassagesevere,data=GUIDE)

GUIDEclassagetoilet <- robGEE(bothered~female+dayacc+severe,cluster=pract_id,data=GUIDE,tuningc.y=1000,kconst=1000)
anovarobGEE(GUIDEclassage,GUIDEclassagetoilet,data=GUIDE)

# Remove female

GUIDEclassagefemaledayacc <- robGEE(bothered~severe+toilet,cluster=pract_id,data=GUIDE,tuningc.y=1000,kconst=1000)
anovarobGEE(GUIDEclassagefemale,GUIDEclassagefemaledayacc,data=GUIDE)

GUIDEclassagefemalesevere <- robGEE(bothered~dayacc+toilet,cluster=pract_id,data=GUIDE,tuningc.y=1000,kconst=1000)
anovarobGEE(GUIDEclassagefemale,GUIDEclassagefemalesevere,data=GUIDE)

GUIDEclassagefemaletoilet <- robGEE(bothered~dayacc+severe,cluster=pract_id,data=GUIDE,tuningc.y=1000,kconst=1000)
anovarobGEE(GUIDEclassagefemale,GUIDEclassagefemaletoilet,data=GUIDE)

# Remove toilet
GUIDEclassagefemaletoiletdayacc <- robGEE(bothered~severe,cluster=pract_id,data=GUIDE,tuningc.y=1000,kconst=1000)
anovarobGEE(GUIDEclassagefemaletoilet,GUIDEclassagefemaletoiletdayacc,data=GUIDE)

GUIDEclassagefemaletoiletsevere <- robGEE(bothered~dayacc,cluster=pract_id,data=GUIDE,tuningc.y=1000,kconst=1000)
anovarobGEE(GUIDEclassagefemaletoilet,GUIDEclassagefemaletoiletsevere,data=GUIDE)


# ROBUST
GUIDEfemale <- robGEE(bothered~age+dayacc+severe+toilet,cluster=pract_id,data=GUIDE,tuningc.y=1.5,kconst=2.4)
anovarobGEE(GUIDEHuber,GUIDEfemale,data=GUIDE)

GUIDEage <- robGEE(bothered~female+dayacc+severe+toilet,cluster=pract_id,data=GUIDE,tuningc.y=1.5,kconst=2.4)
anovarobGEE(GUIDEHuber,GUIDEage,data=GUIDE)

GUIDEdayacc <- robGEE(bothered~female+age+severe+toilet,cluster=pract_id,data=GUIDE,tuningc.y=1.5,kconst=2.4)
anovarobGEE(GUIDEHuber,GUIDEdayacc,data=GUIDE)

GUIDEsevere <- robGEE(bothered~female+age+dayacc+toilet,cluster=pract_id,data=GUIDE,tuningc.y=1.5,kconst=2.4)
anovarobGEE(GUIDEHuber,GUIDEsevere,data=GUIDE)

GUIDEtoilet <- robGEE(bothered~female+age+dayacc+severe,cluster=pract_id,data=GUIDE,tuningc.y=1.5,kconst=2.4)
anovarobGEE(GUIDEHuber,GUIDEtoilet,data=GUIDE)

# Remove severe

GUIDEseverefemale <- robGEE(bothered~age+dayacc+toilet,cluster=pract_id,data=GUIDE,tuningc.y=1.5,kconst=2.4)
anovarobGEE(GUIDEsevere,GUIDEseverefemale,data=GUIDE)

GUIDEsevereage <- robGEE(bothered~female+dayacc+toilet,cluster=pract_id,data=GUIDE,tuningc.y=1.5,kconst=2.4)
anovarobGEE(GUIDEsevere,GUIDEsevereage,data=GUIDE)

GUIDEseveredayacc <- robGEE(bothered~female+age+toilet,cluster=pract_id,data=GUIDE,tuningc.y=1.5,kconst=2.4)
anovarobGEE(GUIDEsevere,GUIDEseveredayacc,data=GUIDE)

GUIDEseveretoilet <- robGEE(bothered~female+age+dayacc,cluster=pract_id,data=GUIDE,tuningc.y=1.5,kconst=2.4)
anovarobGEE(GUIDEsevere,GUIDEseveretoilet,data=GUIDE)

# Remove female
GUIDEseverefemaleage <- robGEE(bothered~dayacc+toilet,cluster=pract_id,data=GUIDE,tuningc.y=1.5,kconst=2.4)
anovarobGEE(GUIDEseverefemale,GUIDEseverefemaleage,data=GUIDE)

GUIDEseverefemaledayacc <- robGEE(bothered~age+toilet,cluster=pract_id,data=GUIDE,tuningc.y=1.5,kconst=2.4)
anovarobGEE(GUIDEseverefemale,GUIDEseverefemaledayacc,data=GUIDE)

GUIDEseverefemaletoilet <- robGEE(bothered~age+dayacc,cluster=pract_id,data=GUIDE,tuningc.y=1.5,kconst=2.4)
anovarobGEE(GUIDEseverefemale,GUIDEseverefemaletoilet,data=GUIDE)

# Remove age
GUIDEseverefemaleagedayacc <- robGEE(bothered~toilet,cluster=pract_id,data=GUIDE,tuningc.y=1.5,kconst=2.4)
anovarobGEE(GUIDEseverefemaleage,GUIDEseverefemaleagedayacc,data=GUIDE)

GUIDEseverefemaleagetoilet <- robGEE(bothered~dayacc,cluster=pract_id,data=GUIDE,tuningc.y=1.5,kconst=2.4)
anovarobGEE(GUIDEseverefemaleage,GUIDEseverefemaleagetoilet,data=GUIDE)

# Final model (bottom of page 181)
GUIDEseverefemaleage

################################################################################
#  LEI data                                                                    #
################################################################################

#
#       ##############
#       # Figure 6.5 #
#       ##############
#

xyplot(factor(y1)~trial|subject,data=LEI, type="b",ylab="LEI completed in less than 30 seconds",scale=list(c(tick.number=2)),as.table=TRUE)


#
#       ##############
#       # Table 6.3  #
#       ##############
#
mean(LEI[LEI$y1==1,-c(1,2,12)])
mean(LEI[LEI$y1==0,-c(1,2,12)])

#
#       ##############
#       # Table 6.4  #
#       ##############
# Please, see the corrected version on the errata webpage for this table.

LEIHuber <- robGEE(y1~factor(neckflex)+factor(extoa)+factor(proplgsp)+factor(proplift)+factor(askas)+factor(help)+factor(comps)+factor(trhand)+factor(trgend),cluster=subject,data=LEI,tuningc.y=1.5,kconst=2.4)
LEIHuber

#
#       ##############
#       # Figure 6.5 #
#       ##############
#
plot(LEIHuber$w.r,xlab="Observation",ylab="Weights w(r)")
identify(LEIHuber$w.r)

#
#       ##############
#       # Table 6.5  #
#       ##############
#

LEIHubersub1 <- robGEE(y1~factor(neckflex)+factor(proplgsp)+factor(proplift)+factor(help)+factor(comps),cluster=subject,data=LEI,tuningc.y=1.5,kconst=2.4)
anovarobGEE(LEIHuber,LEIHubersub1,data=LEI)

LEIHubersub2 <- robGEE(y1~factor(neckflex)+factor(proplgsp)+factor(proplift)+factor(help),cluster=subject,data=LEI,tuningc.y=1.5,kconst=2.4)
anovarobGEE(LEIHubersub1,LEIHubersub2,data=LEI)


# Final model
LEIHubersub1

###################################################################################
#  piglet data                                                                    #
###################################################################################


#
#       ##############
#       # Table 6.6  #
#       ##############
#

pigletsrob <- robGEE(morti~factor(gentype)+factor(parity)+ factor(birthassist), cluster = litter, data = piglets, tuningc.y = 1.5, kconst = 2.4)


#
#       ###############
#       # Figure 6.7  #
#       ###############
#
plot(pigletsrob$w.r,xlab="Observation",ylab="Weights w.y")




