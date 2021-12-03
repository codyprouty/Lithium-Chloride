##

#Analysis for lithium chloride manuscript

#Contact: cprouty@ufl.edu    https://scholar.google.com/citations?user=PpeDx78AAAAJ&hl=en

##

#load packages
library(lme4)
library(lsmeans)
library(multcomp)
##

#Import files
Colony <- read.csv("ColonyEvals.csv")
Wash <- read.csv("MitesLCWash.csv")
Sticky <- read.csv("MitesLCSticky.csv")
###

#Organize Data
names(Colony)[1] <- "Treatment"
Colony$Replicate <- as.factor(Colony$Replicate)
Colony$Frame <- as.factor(Colony$Frame)
Colony$Time <- as.factor(Colony$Time)
names(Wash)[1] <- "Sample"
Wash$Sample <- as.factor(Wash$Sample)
Wash$Replicate <- as.factor(Wash$Replicate)
names(Sticky)[1] <- "Sample"
Sticky$Sample <- as.factor(Sticky$Sample)
Sticky$Replicate <- as.factor(Sticky$Replicate)
###

#Effect of treatment on alcohol wash
MW <- lmer(Mitesper ~ Treatment * Sample +(1|Replicate), data = Wash)
anova(MW)
summary(MW)

lsm<-lsmeans (MW, list( ~ Treatment + Sample))
cld(lsm)
###

#Effect of treatment on sticky board mites
MS <- lmer(Mites~ Treatment * Sample +(1|Replicate), data = Sticky)
anova(MS)
summary(MS)

lsm<-lsmeans (MS, list( ~ Treatment + Sample))
cld(lsm)
###

#Comparisons within time periods
Sticky1 <- subset(Sticky, Sample == 1)
Sticky2 <- subset(Sticky, Sample == 2)
Sticky3 <- subset(Sticky, Sample == 3)

MS1 <- lmer(Mites~ Treatment * Sample +(1|Replicate), data = Sticky1)
anova(MS1)
summary(MS1)

lsm<-lsmeans (MS1, list( ~ Treatment + Sample))
cld(lsm)
###

#Effect of treatment on bee estimation
Bees <- lmer(Beespercm2~ Treatment * Time +(1|Replicate) + (1|Frame) + (1|Side), data = Colony)
anova(Bees)
summary(Bees)


lsm<-lsmeans (Bees, list( ~ Treatment + Time))
cld(lsm)
###

#Effect of treatment on brood estimation
Brood <- lmer(Broodpercm2~ Treatment * Time +(1|Replicate) + (1|Frame) + (1|Side), data = Colony)
anova(Brood)
summary(Brood)


lsm<-lsmeans (Brood, list( ~ Treatment + Time))
cld(lsm)
###

#Comparisons within time periods
Colony1 <- subset(Colony, Time ==1)
Colony2 <- subset(Colony, Time ==2)

Brood1 <- lmer(Broodpercm2~ Treatment +(1|Replicate) + (1|Frame) + (1|Side), data = Colony1)
anova(Brood1)
summary(Brood1)


lsm<-lsmeans (Brood1, list( ~ Treatment ))
cld(lsm)
