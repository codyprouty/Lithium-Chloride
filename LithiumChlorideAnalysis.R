##

#Analysis for lithium chloride manuscript

#Contact: cprouty@ufl.edu    https://scholar.google.com/citations?user=PpeDx78AAAAJ&hl=en

##

#load packages
library(lme4)
library(lsmeans)
library(multcomp)
library(brglm2)
library(afex)
##

setwd()
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
MW <- lmer(Mitesper ~ Treatment * Sample + (1|Replicate), Wash)
anova(MW, test="F")
#Effect of mite washes insignificant
###

#Effect of treatment on sticky board mites
MS <- lmer(Mites~ Treatment * Sample +(1|Replicate), data = Sticky)
anova(MS, test="F")
###

#Comparisons within time periods
Sticky1 <- subset(Sticky, Sample == 1)
Sticky2 <- subset(Sticky, Sample == 2)
Sticky3 <- subset(Sticky, Sample == 3)

MS1 <- lm(Mites~ Treatment, data = Sticky2)
anova(MS1, test="Chisq")
summary(MS1)
#significant differences only exist on the second sampling time

#MCs for time 2
lsm<-lsmeans (MS1, list( ~ Treatment))
cld(lsm)
###

#Effect of treatment on bee estimation
Bees <- lmer(Beespercm2~ Treatment * Time +(1|Replicate) + (1|Frame) + (1|Side), data = Colony)
anova(Bees)
summary(Bees)

#Need to check residuals for normality
resids <- resid(Bees)
hist(resids, breaks = 10, xlab="residuals")
shapiro.test(resids)
#Bee estimations are not normally distributed.

#Average the data within treatments, and run the ANOVA based on that.
BeeAvg <- aggregate(Beespercm2~Replicate + Time + Treatment, data=Colony, FUN=mean)
Bees1 <- lm(Beespercm2~Treatment*Time, data = BeeAvg)
anova(Bees1)

resids <- resid(Bees1)
hist(resids, breaks = 10, xlab="residuals")
shapiro.test(resids)
#Residuals are now normally distributed
###

#Effect of treatment on brood estimation
Colony$Broodpercm2 <- round(Colony$Broodpercm2, 2)

Brood <- lmer(Broodpercm2~ Treatment * Time +(1|Replicate) + (1|Frame) + (1|Side), data = Colony)
anova(Brood)

resids <- resid(Brood)
hist(resids, breaks = 10, xlab="residuals")
shapiro.test(resids)
#Again, not normally distributed

#Average out multiple observations and frames, same as bees
BroodAvg <- aggregate(Broodpercm2~Replicate + Time + Treatment, data=Colony, FUN=mean)
Brood1 <- lm(Broodpercm2~Treatment*Time, data = BroodAvg)
anova(Brood1)

resids <- resid(Brood1)
hist(resids, breaks = 10, xlab="residuals")
shapiro.test(resids)
#Averaged brood residuals are normally distributed.
###

