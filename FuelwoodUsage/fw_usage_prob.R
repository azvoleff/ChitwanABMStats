#############################################################################
# Does a series of models to try to predict fuelwood usage based on household 
# size, etc.
###############################################################################

library(Hmisc)
library(ggplot2)
PLOT_WIDTH = 8.33
PLOT_HEIGHT = 5.53
DPI=  300
theme_update(theme_grey(base_size=18))

# Find conversions from bhari to kg, and from cart and quintal to bhari
load("/media/Zvoleff_Passport/Data/Nepal/Fall_2009_Fieldwork/HHWoodUsage/R/hhwu2.Rdata")
summary(hhwu2$D0.Mass.Bhari)
summary(hhwu2$D0.Bhari.Cart)
summary(hhwu2$D0.Cart.Quintal)

load("/media/truecrypt1/Nepal/CVFS_R_format/t3ag.Rdata")
# Need months 108-119 from the household registry (January 2006-December 2006)
load("/media/truecrypt1/Nepal/CVFS_HHReg/hhreg126.Rdata")
#load("C:/Users/azvoleff/Code/R/Chitwan_R_files/Migration/migrations_wideformat_all_LD_migrations-1_months_away.Rdata")

# Calculate mean household size in 2006 (months 108-119).
hhid.cols <- grep('^hhid1[0-1][0-9]$', names(hhreg))
hhid.cols <- hhid.cols[9:length(hhid.cols)]
hhid <- hhreg[hhid.cols]
livng.cols <- grep('^livng1[0-1][0-9]$', names(hhreg))
livng.cols <- livng.cols[9:length(livng.cols)]
livng <- hhreg[livng.cols]
row.names(livng) <- hhreg$hhid
# Recode livng vars so that they are presense/absence of each indiv in the 
# household
livng[livng == 1] <- 0 # 1 is originally resp away from household
livng[livng == 2] <- 1
livng[livng == 4] <- 1
livng[livng == 5] <- 1
livng[livng != 1] <- 0

###############################################################################
# Get mean household size for each household for Jan-Dec 2006 (the year t3ag 
# was administered)
monthnum <- 108
for (colnum in 1:ncol(livng)) {
    this.hhsizes <- aggregate(livng[,colnum], by=list(hhid=hhid[,colnum]), sum, na.rm=T)
    names(this.hhsizes) <- c("hhid",  paste("hhsize", monthnum, sep=""))
    if (monthnum == 108) {
        hhsizes <- this.hhsizes
    } else {
        hhsizes <- merge(hhsizes, this.hhsizes)
    }
    monthnum <- monthnum + 1
}
# Drop the first two rows (the missing value code rows):
hhsizes <- hhsizes[-c(1,2),]
hhsize.mean <- apply(hhsizes[2:ncol(hhsizes)], 1, mean, na.rm=T)
hhsize.mean <- data.frame(hhid=hhsizes$hhid, hhsize.mean)


###############################################################################
# Count the number of LD migrants per household, per month, for Jan 2005 - Dec 
# 2006.
migr.jan05.col<- grep('^migr96$', names(mig.dist.merged))
migr <- mig.dist.merged[c(migr.jan05.col:(migr.jan05.col+23))]
hhid.jan05.col<- grep('^hhid96$', names(mig.dist.merged))
hhid <- mig.dist.merged[c(hhid.jan05.col:(hhid.jan05.col+23))]
monthnum <- 96
for (colnum in 1:ncol(migr)) {
    this.numLDmigr <- aggregate(migr[,colnum]=="LD", 
                           by=list(hhid=hhid[,colnum]), sum, na.rm=T)
    names(this.numLDmigr) <- c("hhid",  paste("numLDmigr", monthnum, sep=""))
    if (monthnum == 96) {
        numLDmigr <- this.numLDmigr
    } else {
        numLDmigr <- merge(numLDmigr, this.numLDmigr)
    }
    monthnum <- monthnum + 1
}
# Drop the first two rows (the missing value code rows):
numLDmigr <- numLDmigr[-c(1,2),]
numLDmigr.total <- apply(numLDmigr[2:ncol(numLDmigr)], 1, sum, na.rm=T)
numLDmigr <- data.frame(hhid=numLDmigr$hhid, numLDmigr.total, 
                        anyLDmigr=(numLDmigr.total>=1))

###############################################################################
# Add in other predictors and calculate fuelwood usage
# Dependent variable - fuelwood usage:
#	t3e15.1 - "Since last year till now, approximately, how many bhari, carts, 
#	or quintal of firewood did your household use?"
# 	t3e15.1a - bhari of firewood
# 	t3e15.1b - carts of firewood
# 	t3e15.1c - quintal of firewood
fwusage.bhari<- t3ag$t3e15.1a
fwusage.bhari[is.na(fwusage.bhari)] <- 0
fwusage.cart <- t3ag$t3e15.1b
fwusage.cart[is.na(fwusage.cart)] <- 0
fwusage.quintal <- t3ag$t3e15.1c
fwusage.quintal[is.na(fwusage.quintal)] <- 0
fwusage.kg <- 28.3*fwusage.bhari + 28.3*15*fwusage.cart + 28.3*15*3*fwusage.quintal
# Make a 6 character t3 household ID
t3hhid <- paste(t3ag$t3.neigh, paste('0', t3ag$t3.house, sep=""), sep="")

# Make a "any non-wood fuel source" variable:
anynonwood <- t3ag$t3c31b
anynonwood[t3ag$t3c31b==1] <- 1 # Electricity
anynonwood[t3ag$t3c31c==1] <- 1 # Gas
anynonwood[t3ag$t3c31d==1] <- 1 # Biogas
anynonwood[t3ag$t3c31e==1] <- 1 # Saw-dust
anynonwood[t3ag$t3c31f==1] <- 1 # Kerosene
anynonwood[t3ag$t3c31g==1] <- 1 # Others

income_gt50k <- t3ag$t3g10
income_gt50k[income_gt50k=0] <- NA
income_gt50k[income_gt50k=1] <- 0
income_gt50k[income_gt50k=2] <- 1

fwusage <- data.frame(hhid=t3hhid, fwusage.kg, anynonwood, anywood=t3ag$t3e15, income_gt50k)
fwusage <- merge(fwusage, hhsize.mean)
fwusage <- merge(fwusage, numLDmigr)
fwusage <- cbind(fwusage, fwusage.kg.perday=(fwusage$fwusage.kg/(365 * fwusage$hhsize.mean)))

# Potential predictor variables:
# 	t3a48 - orchard land? (make dichotomous)
# 	t3c31 - what heating sources do you use in your house for cooking?
# 	t3e35 - bring fodder/firewood/thatch from community forest
# 	t3b5 - any livestock
# 	t3c45 - gas stove
# 	ethnic group
# 	HH size
# 	distance from national park
# 	distance from comm. forest
# 	distance from comm. forest
fwpred  <- lm(fwusage.kg.perday ~ hhsize.mean + I(hhsize.mean^2) + anynonwood + anyLDmigr + income_gt50k, data=fwusage)

fwpred  <- lm(fwusage.kg.perday ~ hhsize.mean + I(hhsize.mean^2) + anynonwood + anyLDmigr, data=fwusage)

fwusage.trimmed <- fwusage[fwusage$fwusage.kg.perday<2,]
fwusage.trimmed$fwusage.kg.perday <- fwusage.trimmed$fwusage.kg.perday*2

fwpred.trimmed  <- lm(fwusage.kg.perday ~ hhsize.mean + I(hhsize.mean^2) + anynonwood + anyLDmigr, data=fwusage.trimmed)

plot(fwusage$hhsize.mean, fwusage$fwusage.kg.perday)
plot(fwusage.trimmed$hhsize.mean, fwusage.trimmed$fwusage.kg.perday)
