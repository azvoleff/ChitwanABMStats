###############################################################################
# Does a series of models to try to predict fuelwood usage based on household 
# size, etc.
###############################################################################

library(Hmisc)
library(ggplot2)
library(lme4)

PLOT_WIDTH <- 8.33
PLOT_HEIGHT <- 5.53
DPI <-  300
theme_set(theme_grey(base_size=18))

load("W:/Nepal/CVFS_R_format/t3ag.Rdata")
# Need months 108-119 from the household registry (January 2006-December 2006)
load("W:/Nepal/CVFS_HHReg/hhreg126.Rdata")

# Calculate mean household size in 2006 (months 108-119).
hhid.cols <- grep('^hhid1[0-1][0-9]$', names(hhreg))
hhid.cols <- hhid.cols[9:length(hhid.cols)]
hhid <- hhreg[hhid.cols]
age.cols <- grep('^age1[0-1][0-9]$', names(hhreg))
age.cols <- age.cols[9:length(age.cols)]
age <- hhreg[age.cols]
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
# was administered). Also count how many women over age 6 are present in each 
# household.
monthnum <- 108
for (colnum in 1:ncol(livng)) {
    this.hhsizes <- aggregate(livng[,colnum], by=list(hhid=hhid[,colnum]), sum, 
                              na.rm=T)
    names(this.hhsizes) <- c("hhid",  paste("hhsize", monthnum, sep=""))
    # Count the number of women resident within the house, counting only women 
    # within the most likely age range to be helping with fuelwood collection.
    numwomen_collectors <- livng[, colnum] * (hhreg$gender == 1) &
            (age[, colnum] > 8) & (age[, colnum] < 45)
    this.numwomen <- aggregate(numwomen_collectors,
                               by=list(hhid=hhid[,colnum]), sum, na.rm=T)
    names(this.numwomen) <- c("hhid",  paste("numwomen", monthnum, sep=""))
    if (monthnum == 108) {
        numwomen <- this.numwomen
        hhsizes <- this.hhsizes
    } else {
        numwomen <- merge(numwomen, this.numwomen, all=TRUE)
        hhsizes <- merge(hhsizes, this.hhsizes, all=TRUE)
    }
    monthnum <- monthnum + 1
}
# Drop the first two rows (the missing value code rows):
hhsizes <- hhsizes[-c(1,2),]
hhsize_mean <- apply(hhsizes[2:ncol(hhsizes)], 1, mean, na.rm=T)
hhsize_mean <- data.frame(hhid=hhsizes$hhid, hhsize_mean)
# Drop the first two rows (the missing value code rows):
numwomen <- numwomen[-c(1,2),]
numwomen_mean <- apply(numwomen[2:ncol(numwomen)], 1, mean, na.rm=T)
numwomen_mean <- data.frame(hhid=numwomen$hhid, numwomen_mean)

###############################################################################
#  Calculate household ethnicities by taking the mean and rounding
ethnic_hhid <- data.frame(ethnic=hhreg$ethnic, hhid=hhreg$hhid108)
ethnic_hhid <- ethnic_hhid[!is.na(ethnic_hhid$ethnic), ]
ethnic_hhid <- ethnic_hhid[!is.na(ethnic_hhid$hhid), ]
ethnic <- aggregate(ethnic_hhid$ethnic, by=list(hhid=ethnic_hhid$hhid), mean, na.rm=TRUE)
ethnic$x <- round(ethnic$x)
names(ethnic)[names(ethnic) == 'x'] <- 'ethnic'
ethnic$ethnic <- factor(ethnic$ethnic, levels=c(1,2,3,4,5), labels=c("UpHindu",
        "HillTibeto", "LowHindu", "Newar", "TeraiTibeto"))

# Make variable for mean gender under 15
gender_hhid <- data.frame(gender=hhreg$gender, hhid=hhreg$hhid108)
gender_hhid <- gender_hhid[!is.na(gender_hhid$gender), ]
gender_hhid <- gender_hhid[!is.na(gender_hhid$hhid), ]
meangender <- aggregate(gender_hhid$gender, by=list(hhid=gender_hhid$hhid), mean, na.rm=TRUE)
names(meangender)[names(meangender) == 'x'] <- 'gender'

###############################################################################
# Add in forest distances and Narayanghat distances columns
load("W:/Nepal/ICPSR_0538_Restricted/Recode/CVFS_NBHs_forest_distances_recode.Rdata")
forest_dist$NEIGHID <- sprintf("%03i", forest_dist$NEIGHID)

load("W:/Nepal/ICPSR_0538_Restricted/Recode/recoded_NBH_data.Rdata")
nbh_data <- merge(forest_dist, nbh_recode)
columns <- grep('^(NEIGHID|BZ_meters|CNP_meters|closest_type|closest_meters|dist_nara)$', names(nbh_data))
nbh_data <- nbh_data[columns]
nbh_data$closest_type <- factor(nbh_data$closest_type)
nbh_data$closest_km <- nbh_data$closest_meters / 1000

###############################################################################
# Add in other predictors and calculate fuelwood usage
# Dependent variable - fuelwood usage:
#	t3e15.1 - "Since last year till now, approximately, how many bhari, carts, 
#	or quintal of firewood did your household use?"
# 	t3e15.1a - bhari of firewood
# 	t3e15.1b - carts of firewood
# 	t3e15.1c - quintal of firewood
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

fwusage <- data.frame(hhid=t3hhid, NEIGHID=t3ag$t3.neigh, anynonwood, anywood=t3ag$t3e15, income_gt50k, elec_avail=t3ag$t3c30)
fwusage$elec_avail <- factor(fwusage$elec_avail)
fwusage$anywood <- factor(fwusage$anywood)
fwusage <- merge(fwusage, ethnic)
fwusage <- merge(fwusage, meangender)
fwusage <- merge(fwusage, numwomen_mean)
fwusage <- merge(fwusage, hhsize_mean)
fwusage <- merge(fwusage, nbh_data)

# For compatibility with GLMER, remove labels added by Hmisc
for (i in 1:ncol(fwusage)) {
    z<-class(fwusage[[i]])
    if (length(z) == 1 && z =='labelled'){
       class(fwusage[[i]])<- NULL
       attr(fwusage[[i]],'label')<-NULL
    } else if (z[[1]]=='labelled'){
       class(fwusage[[i]])<-z[-1]
       attr(fwusage[[i]],'label')<-NULL
    }
}

fwuse_prob_glm <- glm(anywood ~ hhsize_mean + ethnic + gender + elec_avail + closest_type + closest_km, data=fwusage, family="binomial")
summary(fwuse_prob_glm)
exp(coef(fwuse_prob_glm))

fwusage$numwomen_mean_GT1 <- fwusage$numwomen_mean > 1
fwuse_prob_ml <- glmer(anywood ~ hhsize_mean + ethnic + numwomen_mean_GT1 + 
                       elec_avail + dist_nara + closest_type + (1 | NEIGHID), 
                       data=fwusage, family="binomial")
summary(fwuse_prob_ml)
fwuse_prob_ml_OR <- data.frame(coef=fixef(fwuse_prob_ml), 
                              OR=round(exp(fixef(fwuse_prob_ml)), 4))
(fwuse_prob_ml_OR <- round(fwuse_prob_ml_OR, 4))
write.csv(fwuse_prob_ml_OR, file="fwuse_prob_ml_ODDS.csv")
