#!/usr/bin/env Rscript
###############################################################################
# Loads deaths and age at death from the ICPSR Restricted dataset DS0010, the 
# household registry data, and calculates several statistics (birth rates, 
# marriage rates, and mortality rates) for parameterizing the ChitwanABM model.
###############################################################################

# Hmisc is needed as hhreg is a "labelled" dataframe. If Hmisc is nto included, 
# will get errors saying "cannot coerce class "labelled" into a data.frame"
require(Hmisc, quietly=TRUE)
require(ggplot2, quietly=TRUE)

load("V:/Nepal/CVFS_HHReg/hhreg126.Rdata")
# LAST_MONTH is how many months of the household registry to include (max 
# number of months is 126, so to include all the months set LAST_MONTH to 126).  
# Here LAST_MONTH is set to 60 to leave an independent set of data for model 
# verification and validation - only the first 60 months of the household 
# registry data is used here for parameterization.
LAST_MONTH <- 60
# Drop the appropriate monthly columns if LAST_MONTH is < 126
varying_cols <- grep('^[a-zA-Z]*[1-9][0-9]{0,2}$', names(hhreg))
varying_cols_times <- as.numeric(gsub('[a-zA-Z]', '', names(hhreg)[varying_cols]))
if (LAST_MONTH < max(varying_cols_times)) {
    drop_cols <- varying_cols[varying_cols_times > LAST_MONTH]
    hhreg <- hhreg[-drop_cols]
}

hhreg$gender <- factor(hhreg$gender, labels=c("m", "f"))
hhreg$ethnic <- factor(hhreg$ethnic, levels=c(1,2,3,4,5,6), labels=c("UpHindu",
        "HillTibeto", "LowHindu", "Newar", "TeraiTibeto", "Other"))

# TODO: Need to only consider neighborhoods with neighid < 151

# Function to write out probabilities of events in the format required by the 
# ChitwanABM model.
make_txt_prob <- function(probs, binlims, param.name) {
    # param.name is the name used by the ChitwanABM model for this parameter.
    txtprob <- paste("'", param.name, "' : [{", sep="")
    for (rownum in 1:length(probs)) {
        txtprob <- paste(txtprob, "(", binlims[rownum], ", ",
                binlims[rownum+1], "):", round(probs[rownum], digits=6),
                sep="")
        if (rownum<length(probs)) txtprob <- paste(txtprob, ", ", sep="")
    }
    txtprob <- paste(txtprob, "} | validate_probability(", binlims[1], ", ",
            binlims[length(binlims)], ")]", sep="")
    return(txtprob)
}

make_txt_prob_dist <- function(probs, binlims, param.name) {
    # param.name is the name used by the ChitwanABM model for this parameter.
    binlims <- paste(round(binlims, digits=6), collapse=", ")
    probs <- paste(round(probs, digits=6), collapse=", ")
    txtprob <- paste("'", param.name, "' : [((", binlims, "), (", probs,  "))] | validate_prob_dist]", sep="")
    return(txtprob)
}

# Before the reshape, add a new set of columns (53 columns) coding whether a 
# new marriage occurred prior to the survey in the month. Do this by using the 
# marit columns.
# 	First: recode 1 and 2 (married living with/without spouse) as 2, meaning 
# 	married in that month. Recode 3, 4, and 5 (unmarried, widowed, and 
# 	divorced) as 1, meaning unmarried. Recode 6 (separated, as 4).
#
# 	Next, Subtract the marit1-53 columns from the marit2-54 columns. Now, in 
# 	these new columns:
# 		1 = got married
# 		0 = no change in marital status
# 		-1 = marriage ended
# 		Other numbers have to do with separated -> other status. This is 
# 		ignored for now.
maritcolumns <- grep('^marit[0-9]*$', names(hhreg))
maritstatus <- hhreg[maritcolumns]
maritstatus[maritstatus==-4] <- NA
maritstatus[maritstatus==-3] <- NA
maritstatus[maritstatus==-1] <- NA
maritstatus[maritstatus==1] <- 2
maritstatus[maritstatus==3] <- 1
maritstatus[maritstatus==4] <- 1
maritstatus[maritstatus==5] <- 1
maritstatus[maritstatus==6] <- 4
maritstatus.chg <- maritstatus[2:ncol(maritstatus)] - maritstatus[1:(ncol(maritstatus)-1)]
# Add a column for time 1, which is only NAs as marital status is not known 
# prior to the first month, so no change can be calculated.
maritstatus.chg <- cbind(marit1=matrix(NA, nrow(maritstatus.chg),1), maritstatus.chg)
# Rename the columns to maritchg so they do not interfere with the 'marit' 
# column
names(maritstatus.chg) <- sub('^marit', 'maritchg', names(maritstatus.chg))
names(maritstatus) <- sub('^marit', 'maritstat', names(maritstatus))

hhreg <- cbind(hhreg, maritstatus.chg)
hhreg <- cbind(hhreg, maritstatus)

###############################################################################
# First birth timing
###############################################################################
# Pull out all rows where there was a marriage, and where the person is female.  
# These rows are indicated by 
gotmarried <- (maritstatus.chg==1) & (hhreg$gender=="f")
# Find the index of the first marriage in each row.
marriage_month <- apply(gotmarried, 1, function(x) match(1, x))

# Now find the index of the first birth of each row (only in the rows where 
# there was a marriage)
preg.cols <- grep('^preg[0-9]*$', names(hhreg))
births <- hhreg[preg.cols]
births <- births==3 | births==5
firstbirth_month <- apply(births, 1, function(x) match(TRUE, x))

gotmarried.rows <- marriage_month > 0
gotmarried.rows[is.na(gotmarried.rows)] <- FALSE
firstbirth_times <- firstbirth_month[gotmarried.rows] - marriage_month[gotmarried.rows]
firstbirth_times <- firstbirth_times[!(firstbirth_times<0)]

firstbirthlims <- c(0, 6, 9, 12, 17, 22, 30, 40)
firstbirthbin <- cut(firstbirth_times, firstbirthlims)
firstbirthprob <- data.frame(prob=table(firstbirthbin))
names(firstbirthprob) <- c('bin', 'prob')
firstbirthprob$prob <- firstbirthprob$prob / sum(firstbirthprob$prob)

# Also before the reshape, add a new set of columns coding whether an 
# individual is at risk of giving birth. Only females are at risk of giving 
# birth, and they are not at risk of giving birth for the 9 months before and 
# the 9 months after they give birth. A birth is coded as a 3 (livebirth) or a 
# 5 (stillbirth).  The atrisk.birth variable does NOT account for age 
# limitations on births.  This is taken care of by the data itself.
riskbirth <- hhreg[preg.cols]
riskbirth[hhreg[preg.cols] != 3] <- 1
riskbirth[hhreg[preg.cols] == 3 | hhreg[preg.cols] == 5] <- 0
# Now the month in which a woman gave birth is coded as a 0. Also code as 0 the 
# 9 months before and the 9 months after she gave birth.
atrisk.birth <- riskbirth
for (n in 2:9) {
    # First do 9 months after birth
    cols.mask.forward = n:ncol(atrisk.birth)
    cols.multiplier.forward = 1:(ncol(atrisk.birth) - n +1)
    atrisk.birth[cols.mask.forward] <- atrisk.birth[cols.mask.forward] *
            riskbirth[cols.multiplier.forward]
    # Now do 9 months prior to birth
    cols.mask.back = 1:(ncol(atrisk.birth)-n+1)
    cols.multiplier.back = n:ncol(atrisk.birth)
    atrisk.birth[cols.mask.back] <- atrisk.birth[cols.mask.back] *
            riskbirth[cols.multiplier.back]
}
# Code the first 9 months as zeros as we don't know if a woman gave birth or 
# not (missing the first 9 months of data).
atrisk.birth[1:9] <- 0
atrisk.birth[hhreg$gender != "f",] <- 0
names(atrisk.birth) <- sub('^preg', 'atrisk.birth', names(atrisk.birth))
hhreg <- cbind(hhreg, atrisk.birth)

###############################################################################
# Now do the reshape.
###############################################################################
non_vary_cols <- grep('^(respid|ethnic|gender)$', names(hhreg))
vary_cols <- grep('^(livng|age|preg|marit|maritchg|maritstat|place|atrisk.birth)[0-9]*$', names(hhreg))
events <- reshape(hhreg[c(non_vary_cols, vary_cols)], direction="long",
        varying=names(hhreg[vary_cols]), idvar="respid", timevar="time", sep="")

###############################################################################
# Process age/livngs/hasspouse/marr to recode and remove NAs, etc.
###############################################################################
# Add a new column "has spouse" that is 0 if a person is not married, 1 if a 
# person is married. Do they by recoding the MARIT data. Recode unmarried, 
# widowed, divorced and separated (3, 4, 5 and 6) as 0, and married living with 
# spouse and married not living with spouse (1 and 2) to 1.
events$hasspouse <- events$marit
events$hasspouse[events$hasspouse==2] <- 1
events$hasspouse[events$hasspouse!=1] <- 0

# Here I recode the livng data so any point where an individual was known alive 
# is a 2, the period in which they died is a 3, and all other times (including 
# when they were away (alive/dead unknown) is a 1.
# TODO: Better data could be had be going through and finding people who were 
# away for several months but returned. The months when they were away can be 
# counted as person months in the calculations because, if they returned, it is 
# known that they were alive while they were gone. The current method therefore 
# biases the mortality estimates upwards slightly, by not taking account of 
# months when people who returned to the study were away.
events$death <- events$livng
events$death[events$livng==1] <- 0 # Code away from HH as alive
events$death[events$livng==2] <- 0 # Code in HH as alive
events$death[events$livng==3] <- 1 # Code death as dead
events$death[events$livng==4] <- 0 # Code new member as alive
events$death[events$livng==5] <- 0 # Code HH merged as alive
events$death[events$livng==6] <- 0 # Code first month away as alive
events$death[events$deaths != 0 | events$deaths!=1] <- NA # Code all others as unknown

###############################################################################
# Process deaths.
###############################################################################
# Add a column to store the bin index for each record (determined by the
# person's age).
deathlims <- c(0, 3, 6, 12, 20, 40, 60, 80, 90, 199)
events$deathbin <- cut(events$age, deathlims)
# Then count the number of death events per bin
deaths <- aggregate(events$livng==3, by=list(gender=events$gender,
        deathbin=events$deathbin), sum, na.rm=TRUE)
deathspsnmnths <- aggregate(events$livng==2, by=list(gender=events$gender,
        deathbin=events$deathbin), sum, na.rm=TRUE)
deathprob <- data.frame(gender=deaths$gender, bin=deaths$deathbin,
        prob=(deaths$x/deathspsnmnths$x)*12)

# Pool the death probabilities for age 3-20 for male and female as there are 
# not enough observations for a good gender-specific estimate. Also do this for 
# ages greater than 90.
pooled_death_bins <- c('(3,6]', '(6,12]', '(12,20]', '(90,199]')
for (pooled_bin in pooled_death_bins) {
    deathprob[deathprob$bin==pooled_bin, ]$prob <- with(events[events$deathbin 
            == pooled_bin, ], sum(death, na.rm=TRUE)/(sum(death==0, na.rm=TRUE)/12))
}

###############################################################################
# Process births.
###############################################################################
# Note that hhreg data is only available for pregnancy/births Note that every 
# live birth is a 3 and all other pregnancy statuses (1, 2, 4, 5, and 6) are 
# ignored. Males are always coded as -1 for preg status. NOTE: This means that 
# events$preg is only meaningful in conjunction with events$livng to make sure 
# that only live people are counted, and with events$gender to ensure only 
# women are counted.

# Preg status is only recorded in the hhreg data for women between the ages of 
# 18 and 45.
preglims <- c(15, 16, 18, 20, 23, 26, 30, 35, 40, 45)
events$pregbin <- cut(events$age, preglims)
# Then count the number of births per bin, only considering married women 
# (there are only 2 births to unmarried women)
fecund <- events[events$gender=="f" & !is.na(events$preg),]
births <- with(fecund[fecund$hasspouse==1,], aggregate(preg==3,
        by=list(pregbin=pregbin), sum, na.rm=T))

birthpsnmnths <- aggregate(fecund$atrisk.birth==1,
        by=list(pregbin=fecund$pregbin), sum, na.rm=T)
birthprob <- data.frame(bin=births$pregbin, prob=(births$x/birthpsnmnths$x)*12)

# TODO: Also calculate the proportion of female/male births
###############################################################################
# Process marriages.
###############################################################################
marrlims <- c(15, 18, 22, 30, 40, 60, 199)
events$marrbin <- cut(events$age, marrlims)

events$maritchg[is.na(events$maritchg)] <- 0
marriages <- aggregate(events$maritchg==1, by=list(gender=events$gender,
                marrbin=events$marrbin), sum)
# Remove NAs from maritstat
events$maritstat[is.na(events$maritstat)] <- 0
marrpsnmnths <- aggregate(events$maritstat==1, by=list(gender=events$gender,
        marrbin=events$marrbin), sum)
marrprob <- data.frame(gender=marriages$gender, bin=marriages$marrbin,
        prob=(marriages$x/marrpsnmnths$x)*12)

###############################################################################
# Write out probabilities to text, and plot probabilities
###############################################################################
txtprobs <- c()
txtprobs <- c(txtprobs, make_txt_prob(birthprob$prob, preglims,
        "prob.birth"))
txtprobs <- c(txtprobs, make_txt_prob_dist(firstbirthprob$prob,
        firstbirthlims, "probability.firstbirth_times"))
txtprobs <- c(txtprobs,
        make_txt_prob(deathprob[deathprob$gender=="m",]$prob,
        deathlims, "probability.death.male"))
txtprobs <- c(txtprobs,
        make_txt_prob(deathprob[deathprob$gender=="f",]$prob,
        deathlims, "probability.death.female"))
txtprobs <- c(txtprobs,
        make_txt_prob(marrprob[marrprob$gender=="m",]$prob,
        marrlims, "probability.marriage.male"))
txtprobs <- c(txtprobs,
        make_txt_prob(marrprob[marrprob$gender=="f",]$prob,
        marrlims, "probability.marriage.female"))
write(txtprobs, file="probs.txt")

write.csv(firstbirthprob, file="firstbirth_probs.csv", row.names=FALSE)
write.csv(deathprob[deathprob$gender=="m",], file="death_probs_male.csv", row.names=FALSE)
write.csv(deathprob[deathprob$gender=="f",], file="death_probs_female.csv", row.names=FALSE)
write.csv(marrprob[marrprob$gender=="m",], file="marr_probs_male.csv", row.names=FALSE)
write.csv(marrprob[marrprob$gender=="f",], file="marr_probs_female.csv", row.names=FALSE)
write.csv(deathprob, file="death_probs.csv", row.names=FALSE)
write.csv(marrprob, file="marr_probs.csv", row.names=FALSE)

# Set a color blind compatible palette
theme_update(theme_grey(base_size=16))
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

qplot(bin, prob*100, geom="bar", xlab="Age (years)",
        ylab="Annual Probability of Live Birth (%)",
        data=birthprob)
ggsave("prob_birth.png", width=8.33, height=5.53, dpi=300)

qplot(bin, prob*100, geom="bar", xlab="Time to First Birth (months)",
        ylab="Probability (%)", data=firstbirthprob)
ggsave("prob_first_birth.png", width=8.33, height=5.53, dpi=300)

p <- qplot(gender, prob*100, geom="bar", fill=gender, facets=.~bin,
      xlab="Age (years)", ylab="Annual Probability of Dying (%)", 
      data=deathprob)
p + scale_fill_manual(values=cbPalette)
ggsave("prob_death.png", width=8.33, height=5.53, dpi=300)

p <- qplot(gender, prob*100, geom="bar", fill=gender, facets=.~bin,
      xlab="Age (years)",
      ylab="Annual Probability of Marrying (%)", data=marrprob)
p + scale_fill_manual(values=cbPalette)
ggsave("prob_marriage.png", width=8.33, height=5.53, dpi=300)
