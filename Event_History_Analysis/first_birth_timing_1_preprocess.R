#!/usr/bin/env Rscript
###############################################################################
# Encodes first_preg data, as right censored data. Only includes individuals 
# present in 1996. Encodes wide and long format person-month dataset for later 
# analysis with glmer and or MLwiN.
# 
# Follows Ghimire and Hoelter (2007) and Ghimire and Axinn (2010):
# 	Ghimire, D. J., and L. F. Hoelter. 2007. Land use and first birth timing in 
# 	an agricultural setting. Population & Environment 28:289-320.
# 	Ghimire, D. J., and W. G. Axinn. 2010. Community context, land use, and 
# 	first birth. Rural Sociology 75 (3):478-513.
#     
###############################################################################

# Hmisc is needed as hhreg is a "labelled" dataframe. If Hmisc is not included, 
# will get errors saying "cannot coerce class "labelled" into a data.frame".
library(Hmisc)
library(ggplot2)
library(foreign)

# Months.total is how many months of the household registry to include (max 
# number of months is 126, so to include all the months set LAST_MONTH to 126).
LAST_MONTH <- 72

print("Loading data...")
load("T:/Nepal/CVFS_HHReg/hhreg126.Rdata")
#load("/media/truecrypt1/Nepal/CVFS_HHReg/hhreg126.Rdata")
# Drop the appropriate monthly columns if LAST_MONTH is < 126
varying_cols <- grep('^[a-zA-Z]*[1-9][0-9]{0,2}$', names(hhreg))
varying_cols_times <- as.numeric(gsub('[a-zA-Z]', '', names(hhreg)[varying_cols]))
if (LAST_MONTH < max(varying_cols_times)) {
    drop_cols <- varying_cols[varying_cols_times > LAST_MONTH]
    hhreg <- hhreg[-drop_cols]
}
hhid_cols <- grep('^hhid[0-9]*$', names(hhreg))
marit_cols <- grep('^(marit)[0-9]*$', names(hhreg))
place_cols <- grep('^(place)[0-9]*$', names(hhreg))
age_cols <- grep('^(age)[0-9]*$', names(hhreg))

###############################################################################
# Process LHC
###############################################################################
lhc <- read.xport('G:/Data/Nepal/CVFS_Public/20120722_Chitwan_Unrestricted_Data/ICPSR_04538/DS0013/04538_0013_data.xpt')
#lhc <- read.xport('/media/Zvoleff_Passport/Data/Nepal/CVFS_Public/20120722_Chitwan_Unrestricted_Data/ICPSR_04538/DS0013/04538_0013_data.xpt')
lhc_marr_cols <- grep('^(M1E199[4-9])|(M1E20[0-5][0-9])$', names(lhc))
marr_year <- apply(lhc[lhc_marr_cols], 1,
                    function(marit_row) match(1, marit_row))
mths_marr_pre_1997 <- (length(lhc_marr_cols) - marr_year) * 12 + (13 - lhc$MARR1MTH)
mths_marr_pre_1997[is.na(mths_marr_pre_1997)] <- 0
lhc_age_cols <- grep('^(M1E199[4-9])|(M1E20[0-5][0-9])$',names(lhc))
marr_age_pre97_marriages <- lhc$AGE2053 - (length(lhc_marr_cols) - marr_year)
# The BCYN variables are whether respondent and respondent's spouse have ever 
# been sterilized.
lhc_vars <- with(lhc, data.frame(RESPID, ever_ster=BCYN8, sp_ever_ster=BCYN9,
                                 spouse_2=MYN2, marr_age_pre97_marriages))

t1indiv <- read.xport("T:/Nepal/ICPSR_0538_Restricted/da04538-0012_REST.xpt")
#t1indiv <- read.xport("/media/truecrypt1/Nepal/ICPSR_0538_Restricted/da04538-0012_REST.xpt")
t1indiv <- merge(t1indiv, lhc_vars)
# To merge with the hhreg data, need to convert the old format respondent ID 
# (NNNHHSS) (where NNN is NBH ID, HH is household ID, and SS is subject ID) to 
# NNNHHHSSS:
old_respID <- sprintf("%07i", t1indiv$RESPID)
NBHID <- sprintf("%03i", as.numeric(substr(old_respID, 1, 3)))
HHID <- sprintf("%03i", as.numeric(substr(old_respID, 4, 5)))
SUBJID <- sprintf("%03i", as.numeric(substr(old_respID, 6, 7)))
t1indiv$RESPID <- paste(NBHID, HHID, SUBJID, sep="")
t1indiv_vars <- with(t1indiv, data.frame(respid=RESPID, schooling_yrs=A1, 
                                         had_children=C2, ever_ster, 
                                         sp_ever_ster, mths_marr_pre_1997, 
                                         spouse_2, marr_age_pre97_marriages))
hhreg <- merge(hhreg, t1indiv_vars, all.x=TRUE)

# Code "had children" and ever sterilized variables as -1 for people who were 
# not interviewed in 1996, so new respondents who were not interviewed in 1996 
# won't be excluded from the data.
#hhreg$had_children[hhreg$individl != 1] <- -1
#hhreg$ever_ster[hhreg$individl != 1] <- -1
#hhreg$sp_ever_ster[hhreg$individl != 1] <- -1

hhreg$gender <- factor(hhreg$gender, labels=c("male", "female"))
hhreg$ethnic <- factor(hhreg$ethnic, levels=c(1,2,3,4,5,6), labels=c("UpHindu",
        "HillTibeto", "LowHindu", "Newar", "TeraiTibeto", "Other"))

# Clean the data to convert unneeded missing value codes to NAs
hhreg[hhid_cols][hhreg[hhid_cols]=="     A"] <- NA # Inappropriate code is A

# Recode 1 and 2 (married living with/without spouse) as 1, meaning married in 
# that month. Recode 3, (unmarried) as 0, and 4, 5 and 6 (widowed, divorced, 
# separated) as NA.
hhreg[marit_cols][hhreg[marit_cols] < 0] <- NA # Missing values
hhreg[marit_cols][hhreg[marit_cols] == 1] <- 1 # Married living with spouse
hhreg[marit_cols][hhreg[marit_cols] == 2] <- 1 # Married not living with spouse
hhreg[marit_cols][hhreg[marit_cols] == 3] <- 0 # Unmarried
hhreg[marit_cols][hhreg[marit_cols] == 4] <- NA # Widowed
hhreg[marit_cols][hhreg[marit_cols] == 5] <- NA # Divorced
hhreg[marit_cols][hhreg[marit_cols] == 6] <- NA # Separated

###############################################################################
# Construct sample
###############################################################################
# Following Ghimire and Hoelter (2007), the sample is all women "aged 15–29 in 
# 1996 who had not previously given birth and who were neither sterilized nor 
# married to men who were sterilized". Also ensure that women were local in 
# 1996, and had not have been married more than once as of 1996
in_sample <- (hhreg[age_cols[1]] >=15)     & (hhreg[age_cols[1]] <= 29) &
             (hhreg[place_cols[1]] > 1)    & (hhreg[place_cols[1]] <= 151) &
             !is.na(hhreg[, hhid_cols[1]]) & (hhreg$gender == "female") &
             (hhreg$had_children != 1)     & (hhreg$sp_ever_ster != 1) &
             (hhreg$ever_ster != 1)        & (hhreg$spouse_2 == 0)
table(in_sample, exclude=NULL)
#table(hhreg$had_children, exclude=NULL)
#table(hhreg$sp_ever_ster, exclude=NULL)
#table(hhreg$ever_ster, exclude=NULL)
in_sample[is.na(in_sample)] <- FALSE
sample <- hhreg[in_sample, ]

# Save the variables that will be needed later as independent variables, 
# including the neighborhood and household IDs, for each person, and some other 
# covariates.
indepvars <- cbind(respid=hhreg$respid, hhreg[hhid_cols], hhreg[place_cols], ethnic=hhreg$ethnic, gender=hhreg$gender, hhreg[age_cols], originalHH=hhreg$hhid1, originalNBH=hhreg$place1, schooling_yrs=hhreg$schooling_yrs, mths_marr_pre_1997=hhreg$mths_marr_pre_1997)
indepvars <- indepvars[in_sample,]

###############################################################################
# Censor the data
###############################################################################
# Now censor the data by finding the first birth activity in each row, and 
# setting every cell in the row after that one to NA. Also censor every cell in 
# a row after the first NA in that row.
print("Censoring data...")
censor_data <- function(first_marr_col, last_marr_col, preg_record) {
    first_birth_column <- na.omit(match(1, preg_record))
    if (length(first_birth_column) > 0) {
        first_birth_column <- min(first_birth_column[!is.na(first_birth_column)])
    } else first_birth_column <- NA
    first_NA_column <- match(TRUE, is.na(preg_record))
    # First censor all observations up until the first marriage, and censor 
    # after dissolution of first marriage
    if (is.na(first_marr_col) | (first_marr_col > (length(preg_record) - 8))) {
        # If respondent never married, or married within the last 8 months of 
        # data collection, set whole preg_record to NA:
        return(rep(NA, length(preg_record)))
    }
    else if (first_marr_col != 1) {
        preg_record[1:(first_marr_col - 1)] <- NA
    }
    if (!is.na(last_marr_col) && last_marr_col < length(preg_record)) {
        preg_record[last_marr_col:length(preg_record)] <- NA
    }
    # Now censor all observations after 1st pregnancy resulting in a live 
    # birth, or after first NA column in the pregnancy data. First pregnancy is 
    # defined as the month 9 months prior to the first live birth.  The 
    # min(!is.na below is necessary to find a match of any type of live birth.
    if (is.na(first_birth_column) && is.na(first_NA_column)) {
        return(preg_record)
    } else if (is.na(first_birth_column)) {
        first_censored_col <- first_NA_column
    } else if (is.na(first_NA_column)) {
        first_censored_col <- first_birth_column - 8
    } else if (first_NA_column < first_birth_column) {
        first_censored_col <- first_NA_column
    } else {
        first_censored_col <- first_birth_column - 8
    }
    if (first_censored_col >= 1) {
        preg_record[first_censored_col:length(preg_record)] <- NA
    }
    if (!is.na(first_birth_column) && ((first_birth_column - 8) >= 1)) {
        preg_record[first_birth_column - 8] <- 1
    }
    return(preg_record)
}
sample_preg_cols <- grep('^(preg)[0-9]*$', names(sample))
sample_marr_cols <- grep('^(marit)[0-9]*$', names(sample))
first_marr_col <- apply(sample[sample_marr_cols], 1,
                    function(marit_row) match(1, marit_row))
last_marr_col <- apply(sample[sample_marr_cols], 1,
    function(marit_row) (length(marit_row) - match(1, rev(marit_row)) + 1))
live_births <- sample[sample_preg_cols]
live_births <- (live_births == 3) | (live_births == 5)
first_preg <- t(mapply(censor_data, first_marr_col, last_marr_col,
                         as.list(as.data.frame(t(live_births)))))
# Apply returned a matrix, and lost its row and column names.  Reassign them so 
# first_preg ends up as the final censored matrix with correct row and column 
# names.
first_preg <- data.frame(first_preg)
names(first_preg) <- sub('preg', 'first_preg', names(sample[sample_preg_cols]))
first_preg$respid <- sample$respid

# Add a time-varying number of months married variable
n_months_marr <- sample[sample_marr_cols]
# Count months when a woman was widowed, divorced, or separated as 0 months 
# married so they don't mess up the cumulative sum. These months will be 
# excluded from the long dataset later on since the first_preg indicator will 
# be set to NA in that month in the censor function.
n_months_marr[is.na(sample[sample_marr_cols])] <- 0
# Take account of months of marriage before the start of the dataset for women 
# married pre-1997:
n_months_marr[, 1] <- n_months_marr[, 1] + sample$mths_marr_pre_1997
n_months_marr <- as.data.frame(t(apply(n_months_marr, 1, cumsum)))
names(n_months_marr) <- paste('n_months_marr', seq(1, length(sample_marr_cols)), sep="")
n_months_marr$respid <- sample$respid
indepvars <- merge(indepvars, n_months_marr)

# Add age at first-marriage variable:
sample_age_cols <- grep('^(age)[0-9]*$', names(sample))
indepvars$age_at_first_marr <- mapply(function(age_row, marr_col) age_row[marr_col],
                        as.list(as.data.frame(t(sample[sample_age_cols]))),
                        first_marr_col)
pre_97_rows <- !is.na(sample$marr_age_pre97_marriages)
indepvars$age_at_first_marr[pre_97_rows] <- sample$marr_age_pre97_marriages[pre_97_rows]

###############################################################################
# Output the data
###############################################################################
print("Outputting censored data...")
# First output in wide format
# Add columns with neighborhood and household ID, ethnicity, age, sex, and 
# hhid.
first_preg_wide <- merge(indepvars, first_preg, by="respid", all.x=F, all.y=T)
# Need to order the data properly for it to be used in MLwiN
first_preg_wide <- first_preg_wide[order(first_preg_wide$respid, first_preg_wide$originalHH, first_preg_wide$originalNBH),]
save(first_preg_wide, file=paste("data/first_preg_data-wideformat-up_to_month_", LAST_MONTH, ".Rdata", sep=""))
write.csv(first_preg_wide, file=paste("data/first_preg_data-wideformat-up_to_month_", LAST_MONTH, ".csv", sep=""), row.names=FALSE)

# Now in long format
first_preg_cols <- grep('^first_preg[0-9]*$', names(first_preg_wide))
age_cols <- grep('^age[0-9]*$', names(first_preg_wide))
hhid_cols <- grep('^hhid[0-9]*$', names(first_preg_wide))
place_cols <- grep('^place[0-9]*$', names(first_preg_wide))
n_months_marr_cols <- grep('^n_months_marr[0-9]*$', names(first_preg_wide))
# Now construct the long-format dataset
first_preg_long <- reshape(first_preg_wide, idvar="respid", 
                             varying=list(first_preg_cols, age_cols,
                                          hhid_cols, place_cols, 
                                          n_months_marr_cols), 
                             v.names=c("first_preg", "age", "hhid", "place", 
                                       "n_months_marr"),
                             direction="long", sep="")
first_preg_long <- first_preg_long[!is.na(first_preg_long$first_preg),]
first_preg_long <- first_preg_long[order(first_preg_long$respid, first_preg_long$originalHH, first_preg_long$originalNBH),]
save(first_preg_long, file=paste("data/first_preg_data-longformat-up_to_month_", LAST_MONTH, ".Rdata", sep=""))
write.csv(first_preg_long, file=paste("data/first_preg_data-longformat-up_to_month_", LAST_MONTH, ".csv", sep=""), row.names=FALSE)
