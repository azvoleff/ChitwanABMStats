#!/usr/bin/env Rscript
###############################################################################
# Analyzes divorce rates in Chitwan using discrete time event history analysis.  
# Divorce is tracked for a sample of once married women present in Chitwan in 
# 1996. The hazard duration is coded similarly to how Ghimire and Axinn coded 
# first-birth timing, by controlling for the number of months married before 
# the start of observation period, and then having dichotomous 6-month 
# indicators for hazard duration.
###############################################################################

# Hmisc is needed as hhreg is a "labelled" dataframe. If Hmisc is not included, 
# will get errors saying "cannot coerce class "labelled" into a data.frame"
library(Hmisc)
library(ggplot2)
library(foreign)

# LAST_MONTH is how many months of the household registry to include (max 
# number of months is 126, so to include all the months set LAST_MONTH to 126).
LAST_MONTH <- 126 # Yabiku (2006) uses 90 months

###############################################################################
# Recode the data
###############################################################################
print("Loading data...")
load("V:/Nepal/CVFS_HHReg/hhreg126.Rdata")
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
age_cols <- grep('^age[0-9]*$', names(hhreg))

# Load the interpolated LULC data:
load("V:/Nepal/ICPSR_0538_Restricted/Recode/interpolated_percent_agveg.Rdata")
interp_percagveg_cols <- grep('^interp_percagveg[0-9]*$', names(interp_percagveg))
interp_percagveg_cols <- interp_percagveg_cols[1:LAST_MONTH]
interp_logpercagveg <- log(interp_percagveg[interp_percagveg_cols] + 1)
interp_logpercagveg <- data.frame(NEIGHID=interp_percagveg$NEIGHID, interp_logpercagveg)
names(interp_logpercagveg) <- sub("percagveg", "logpercagveg", names(interp_logpercagveg))

###############################################################################
# Process LHC
###############################################################################
lhc <- read.xport('M:/Data/Nepal/CVFS_Public/20120722_Chitwan_Unrestricted_Data/ICPSR_04538/DS0013/04538_0013_data.xpt')
lhc_marr_cols <- grep('^(M1E199[4-9])|(M1E20[0-5][0-9])$',names(lhc))
marr_year <- apply(lhc[lhc_marr_cols], 1,
                    function(marit_row) match(1, marit_row))
mths_marr_pre_1997 <- (length(lhc_marr_cols) - marr_year) * 12 + (13 - lhc$MARR1MTH)
mths_marr_pre_1997[is.na(mths_marr_pre_1997)] <- 0
lhc_age_cols <- grep('^(M1E199[4-9])|(M1E20[0-5][0-9])$',names(lhc))
marr_age_pre97_marriages <- lhc$AGE2053 - (length(lhc_marr_cols) - marr_year)
lhc_vars <- with(lhc, data.frame(RESPID, spouse_2=MYN2, marr_age_pre97_marriages))

t1indiv <- read.xport("V:/Nepal/ICPSR_0538_Restricted/da04538-0012_REST.xpt")
t1indiv <- merge(t1indiv, lhc_vars)
# To merge with the hhreg data, need to convert the old format respondent ID 
# (NNNHHSS) (where NNN is NBH ID, HH is household ID, and SS is subject ID) to 
# NNNHHHSSS:
old_respID <- sprintf("%07i", t1indiv$RESPID)
NBHID <- sprintf("%03i", as.numeric(substr(old_respID,1,3)))
HHID <- sprintf("%03i", as.numeric(substr(old_respID,4,5)))
SUBJID <- sprintf("%03i", as.numeric(substr(old_respID,6,7)))
t1indiv$RESPID <- paste(NBHID, HHID, SUBJID, sep="")
t1indiv_vars <- with(t1indiv, data.frame(respid=RESPID, schooling_yrs=A1, 
                                         had_children=C2, mths_marr_pre_1997, 
                                         spouse_2, marr_age_pre97_marriages))
hhreg <- merge(hhreg, t1indiv_vars, all.x=TRUE)

hhreg$gender <- factor(hhreg$gender, labels=c("male", "female"))
hhreg$ethnic <- factor(hhreg$ethnic, levels=c(1,2,3,4,5,6), labels=c("UpHindu",
        "HillTibeto", "LowHindu", "Newar", "TeraiTibeto", "Other"))

# Clean the data to convert unneeded missing value codes to NAs
hhreg[hhid_cols][hhreg[hhid_cols]=="     A"] <- NA # Inappropriate code is A

# Recode 1, 2, and 6 (married living with/without spouse, and separated) as 0, 
# meaning married in that month, with no divorce. Recode 3, (unmarried) and 4 
# (widowed) as NA, and 5 (divorced) as 1.
divorce_codes <- hhreg[marit_cols]
names(divorce_codes) <- sub('marit', 'divorce', names(divorce_codes))
hhreg <- cbind(hhreg, divorce_codes)
divorce_cols <- grep('^divorce[0-9]*$', names(hhreg))
hhreg[divorce_cols][hhreg[divorce_cols] < 0] <- NA # Missing values
hhreg[divorce_cols][hhreg[divorce_cols] == 1] <- 0 # Married living with spouse
hhreg[divorce_cols][hhreg[divorce_cols] == 2] <- 0 # Married not living with spouse
hhreg[divorce_cols][hhreg[divorce_cols] == 3] <- NA # Unmarried
hhreg[divorce_cols][hhreg[divorce_cols] == 4] <- NA # Widowed
hhreg[divorce_cols][hhreg[divorce_cols] == 5] <- 1 # Divorced
hhreg[divorce_cols][hhreg[divorce_cols] == 6] <- 0 # Separated

# Now only include people who, in 1996, were: 1) local in the Chitwan Valley,
# 2) married, 3) had a valid household ID number, 4) were in their first 
# marriage (spouse_2 == 0).
in_sample <- (hhreg[place_cols[1]] > 1)  & (hhreg[place_cols[1]] <= 502) &
             (hhreg[marit_cols[1]] == 1) & !is.na(hhreg[, hhid_cols[1]])  &
             (hhreg$spouse_2 == 0)
in_sample[is.na(in_sample)] <- FALSE
sample <- hhreg[in_sample, ]
# Save the variables that will be needed later as independent variables, 
# including the neighborhood and household IDs, for each person, and some other 
# covariates.
indepvars <- cbind(respid=hhreg$respid, hhreg[hhid_cols], hhreg[place_cols], 
                   ethnic=hhreg$ethnic, gender=hhreg$gender, hhreg[age_cols], 
                   originalHH=hhreg$hhid1, originalNBH=hhreg$place1, 
                   schooling_yrs=hhreg$schooling_yrs, 
                   had_children=hhreg$had_children)
indepvars <- indepvars[in_sample,]
indepvars <- merge(indepvars, interp_logpercagveg, by.x="originalNBH", by.y="NEIGHID", all.x=TRUE)

###############################################################################
# Censor the data
###############################################################################
# Now censor the data by finding the first divorce activity in each row, and 
# setting every cell in the row after that one to NA. Also censor every cell in 
# a row after the first NA in that row. The min(!is.na below is necessary to 
# find the first match of any type of divorce.
print("Censoring data...")
censor_data <- function(record) {
    first_divorce_column <- na.omit(match(1, record))
    if (length(first_divorce_column)>0) {
        first_divorce_column <- min(first_divorce_column[!is.na(first_divorce_column)])
    } else first_divorce_column <- NA
    first_NA_column <- match(TRUE, is.na(record))
    if (is.na(first_divorce_column) && is.na(first_NA_column)) {
        return(record)
    } else if (is.na(first_divorce_column)) {
        first_censored_col <- first_NA_column
    } else if (is.na(first_NA_column)) {
        first_censored_col <- first_divorce_column + 1
    } else if (first_NA_column < first_divorce_column) {
        first_censored_col <- first_NA_column
    } else {
        first_censored_col <- first_divorce_column + 1
    }
    if (first_censored_col <= length(record)) {
        record[first_censored_col:length(record)] <- NA
    }
    return(record)
}

sample_divorce_cols <- grep('^divorce[0-9]*$', names(sample))
first_divorce <- t(apply(sample[sample_divorce_cols], 1, censor_data))
# Apply returned a matrix, and first_divorce lost its row and column names.  
# Reassign them so first_divorce ends up as the final censored matrix with 
# correct row and column names.
first_divorce <- data.frame(first_divorce)
names(first_divorce) <- sub('divorce', 'first_divorce', names(sample[sample_divorce_cols]))
first_divorce$respid <- sample$respid

# Add a time-varying number of months married variable
sample_marit_cols <- grep('^marit[0-9]*$', names(sample))
n_months_marr <- sample[sample_marit_cols]
# Count months when a woman was widowed, divorced, or separated as 0 months 
# married so they don't mess up the cumulative sum. These months will be 
# excluded from the long dataset later on since the first_divorce indicator 
# will be set to NA in that month in the censor function.
n_months_marr[is.na(sample[sample_marit_cols])] <- 0
# Take account of months of marriage before the start of the dataset for women 
# married pre-1997:
n_months_marr[, 1] <- n_months_marr[, 1] + sample$mths_marr_pre_1997
n_months_marr <- as.data.frame(t(apply(n_months_marr, 1, cumsum)))
names(n_months_marr) <- paste('n_months_marr', seq(1, length(sample_marit_cols)), sep="")
n_months_marr$respid <- sample$respid
indepvars <- merge(indepvars, n_months_marr)

# Add age at first-marriage variable:
sample_age_cols <- grep('^(age)[0-9]*$', names(sample))
sample_first_marr_col <- apply(sample[sample_marit_cols], 1,
                    function(marit_row) match(1, marit_row))
indepvars$age_at_first_marr <- mapply(function(age_row, marr_col) age_row[marr_col],
                        as.list(as.data.frame(t(sample[sample_age_cols]))),
                        sample_first_marr_col)
pre_97_rows <- !is.na(sample$marr_age_pre97_marriages)
indepvars$age_at_first_marr[pre_97_rows] <- sample$marr_age_pre97_marriages[pre_97_rows]

###############################################################################
# Output the data
###############################################################################
print("Outputting censored data...")

# First output in wide format
# Add columns with neighborhood and household ID, ethnicity, age, sex, and 
# hhid.
first_divorce_wide <- merge(indepvars, first_divorce, by="respid", all.x=F, all.y=T)
# Need to order the data properly for it to be used in MLwiN
first_divorce_wide <- first_divorce_wide[order(first_divorce_wide$respid, first_divorce_wide$originalHH, first_divorce_wide$originalNBH),]
save(first_divorce_wide, file=paste("data/divorce_data-wideformat-up_to_month_", LAST_MONTH, ".Rdata", sep=""))
write.csv(first_divorce_wide, file=paste("data/divorce_data-wideformat-up_to_month_", LAST_MONTH, ".csv", sep=""), row.names=FALSE)

# Now in long format
first_divorce_cols <- grep('^first_divorce[0-9]*$', names(first_divorce_wide))
age_cols <- grep('^age[0-9]*$', names(first_divorce_wide))
hhid_cols <- grep('^hhid[0-9]*$', names(first_divorce_wide))
logpercagveg_cols <- grep('^interp_logpercagveg[0-9]*$', names(first_divorce_wide))
place_cols <- grep('^place[0-9]*$', names(first_divorce_wide))
# Now construct the long-format dataset
first_divorce_long <- reshape(first_divorce_wide, idvar="respid", 
                             varying=list(first_divorce_cols, age_cols,
                                          hhid_cols, place_cols, 
                                          logpercagveg_cols), 
                      v.names=c("first_divorce", "age", "hhid", "place", 
                                "interp_logpercagveg"),
                             direction="long", sep="")
first_divorce_long <- first_divorce_long[!is.na(first_divorce_long$first_divorce),]
first_divorce_long <- first_divorce_long[order(first_divorce_long$respid, first_divorce_long$originalHH, first_divorce_long$originalNBH),]
save(first_divorce_long, file=paste("data/divorce_data-longformat-up_to_month_", LAST_MONTH, ".Rdata", sep=""))
write.csv(first_divorce_long, file=paste("data/divorce_data-longformat-up_to_month_", LAST_MONTH, ".csv", sep=""), row.names=FALSE)
