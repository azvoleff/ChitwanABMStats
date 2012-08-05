#!/usr/bin/env Rscript
###############################################################################
# Encodes marriage data, as right censored data. Only includes individuals 
# present in 1996. Encodes wide and long format person-month dataset for later 
# analysis with glmer and or MLwiN.
# 
# Follows analysis of Yabiku, 2006:
#     Yabiku, S. T. 2006. Land use and marriage timing in Nepal. Population & 
#     Environment 27 (5):445-461.
###############################################################################

# Hmisc is needed as hhreg is a "labelled" dataframe. If Hmisc is not included, 
# will get errors saying "cannot coerce class "labelled" into a data.frame"
library(Hmisc)
library(ggplot2)
library(foreign)

# LAST_MONTH is how many months of the household registry to include (max 
# number of months is 126, so to include all the months set LAST_MONTH to 126).
LAST_MONTH <- 60 # Yabiku (2006) uses 90 months

###############################################################################
# Recode the data as necessary and setup the marit_status matrix for later 
# censoring.
###############################################################################
print("Loading data...")
#load("V:/Nepal/CVFS_HHReg/hhreg126.Rdata")
load("/media/truecrypt1/Nepal/CVFS_HHReg/hhreg126.Rdata")
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
#load("V:/Nepal/ICPSR_0538_Restricted/Recode/interpolated_percent_agveg.Rdata")
load("/media/truecrypt1/Nepal/ICPSR_0538_Restricted/Recode/interpolated_percent_agveg.Rdata")
interp_percagveg_cols <- grep('^interp_percagveg[0-9]*$', names(interp_percagveg))
interp_percagveg_cols <- interp_percagveg_cols[1:LAST_MONTH]
interp_logpercagveg <- log(interp_percagveg[interp_percagveg_cols] + 1)
interp_logpercagveg <- data.frame(NEIGHID=interp_percagveg$NEIGHID, interp_logpercagveg)
names(interp_logpercagveg) <- sub("percagveg", "logpercagveg", names(interp_logpercagveg))
# Convert to numeric (1-3 digit) neighborhood ID so it will merge properly with 
# the other data:
interp_logpercagveg$NEIGHID <- as.numeric(interp_logpercagveg$NEIGHID)

#t1indiv <- read.xport("V:/Nepal/ICPSR_0538_Restricted/da04538-0012_REST.xpt")
t1indiv <- read.xport("/media/truecrypt1/Nepal/ICPSR_0538_Restricted/da04538-0012_REST.xpt")
# To merge with the hhreg data, need to convert the old format respondent ID 
# (NNNHHSS) (where NNN is NBH ID, HH is household ID, and SS is subject ID) to 
# NNNHHHSSS:
old_respID <- sprintf("%07i", t1indiv$RESPID)
NBHID <- sprintf("%03i", as.numeric(substr(old_respID,1,3)))
HHID <- sprintf("%03i", as.numeric(substr(old_respID,4,5)))
SUBJID <- sprintf("%03i", as.numeric(substr(old_respID,6,7)))
t1indiv$RESPID <- paste(NBHID, HHID, SUBJID, sep="")
educdata <- with(t1indiv, data.frame(respid=RESPID, schooling_yrs=A1))
hhreg <- merge(hhreg, educdata, all.x=TRUE)

hhreg$gender <- factor(hhreg$gender, labels=c("male", "female"))
hhreg$ethnic <- factor(hhreg$ethnic, levels=c(1,2,3,4,5,6), labels=c("UpHindu",
        "HillTibeto", "LowHindu", "Newar", "TeraiTibeto", "Other"))

# Clean the data to convert unneeded missing value codes to NAs
hhreg[hhid_cols][hhreg[hhid_cols]=="     A"] <- NA # Inappropriate code is A

# Recode 1 and 2 (married living with/without spouse) as 1, meaning married in 
# that month. Recode 3, (unmarried) as 0, and 4, 5 and 6 (widowed, divorced, 
# separated) as NA.
hhreg[marit_cols][hhreg[marit_cols] < 0] <- NA
hhreg[marit_cols][hhreg[marit_cols]==1] <- 1
hhreg[marit_cols][hhreg[marit_cols]==2] <- 1
hhreg[marit_cols][hhreg[marit_cols]==3] <- 0
hhreg[marit_cols][hhreg[marit_cols]==4] <- NA
hhreg[marit_cols][hhreg[marit_cols]==5] <- NA
hhreg[marit_cols][hhreg[marit_cols]==6] <- NA

marit_status <- hhreg[marit_cols]
row.names(marit_status) <- hhreg$respid

# TODO: Check if Yabiku only included people local in 1996 - the paper is not 
# clear on this point.
# Now only include people who, in 1996, were: 1) local in the Chitwan Valley,
# 2) older than 15, 3) younger than 20 (per Yabiku, 2006), 4) unmarried, 5) had 
# a valid household ID number.
#(places[,1] > 1) & (places[,1] <= 502)
in_sample <- (hhreg[age_cols[1]] >=15)   & (hhreg[age_cols[1]] <= 20) &
             (hhreg[place_cols[1]] > 1)  & (hhreg[place_cols[1]] <= 502) &
             (hhreg[marit_cols[1]] == 0) & !is.na(hhreg[,hhid_cols[1]])
in_sample[is.na(in_sample)] <- FALSE
marit_status <- marit_status[in_sample,]
# Save the variables that will be needed later as independent variables, 
# including the neighborhood and household IDs, for each person, and some other 
# covariates.
indepvars <- cbind(respid=hhreg$respid, hhreg[hhid_cols], hhreg[place_cols], ethnic=hhreg$ethnic, gender=hhreg$gender, hhreg[age_cols], originalHH=hhreg$hhid1, originalNBH=hhreg$place1, schooling_yrs=hhreg$schooling_yrs)
indepvars <- indepvars[in_sample,]
indepvars <- merge(indepvars, interp_logpercagveg, by.x="originalNBH", by.y="NEIGHID", all.x=TRUE)

###############################################################################
# Censor the data
###############################################################################
# Now censor the data by finding the first marriage activity in each row, and 
# setting every cell in the row after that one to NA. Also censor every cell in 
# a row after the first NA in that row. The min(!is.na below is necessary to 
# find the first match of any type of marriage.
print("Censoring data...")
censor_data <- function(record) {
    first_marr_column <- na.omit(match(1, record))
    if (length(first_marr_column)>0) {
        first_marr_column <- min(first_marr_column[!is.na(first_marr_column)])
    } else first_marr_column <- NA
    first_NA_column <- match(TRUE, is.na(record))
    if (is.na(first_marr_column) && is.na(first_NA_column)) {
        return(record)
    } else if (is.na(first_marr_column)) {
        first_censored_col <- first_NA_column
    } else if (is.na(first_NA_column)) {
        first_censored_col <- first_marr_column + 1
    } else if (first_NA_column < first_marr_column) {
        first_censored_col <- first_NA_column
    } else {
        first_censored_col <- first_marr_column + 1
    }
    if (first_censored_col <= length(record)) {
        record[first_censored_col:length(record)] <- NA
    }
    return(record)
}
marit_status_temp <- t(apply(marit_status, 1, censor_data))
# Apply returned a matrix, and marit_status_temp lost its row and column names.  
# Reassign them so marit_status ends up as the final censored matrix with correct 
# row and column names.
marit_status <- marit_status_temp
marit_status_temp <- data.frame(marit_status_temp, row.names=row.names(marit_status))
names(marit_status_temp) <- names(hhreg[marit_cols])
marit_status <- marit_status_temp

# Add respid and column to marit_status dataframe (row.names were assigned earlier 
# for this dataframe).
marit_status <- data.frame(respid=row.names(marit_status), marit_status)

###############################################################################
# Output the data
###############################################################################
print("Outputting censored data...")

# First output in wide format
# Add columns with neighborhood and household ID, ethnicity, age, sex, and 
# hhid.
marit_wide <- merge(indepvars, marit_status, by="respid", all.x=F, all.y=T)
# Need to order the data properly for it to be used in MLwiN
marit_wide <- marit_wide[order(marit_wide$respid, marit_wide$originalHH, marit_wide$originalNBH),]
save(marit_wide, file=paste("data/marriage_data-wideformat-up_to_month_", LAST_MONTH, ".Rdata", sep=""))
write.csv(marit_wide, file=paste("data/marriage_data-wideformat-up_to_month_", LAST_MONTH, ".csv", sep=""), row.names=FALSE)

# Now in long format
marit_cols <- grep('^marit[0-9]*$', names(marit_wide))
age_cols <- grep('^age[0-9]*$', names(marit_wide))
hhid_cols <- grep('^hhid[0-9]*$', names(marit_wide))
logpercagveg_cols <- grep('^interp_logpercagveg[0-9]*$', names(marit_wide))
place_cols <- grep('^place[0-9]*$', names(marit_wide))
# Now construct the long-format dataset
marit_long <- reshape(marit_wide, idvar="respid", 
                             varying=list(marit_cols, age_cols,
                                          hhid_cols, place_cols, 
                                          logpercagveg_cols), 
                      v.names=c("marit", "age", "hhid", "place", 
                                "interp_logpercagveg"),
                             direction="long", sep="")
marit_long <- marit_long[!is.na(marit_long$marit),]
marit_long <- marit_long[order(marit_long$respid, marit_long$originalHH, marit_long$originalNBH),]
save(marit_long, file=paste("data/marriage_data-longformat-up_to_month_", LAST_MONTH, ".Rdata", sep=""))
write.csv(marit_long, file=paste("data/marriage_data-longformat-up_to_month_", LAST_MONTH, ".csv", sep=""), row.names=FALSE)
