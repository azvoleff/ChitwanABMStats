#!/usr/bin/env Rscript
###############################################################################
# Encodes marriage data, as right censored data. Only includes individuals 
# present in 1996. Encodes wide and long format person-month dataset for later 
# analysis with glmer and or MLwiN.
# 
# Follows Ghimire and Hoelter (2007) and Ghimire and Axinn (2010):
# 	Ghimire, D. J., and L. F. Hoelter. 2007. Land use and first birth timing in 
# 		an agricultural setting. Population & Environment 28:289–320.
# 	Ghimire, D. J., and W. G. Axinn. 2010. Community context, land use, and 
# 		first birth. Rural Sociology 75 (3):478–513.
#     
###############################################################################

# Hmisc is needed as hhreg is a "labelled" dataframe. If Hmisc is not included, 
# will get errors saying "cannot coerce class "labelled" into a data.frame"
library(Hmisc)
library(ggplot2)
library(foreign)

# Months.total is how many months of the household registry to include (max 
# number of months is 126, so to include all the months set LAST_MONTH to 126).
LAST_MONTH <- 90 # Yabiku (2006) uses 90 months

print("Loading data...")
load("V:/Nepal/CVFS_HHReg/hhreg126.Rdata")
# Drop the appropriate monthly columns if LAST_MONTH is < 126
varying_cols <- grep('^[a-zA-Z]*[1-9][0-9]{0,2}$', names(hhreg))
varying_cols_times <- as.numeric(gsub('[a-zA-Z]', '', names(hhreg)[varying_cols]))
if (LAST_MONTH < max(varying_cols_times)) {
    drop_cols <- varying_cols[varying_cols_times > LAST_MONTH]
    hhreg <- hhreg[-drop_cols]
}

preg_cols <- grep('^preg[0-9]*$', names(hhreg))
hhid_cols <- grep('^hhid[0-9]*$', names(hhreg))
marit_cols <- grep('^(marit)[0-9]*$', names(hhreg))
place_cols <- grep('^(place)[0-9]*$', names(hhreg))
age_cols <- grep('^age[0-9]*$', names(hhreg))

# Load the LULC data:
lu <- read.xport("V:/Nepal/ICPSR_SupplementalData/Survey_converted/landuse.xpt")
land_agveg_t1 <- with(lu, rowSums(cbind(BARI1, IKHET1, RKHET1)))
land_nonagveg_t1 <- with(lu, rowSums(cbind(GRASSC1, GRASSP1, PLANTC1, PLANTP1)))
land_privbldg_t1 <- with(lu, rowSums(cbind(HHRESID1, MILL1, OTRBLD1)))
land_pubbldg_t1 <- with(lu, rowSums(cbind(ROAD1, SCHOOL1, TEMPLE1)))
land_other_t1 <- with(lu, rowSums(cbind(CANAL1, POND1, RIVER1, SILT1, UNDVP1)))
lu_t1 <- data.frame(NEIGHID=lu$NEIGHID, land_agveg=land_agveg_t1, 
                    land_nonagveg=land_nonagveg_t1, 
                    land_privbldg=land_privbldg_t1, 
                    land_pubbldg=land_pubbldg_t1, land_other=land_other_t1)
# Convert land areas expressed in square feet to square meters
lu_t1[2:6]  <- lu_t1[2:6] * .09290304
lu_t1$NEIGHID <- as.numeric(lu_t1$NEIGHID)
lu_t1$land_total <- apply(lu_t1[2:6], 1, sum)
lu_t1$percagveg <- with(lu_t1, (land_agveg/land_total)*100)

land_agveg_t2 <- with(lu, rowSums(cbind(BARI2, IKHET2, RKHET2)))
land_nonagveg_t2 <- with(lu, rowSums(cbind(GRASSC2, GRASSP2, PLANTC2, PLANTP2)))
land_privbldg_t2 <- with(lu, rowSums(cbind(HHRESID2, MILL2, OTRBLD2)))
land_pubbldg_t2 <- with(lu, rowSums(cbind(ROAD2, SCHOOL2, TEMPLE2)))
land_other_t2 <- with(lu, rowSums(cbind(CANAL2, POND2, RIVER2, SILT2, UNDVP2)))
lu_t2 <- data.frame(NEIGHID=lu$NEIGHID, land_agveg=land_agveg_t2, 
                    land_nonagveg=land_nonagveg_t2, 
                    land_privbldg=land_privbldg_t2, 
                    land_pubbldg=land_pubbldg_t2, land_other=land_other_t2)
# Convert land areas expressed in square feet to square meters
lu_t2[2:6]  <- lu_t2[2:6] * .09290304
lu_t2$NEIGHID <- as.numeric(lu_t2$NEIGHID)
lu_t2$land_total <- apply(lu_t2[2:6], 1, sum)
lu_t2$percagveg <- with(lu_t2, (land_agveg/land_total)*100)

# Now make linear interpolation from month 1 up to month LAST_MONTH, in wide format, 
# for each neighborhood. Note that 40 months is the average time between the T1 
# and T2 mapping.
rt_chg <- (lu_t2$percagveg - lu_t1$percagveg) / 40
rt_chg_matrix <- matrix(rep(rt_chg,LAST_MONTH), nrow=nrow(lu_t2))
initial_percagveg <- matrix(rep(lu_t1$percagveg,LAST_MONTH), ncol=LAST_MONTH)
months_matrix <- matrix(seq(1, LAST_MONTH), ncol=LAST_MONTH, nrow=nrow(lu_t2), byrow=TRUE)
interp_percagveg <- initial_percagveg + (rt_chg_matrix * months_matrix)
interp_percagveg[interp_percagveg < 0] <- 0
interp_percagveg[interp_percagveg > 100] <- 100
interp_logpercagveg <- log(interp_percagveg + 1)
interp_logpercagveg <- data.frame(NEIGHID=lu_t2$NEIGHID, interp_logpercagveg)
names(interp_logpercagveg)[2:ncol(interp_logpercagveg)] <- paste("logpercagveg", seq(1:LAST_MONTH), sep="")

# Load the life history calendar data to see if women/spouses have ever been 
# sterilized.
lhc <- read.xport('M:/Data/Nepal/CVFS_Public/20120722_Chitwan_Unrestricted_Data/ICPSR_04538/DS0013/04538_0013_data.xpt')
lhc_vars <- with(lhc, data.frame(RESPID, ever_ster=BCYN8, sp_ever_ster=BCYN9))

t1indiv <- read.xport("V:/Nepal/ICPSR_0538_Restricted/da04538-0012_REST.xpt")
t1indiv <- merge(t1indiv, lhc_vars)

# To merge with the hhreg data, need to convert the old format respondent ID 
                 # (NNNHHSS) (where NNN is NBH ID, HH is household ID, and SS 
                 # is subject ID) to NNNHHHSSS:
old_respID <- sprintf("%07i", t1indiv$RESPID)
NBHID <- sprintf("%03i", as.numeric(substr(old_respID, 1, 3)))
HHID <- sprintf("%03i", as.numeric(substr(old_respID, 4, 5)))
SUBJID <- sprintf("%03i", as.numeric(substr(old_respID, 6, 7)))
t1indiv$RESPID <- paste(NBHID, HHID, SUBJID, sep="")
t1indiv_vars <- with(t1indiv, data.frame(respid=RESPID, schooling_yrs=A1, 
                                         had_children=C2, ever_ster, 
                                         sp_ever_ster))

hhreg <- merge(hhreg, t1indiv_vars, all.x=TRUE)
# Code "had children" and ever sterilized variables as -1 for people who were 
# not interviewed in 1996, so new respondents who were not interviewed in 1996 
# won't be excluded from the data.
hhreg$had_children[hhreg$individl != 1] <- -1
hhreg$ever_ster[hhreg$individl != 1] <- -1
hhreg$sp_ever_ster[hhreg$individl != 1] <- -1

hhreg$gender <- factor(hhreg$gender, labels=c("male", "female"))
hhreg$ethnic <- factor(hhreg$ethnic, levels=c(1,2,3,4,5,6), labels=c("UpHindu",
        "HillTibeto", "LowHindu", "Newar", "TeraiTibeto", "Other"))

# Clean the data to convert unneeded missing value codes to NAs
hhreg[hhid_cols][hhreg[hhid_cols]=="     A"] <- NA # Inappropriate code is A

# Recode 1 and 2 (married living with/without spouse) as 1, meaning married in 
# that month. Recode 3, (unmarried) as 0, and 4, 5 and 6 (widowed, divorced, 
# separated) as NA.
hhreg[marit_cols][hhreg[marit_cols] < 0] <- NA
hhreg[marit_cols][hhreg[marit_cols] == 1] <- 1
hhreg[marit_cols][hhreg[marit_cols] == 2] <- 1
hhreg[marit_cols][hhreg[marit_cols] == 3] <- 0
hhreg[marit_cols][hhreg[marit_cols] == 4] <- NA
hhreg[marit_cols][hhreg[marit_cols] == 5] <- NA
hhreg[marit_cols][hhreg[marit_cols] == 6] <- NA

###############################################################################
# Construct sample
###############################################################################
# Following Ghimire and Hoelter (2007), the sample is all women "aged 15–29 in 
# 1996 who had not previously given birth and who were neither sterilized nor 
# married to men who were sterilized". Also ensure that women were local in 
# 1996.
in_sample <- (hhreg[age_cols[1]] >=15)     & (hhreg[age_cols[1]] <= 29) &
             (hhreg[place_cols[1]] > 1)    & (hhreg[place_cols[1]] <= 151) &
             !is.na(hhreg[, hhid_cols[1]]) & (hhreg$gender == "female") &
             (hhreg$had_children != 1)  & (hhreg$sp_ever_ster != 1) &
             (hhreg$ever_ster != 1)
table(in_sample, exclude=NULL)
#table(hhreg$had_children, exclude=NULL)
#table(hhreg$sp_ever_ster, exclude=NULL)
#table(hhreg$ever_ster, exclude=NULL)
in_sample[is.na(in_sample)] <- FALSE
sample <- hhreg[in_sample, ]

# Save the variables that will be needed later as independent variables, 
# including the neighborhood and household IDs, for each person, and some other 
# covariates.
indepvars <- cbind(respid=hhreg$respid, hhreg[hhid_cols], hhreg[place_cols], ethnic=hhreg$ethnic, gender=hhreg$gender, hhreg[age_cols], originalHH=hhreg$hhid1, originalNBH=hhreg$place1, schooling_yrs=hhreg$schooling_yrs)
indepvars <- indepvars[in_sample,]
indepvars <- merge(indepvars, interp_logpercagveg, by.x="originalNBH", by.y="NEIGHID", all.x=TRUE)

###############################################################################
# Censor the data
###############################################################################
# Now censor the data by finding the first birth activity in each row, and 
# setting every cell in the row after that one to NA. Also censor every cell in 
# a row after the first NA in that row.
print("Censoring data...")
censor_data <- function(first_marr_column, record) {
    first_birth_column <- na.omit(match(1, record))
    if (length(first_birth_column) > 0) {
        first_birth_column <- min(first_birth_column[!is.na(first_birth_column)])
    } else first_birth_column <- NA
    first_NA_column <- match(TRUE, is.na(record))
    # First censor all observations up until the first marriage
    if (is.na(first_marr_column) | (first_marr_column > (length(record) - 8))) {
        # If respondent never married, or married within the last 8 months of 
        # data collection, set whole record to NA:
        return(rep(NA, length(record)))
    }
    else if (first_marr_column != 1) {
        record[1:(first_marr_column - 1)] <- NA
    }
    # Now censor all observations after 1st pregnancy resulting in a live 
    # birth, or after first NA column in the pregnancy data. First pregnancy is 
    # defined as the month 9 months prior to the first live birth.  The 
    # min(!is.na below is necessary to find a match of any type of live birth.
    if (is.na(first_birth_column) && is.na(first_NA_column)) {
        return(record)
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
        record[first_censored_col:length(record)] <- NA
    }
    if (!is.na(first_birth_column) && ((first_birth_column - 8) >= 1)) {
        record[first_birth_column - 8] <- 1
    }
    return(record)
}
sample_preg_cols <- grep('^(preg)[0-9]*$', names(sample))
sample_marr_cols <- grep('^(marit)[0-9]*$', names(sample))
marr_month <- apply(sample[sample_marr_cols], 1,
                    function(marit_row) match(1, marit_row))
live_births <- sample[sample_preg_cols]
live_births <- (live_births == 3) | (live_births == 5)
first_births <- t(mapply(censor_data, marr_month, 
                             as.list(as.data.frame(t(live_births)))))
# Apply returned a matrix, and lost its row and column names.  Reassign them so 
# first_births ends up as the final censored matrix with correct row and column 
# names.
first_births <- data.frame(first_births, row.names=row.names(sample))
names(first_births) <- sub('preg', 'first_birth', names(sample[sample_preg_cols]))
first_births$respid=row.names(first_births)

###############################################################################
# Output the data
###############################################################################
print("Outputting censored data...")

# First output in wide format
# Add columns with neighborhood and household ID, ethnicity, age, sex, and 
# hhid.
first_births_wide <- merge(indepvars, first_births, by="respid", all.x=F, all.y=T)
# Need to order the data properly for it to be used in MLwiN
first_births_wide <- first_births_wide[order(first_births_wide$respid, first_births_wide$originalHH, first_births_wide$originalNBH),]
save(first_births_wide, file=paste("data/marriage_data-wideformat-up_to_month_", LAST_MONTH, ".Rdata", sep=""))
write.csv(first_births_wide, file=paste("data/marriage_data-wideformat-up_to_month_", LAST_MONTH, ".csv", sep=""), row.names=FALSE)

# Now in long format
first_births_cols <- grep('^first_birth[0-9]*$', names(first_births_wide))
age_cols <- grep('^age[0-9]*$', names(first_births_wide))
hhid_cols <- grep('^hhid[0-9]*$', names(first_births_wide))
logpercagveg_cols <- grep('^logpercagveg[0-9]*$', names(first_births_wide))
place_cols <- grep('^place[0-9]*$', names(first_births_wide))
# Now construct the long-format dataset
first_births_long <- reshape(first_births_wide, idvar="respid", 
                             varying=list(first_births_cols, age_cols,
                                          hhid_cols, place_cols, 
                                          logpercagveg_cols), 
                      v.names=c("first_birth", "age", "hhid", "place", 
                                "logpercagveg"),
                             direction="long", sep="")
first_births_long <- first_births_long[!is.na(first_births_long$first_birth),]
first_births_long <- first_births_long[order(first_births_long$respid, first_births_long$originalHH, first_births_long$originalNBH),]
save(first_births_long, file=paste("data/marriage_data-longformat-up_to_month_", LAST_MONTH, ".Rdata", sep=""))
write.csv(first_births_long, file=paste("data/marriage_data-longformat-up_to_month_", LAST_MONTH, ".csv", sep=""), row.names=FALSE)
