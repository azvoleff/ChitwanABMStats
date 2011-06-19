#!/usr/bin/env Rscript
###############################################################################
# Calculates the percentage of men and percentage of women that move to their 
# new spouses household - based off of marriage_analysis_SA_prep.R
###############################################################################

# Hmisc is needed as hhreg is a "labelled" dataframe. If Hmisc is nto included, 
# will get errors saying "cannot coerce class "labelled" into a data.frame"
require(Hmisc)

load("/media/Local_Secure/CVFS_R_format/hhreg.Rdata")
load("t1_lulc.Rdata")

hhreg$gender <- factor(hhreg$gender, labels=c("male", "female"))
hhreg$ethnic <- factor(hhreg$ethnic, levels=c(1,2,3,4,5,6), labels=c("UpHindu",
        "HillTibeto", "LowHindu", "Newar", "TeraiTibeto", "Other"))

# Recode 1 and 2 (married living with/without spouse) as 1, meaning married in 
# that month. Recode 3, (unmarried) as 0, and 4, 5 and 6 (widowed, divorced, 
# separated) as NA.
maritcolumns <- grep('^(marit)[0-9]*$', names(hhreg))

hhreg[maritcolumns][hhreg[maritcolumns] < 0] <- NA
hhreg[maritcolumns][hhreg[maritcolumns]==1] <- 1
hhreg[maritcolumns][hhreg[maritcolumns]==2] <- 1
hhreg[maritcolumns][hhreg[maritcolumns]==3] <- 0
hhreg[maritcolumns][hhreg[maritcolumns]==4] <- NA
hhreg[maritcolumns][hhreg[maritcolumns]==5] <- NA
hhreg[maritcolumns][hhreg[maritcolumns]==6] <- NA

# Eliminate where there were no months in which the individual was married or 
# unmarried (meaning they were widowed, divorced, or separated the entire time.
good_rows <- apply(hhreg[maritcolumns], 1, function(x) match(c(0,1), x, nomatch=NA))
bad_rows <- is.na(good_rows[1,]) & is.na(good_rows[2,])
hhreg <- hhreg[!bad_rows,]

marriage_month <- apply(hhreg[maritcolumns], 1, function(x) match(1, x, nomatch=NA))
marriage_outcome <- marriage_month
# If the individual was married after month 1, then code this as a 1 (event 
# occurred at an observed timestep).
marriage_outcome[marriage_month>1] <- 1
# NAs mean that there was never a marriage
marriage_outcome[is.na(marriage_month)] <- 0

# Set marriage_month 
marriage_month[is.na(marriage_month)] <- length(maritcolumns)

status_age <- hhreg$agelt + marriage_month/12

# Set the start time and end time to be in months since age 15 (when 
# individuals are first exposed to the hazard of marriage).
# The start time is equal to the time of the first observation, which is the 
# first 0 or 1 to appear in the marit status
start_time <- apply(hhreg[maritcolumns], 1, function(x) match(c(0,1), x, nomatch=999))
start_time <- as.numeric((hhreg$agelt-15)*12) + apply(start_time, 2, min)
end_time <- as.numeric((hhreg$agelt-15)*12 + marriage_month+1)

place_cols <- grep('^(place)[0-9]*$', names(hhreg))
hhid_cols <- grep('^(hhid)[0-9]*$', names(hhreg))

# Assign each individual the hhid and placeid they were assigned for the month 
# that they married.
hhid <- c()
place <- c()
for (n in 1:nrow(hhreg)) {
    place_col <- place_cols[marriage_month[n]]
    place <- c(place, hhreg[n, place_col])
    hhid_col <- hhid_cols[marriage_month[n]]
    hhid <- c(hhid, hhreg[n, hhid_col])
}

stop()
# Now find if/when their hhid changes.
for (rownum in nrow(hhreg)) {
    if


}

respid_col<- grep('^(respid)]*$', names(hhreg))
agelt_col<- grep('^(agelt)]*$', names(hhreg))
gender_col<- grep('^(gender)]*$', names(hhreg))
ethnic_col<- grep('^(ethnic)]*$', names(hhreg))
marriages <- data.frame(respid=hhreg[respid_col], hhid, place,
        marriage_outcome, agelt=hhreg[agelt_col], gender=hhreg[gender_col],
        ethnic=hhreg[ethnic_col], marriage_month, status_age, start_time,
        end_time)

marriages <- marriages[marriages$place<=151 & marriages$place>=1,]
names(marriages)[grep('place', names(marriages))] <- "nid"

marriages <- merge(marriages, lulc)

save(marriages, file="marriage_times.Rdata")

# Censor data: add NAs for every month after the first marriage
#for (rownum in 1:nrow(hhreg)) {
#    for (colnum in maritcolumns[2:length(maritcolumns)]) {
#        if (hhreg[rownum, colnum-1]==1 | is.na(hhreg[rownum, colnum-1])) {
#            hhreg[rownum, colnum] <- NA
#        }
#    }
#}

###############################################################################
# Now do the reshape.
###############################################################################
# Find the column indices of all columns that are needed
#varying_cols <- grep('^(marit|age|hhid|place)[0-9]*$', names(hhreg))
#non_varying_cols <- grep('^(gender|ethnic|respid|agelt)]*$', names(hhreg))

# Reshape age Include columns 1, 3, and 4 as these are respid, ethnic, and 
# gender, respectively.
#events <- reshape(hhreg[c(non_varying_cols, varying_cols)], direction="long",
        #        varying=names(hhreg[varying_cols]), idvar="respid", timevar="time", 
        #        sep="")

#events <- events[events$place<=151 & events$place>=1,]
#names(events)[grep('place', names(events))] <- "nid"

#events <- merge(events, lulc)

#save(events, file="marriage_events_censored.Rdata")
