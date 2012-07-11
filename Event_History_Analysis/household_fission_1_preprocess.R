#!/usr/bin/env Rscript
###############################################################################
# Analyzes household fission, using data from the CVFS monthly household 
# registry. Uses as its sample all present locally in the first month of 1996.  
# The process is as follows:
#   1) Find first marriage month
#   2) Find spouseID
#   3) Find HHID of self in first marriage month
#   3) Find HHID of spouse in first marriage month
#   4) Look 3, 6, 9, 12, 15, 18, 21, and 24 months out from marriage
#       a) Does self's HHID change?
#       b) Does spouse's HHID change?
#   5) Store date of change of HHID
#       a) Are parents of either spouse in the new household?
# 
# NOTE: This analysis uses the output of marriage_timing_1_preprocess.R.
###############################################################################

# Hmisc is needed as hhreg is a "labelled" dataframe. If Hmisc is not included, 
# will get errors saying "cannot coerce class "labelled" into a data.frame"
library(Hmisc)
library(ggplot2)
library(foreign)

load("data/marriage_data-longformat-up_to_month_90.Rdata")

# First find those who have married:
marit_long <- marit_long[marit_long$marit==1,]

dominant_household <- function(hh_row) {
    # Function that takes in a row of household IDs, and returns the dominant 
    # HHID (whatever household a person spent the majority of the time in 
    # during that period. If their time was split evenly between two 
    # households, return NA.
    months_per_HH <- sapply(all_HHs, function(HHID) sum(hh_row == HHID, na.rm=TRUE))
    dominant_HH <- all_HHs[months_per_HH == max(months_per_HH)]
    if (length(dominant_HH) > 1) return(NA)
    return(dominant_HH)
}

make_HHID_window <- function(hh_row, center, width) {
    # Makes a "window" of household IDs centered on a selected month. Allows 
    # making a wide dataset of marriages, with a series of household IDs for 
    # each row with equal width preceding to and following a marriage. Once 
    # this window is created, the above "dominant_household" function can be 
    # used to determine if a change in household ID ocurred following a 
    # marriage.
    if (!is.null(dim(hh_row))) stop("hh_row cannot have more than 1 dimension")
    if (((center - width) < 1) || ((center + width) > length(hh_row)))
        return(NA)
    hh_row <- hh_row[(center-width):(center+width)]
}
