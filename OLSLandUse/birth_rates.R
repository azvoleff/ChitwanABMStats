###############################################################################
# This calculates an OLS model land use, broken into 3 categories, based on the 
# calculations in Table 5 of Axinn and Ghimire, PSC Research Report 07-617 
# (2007). It uses several household and neighborhood level predictors.
#
# LULC categories:
# 	Buildings and infrastructure
# 	Agricultural land area
# 	Non-agricultural land area

library("foreign")
t1.hhag <- read.xport("/media/Local_Secure/ICPSR_0538_Restricted/da04538-0002_REST.xpt")
t1.indivcensus <- read.xport("/media/Local_Secure/ICPSR_0538_Restricted/da04538-0004_REST.xpt")
# Load the hhreg dataframe (household registry data)
#if (!exists("hhreg.reshaped")) load("/media/Local_Secure/CVFS_HHRe/hhreg_reshaped.Rdata")
if (!exists("hhreg")) {
    load("/media/Local_Secure/CVFS_HHReg/hhreg126.Rdata")

    # First make an indicator for each time period of the neighborhood
    hhid.cols <- grep('^hhid[0-9]*$', names(hhreg))
    nids <- sapply(hhreg[hhid.cols], substr, 1, 3)
    nids <- apply(nids, 2, as.numeric)
    nids <- data.frame(nids)
    names(nids) <- gsub('hhid', 'nid', names(nids))
    hhreg <- cbind(hhreg, nids)
    nid.cols <- grep('^nid[0-9]*$', names(hhreg))
}
lulc <- read.xport("/media/Local_Secure/ICPSR_SupplementalData/Survey_converted/landuse.xpt")
lulc <- lulc[order(names(lulc))]
lulc <- lulc[c(8, 24, 49, 1:7, 9:23, 25:48, 50:62)]
t1.indivhist <- read.xport("/media/Local_Secure/ICPSR_0538_Restricted/da04538-0014_REST.xpt")

###############################################################################
# For hhreg:
# I only want data where PLACE1-PLACE126 (columns 227-273 of the dataframe) is 
# between 1-151 (only the CVFS sample neighborhoods). The data should then be 
# aggregated for each column from PLACE1-PLACE126 by this neighborhood ID.  
# Persons per neighborhood can be gotten from LIVNG1-126 (columns 117-163 of 
# the dataframe,  where the variable has a value of 2 (living in HH) or 4 (new 
# HH member living in HH)) and births per neighborhood can be gotten from 
# PREG1-126 (columns 281-327 of the dataframe, where it takes on a value of 3 
# for a live birth).
#
# NOTE: Months 1-126 correspond to February 1997 - December 2000. Still need to 
# figure out which neighborhoods to exclude (there should only be 138 not 151).

# Only include data within neighborhoods 1-151

# TODO: Add strata id key

# For POP only include current HH residents (livng coded as 2 or 4)
livng.cols <- grep('^livng[0-9]*$', names(hhreg))
pop <- apply(hhreg[livng.cols]==2, 2, sum, na.rm=TRUE)
# and for BIRTHS , only include the live births (preg coded as 3)
preg.cols <- grep('^preg[0-9]*$', names(hhreg))
place.cols <- grep('^place[0-9]*$', names(hhreg))
livebirths.place <- apply(hhreg[place.cols] <= 151 & hhreg[preg.cols]==3, 2, sum, na.rm=TRUE)
# Now aggregate the data by neighborhood for BIRTHS and POP (use tapply to do 
# this)
#month <- t(array(1:126, dim=c(126, nrow(hhreg))))

# Now calculate the crude birth rate for each neighborhood, equal to the mean 
# number of persons in the neighborhood divided by the total
sum.over.nbh <- function (datacols, factorcols, columnnames) {
    summatrix <- data.frame(nid=c(1:151), matrix(0, 151, 126))
    names(summatrix) <- c("nid", columnnames)
    for (n in 1:126) {
        sum.by.nbh <- aggregate(datacols[,n], by=list("nid"=factorcols[,n]),
                sum, na.rm=T)
        # Need to use n+1 so that we don't wipe out the first (nid) column
        summatrix[sum.by.nbh$nid,n+1] <- sum.by.nbh$x
    }
    return(summatrix)
}
births.nbh <- sum.over.nbh(hhreg[preg.cols]==3 & hhreg[livng.cols]==2,
        hhreg[nid.cols], names(hhreg[preg.cols]))
pop.nbh <- sum.over.nbh(hhreg[livng.cols]==2, hhreg[nid.cols],
        names(hhreg[livng.cols]))
crude.nbh <- births.nbh
crude.nbh[2:127] <- crude.nbh[2:127] / (pop.nbh[2:127] / 1000)

pdf()
plot(birth.nbh
