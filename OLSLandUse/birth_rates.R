###############################################################################
# Uses the household registry data to calculate crude birth rates to be used in 
# an OLS model predicting LULC (along the lines of Axinn and Ghimire, 2007)

require(ggplot2)
require(foreign)

# Load the hhreg dataframe (household registry data)
load("/media/Local_Secure/CVFS_HHReg/hhreg126.Rdata")

# First make an indicator for each time period of the neighborhood
hhid.cols <- grep('^hhid[0-9]*$', names(hhreg))
nids <- sapply(hhreg[hhid.cols], substr, 1, 3)
nids <- apply(nids, 2, as.numeric)
nids <- data.frame(nids)
names(nids) <- gsub('hhid', 'nid', names(nids))
hhreg <- cbind(hhreg, nids)
nid.cols <- grep('^nid[0-9]*$', names(hhreg))

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
# and for BIRTHS , only include the live births (preg coded as 3)
preg.cols <- grep('^preg[0-9]*$', names(hhreg))
place.cols <- grep('^place[0-9]*$', names(hhreg))

# Make an indicator for if a person is a current resident of Chitwan residing 
# in a survey household
in.Chitwan <- hhreg[livng.cols]==2 & hhreg[place.cols]<=151

livebirths.total <- apply(hhreg[preg.cols]==3 & in.Chitwan, 2, sum, na.rm=TRUE)
pop.total <- apply(in.Chitwan, 2, sum, na.rm=TRUE)

# Now calculate the crude birth rate for each neighborhood, equal to the mean 
# number of persons in the neighborhood divided by the total
sum.over.nbh <- function (datacols, factorcols, columnnames) {
    summatrix <- data.frame(nid=sprintf("%03d", c(1:151)), matrix(0, 151, 126))
    names(summatrix) <- c("nid", columnnames)
    for (n in 1:126) {
        sum.by.nbh <- aggregate(datacols[,n], by=list("nid"=factorcols[,n]),
                sum, na.rm=T)
        # Need to use n+1 so that we don't wipe out the first (nid) column
        summatrix[sum.by.nbh$nid,n+1] <- sum.by.nbh$x
    }
    return(summatrix)
}

livebirths.nbh <- sum.over.nbh(hhreg[preg.cols]==3 & in.Chitwan,
        hhreg[nid.cols], names(hhreg[preg.cols]))
pop.nbh <- sum.over.nbh(hhreg[livng.cols]==2 & in.Chitwan, hhreg[nid.cols],
        names(hhreg[livng.cols]))

monthlabels <- seq(as.Date("1997/02/01"), as.Date("2007/07/01"),
        by="month")
qplot(monthlabels, apply(pop.nbh[2:127], 2, sum), geom="line", xlab="Year",
        ylab="Total Population Size")
ggsave("pop.png", width=8.33, height=5.53, dpi=150)

qplot(monthlabels, apply(livebirths.nbh[2:127], 2, sum), geom="line",
        xlab="Year", ylab="Total Number of Births (per month)")
ggsave("births.png", width=8.33, height=5.53, dpi=150)

pop.runavg <- t(apply(pop.nbh[2:127], 1, filter, rep(1/12, 12),
        method="convolution", sides=2))
births.runtotal <- t(apply(livebirths.nbh[2:127], 1, filter,  rep(1, 12),
        method="convolution", sides=2))
crude.birth.rate.nbh <- births.runtotal / (pop.runavg/1000)
crude.birth.rate.avg <- apply(crude.birth.rate.nbh, 2, mean, na.rm=T)

qplot(monthlabels, crude.birth.rate.avg, geom="line", xlab="Year",
        ylab="Crude Birth Rate (per 1000 people per year)")
ggsave("crude_birth_rate.png", width=8.33, height=5.53, dpi=150)

# Output a crude birth rate calculated from Feb 1997 - Jan 2002 to use for my 
# LULC OLS statistics
CBR.nbh <- cbind(livebirths.nbh[1], CBR=apply(livebirths.nbh[2:61], 1, sum,
        na.rm=T) / (apply(pop.nbh[,2:61], 1, mean, na.rm=T)/1000))
save(CBR.nbh, file="CBH_nbh.Rdata")

# Check results based on the data used in the Axinn 2007, paper. Mean should be 
# 72.61 births (per thousand people)
axinn.CBR <- cbind(livebirths.nbh[1], CBR=apply(livebirths.nbh[2:48], 1, sum,
        na.rm=T) / (apply(pop.nbh[,2:48], 1, mean, na.rm=T)/1000))
