###############################################################################
# Uses the household registry data to calculate crude birth rates to be used in 
# an OLS model predicting LULC (along the lines of Axinn and Ghimire, 2007)

require(ggplot2)
require(foreign)

# Load the hhreg dataframe (household registry data)
load("/media/Local_Secure/CVFS_HHReg/hhreg126.Rdata")

# Load the neighborhood history data to get distance to Narayanghat (DISTNARA) 
# and strata (STRATA) variables
nbhhist <- read.xport("/media/Local_Secure/ICPSR_0538_Restricted/da04538-0014_REST.xpt")
nbh.vars <- nbhhist[c(3, 1174, 1175)]
names(nbh.vars) <- c("nid", "distnara", "strata")
nbh.vars$nid <- sprintf("%03d", nbh.vars$nid)
nbh.vars$strata <- factor(nbh.vars$strata)
# Convert distance to Narayanghat from miles to kilometers
nbh.vars$distnara <- nbh.vars$distnara / .62
dist.category <- nbh.vars$distnara < 5
dist.category[nbh.vars$distnara >= 5 & nbh.vars$distnara < 10]  <- 2
dist.category[nbh.vars$distnara >= 10 & nbh.vars$distnara < 15]  <- 3
dist.category[nbh.vars$distnara >= 15 & nbh.vars$distnara < 20]  <- 4
dist.category[nbh.vars$distnara >= 20]  <- 5
dist.category <- factor(dist.category, ordered=T)
nbh.vars <- cbind(nbh.vars, dist.category)

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
names(livebirths.nbh) <- gsub("preg", "livebirths", names(livebirths.nbh))
pop.nbh <- sum.over.nbh(hhreg[livng.cols]==2 & in.Chitwan, hhreg[nid.cols],
        names(hhreg[livng.cols]))
names(pop.nbh) <- gsub("livng", "pop", names(pop.nbh))

# Make plots
theme_update(theme_grey(base_size=18))
update_geom_defaults("line", aes(size=.5))

monthlabels <- seq(as.Date("1997/02/01"), as.Date("2007/07/01"),
        by="month")
qplot(monthlabels, apply(pop.nbh[2:127], 2, sum), geom="line", xlab="Year",
        ylab="Total Resident Sample Population")
ggsave("pop.png", width=8.33, height=5.53, dpi=300)

qplot(monthlabels, apply(livebirths.nbh[2:127], 2, sum), geom="line",
        xlab="Year", ylab="Total Number of Births (per month)")
ggsave("births.png", width=8.33, height=5.53, dpi=300)

pop.runavg <- t(apply(pop.nbh[2:127], 1, filter, rep(1/12, 12),
        method="convolution", sides=2))
livebirths.runtotal <- t(apply(livebirths.nbh[2:127], 1, filter,  rep(1, 12),
        method="convolution", sides=2))
crude.birth.rate.nbh <- livebirths.runtotal / (pop.runavg/1000)
crude.birth.rate.avg <- apply(crude.birth.rate.nbh, 2, mean, na.rm=T)

qplot(monthlabels, crude.birth.rate.avg, geom="line", xlab="Year",
        ylab="Crude Birth Rate (per 1000 people per year)")
ggsave("crude_birth_rate.png", width=8.33, height=5.53, dpi=300)

# Convert pop.runavg and livebirths.runtotal to data.frames so they can be merged 
# with the other vital statistics
pop.runavg <- data.frame(pop.runavg)
names(pop.runavg) <- gsub("X", "pop.runavg", names(pop.runavg))
livebirths.runtotal <- data.frame(livebirths.runtotal)
names(livebirths.runtotal) <- gsub("X", "livebirths.runtotal", names(livebirths.runtotal))
smootheddata <- cbind(nid=sprintf("%03d", c(1:151)), pop.runavg, livebirths.runtotal)

# Output a crude birth rate calculated from Feb 1997 - Jan 2002 to use for my 
# LULC OLS statistics
CBR.nbh <- cbind(livebirths.nbh[1], CBR=apply(livebirths.nbh[2:61], 1, sum,
        na.rm=T) / (apply(pop.nbh[,2:61], 1, mean, na.rm=T)/1000))
save(CBR.nbh, file="CBH_nbh.Rdata")

# Check results based on the data used in the Axinn 2007, paper. Mean should be 
# 72.61 births (per thousand people)
#axinn.CBR <- cbind(livebirths.nbh[1], CBR=apply(livebirths.nbh[2:48], 1, sum,
#        na.rm=T) / (apply(pop.nbh[,2:48], 1, mean, na.rm=T)/1000))

# Stack the data so I can look at NBH diffs over time
vital <- merge(nbh.vars, pop.nbh)
vital <- merge(vital, smootheddata)
vital <- merge(vital, livebirths.nbh)
vital <- reshape(vital, direction="long", varying=5:ncol(vital),
        sep="", idvar="nid", ids=vital$nid, timevar="timelabel")
vital$timelabel <- monthlabels[vital$timelabel]

livebirths.time.strata <- aggregate(vital$livebirths.runtotal,
        by=list(timelabel=vital$timelabel, strata=vital$strata), sum)
qplot(timelabel, x, colour=strata, geom="line", data=livebirths.time.strata, 
        xlab="Year", ylab="Annual Number of Live Births")
ggsave("livebirths_strata.png", width=8.33, height=5.53, dpi=300)

livebirths.time.dist <- aggregate(vital$livebirths,
        by=list(timelabel=vital$timelabel, dist.category=vital$dist.category),
        sum, na.rm=T)
qplot(timelabel, x, colour=dist.category, geom="line",
        data=livebirths.time.dist, xlab="Year", ylab="Number of Live Births")
ggsave("livebirths_distance.png", width=8.33, height=5.53, dpi=300)

pop.time.strata <- aggregate(vital$pop, by=list(timelabel=vital$timelabel,
        strata=vital$strata), sum, na.rm=T)
qplot(timelabel, x, colour=strata, geom="line", data=pop.time.strata, 
        xlab="Year", ylab="Resident Sample Population")
ggsave("pop_strata.png", width=8.33, height=5.53, dpi=300)
