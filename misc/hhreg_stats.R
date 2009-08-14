###############################################################################
# Loads deaths and age at death from the ICPSR Restricted dataset DS0010, the 
# household registry data.
###############################################################################
library(Hmisc) # contains label function
load("/media/Restricted/Data/CVFS_R_format/hhreg.Rdata")

columns <- grep('^(livng|age|preg|marit|place|spous|hhid)[0-9]*$', names(hhreg))

hhreg$gender <- factor(hhreg$gender, labels=c("male", "female"))

# Reshape age and livngs. Include columns 1, 3, and 4 as these are respid, 
# ethnic, and gender, respectively.
hhreg.reshaped <- reshape(hhreg[c(1, 3, 4, columns)], direction="long",
        varying=names(hhreg[columns]), idvar="respid", timevar="time", sep="")

events <- hhreg.reshaped

# Here I recode the livng data so any point where an individual was known alive 
# is a 2, the period in which they died is a 3, and all other times (including 
# when they were away (alive/dead unknown) is a 1.
# TODO: Better data could be had be going through and finding people who were 
# away for several months but returned. The months when they were away can be 
# counted as person months in the calculations because, if they returned, it is 
# known that they were alive while they were gone. The current method therefore 
# biases the mortality estimates upwards slightly, by not taking account of 
# months when people who returned to the study were away.
events$livng[events$livng==4] <- 2 # Code new HH member as alive
events$livng[events$livng==5] <- 2 # Code HH merged as alive
events$livng[events$livng==6] <- 1 # Code away from HH as unknown
events$livng[is.na(events$livng)] <- 1 # Code NA as unknown
events$livng[events$livng != 2 & events$livng != 3] <- 1 # Code all others as unknown

# Drop rows where livng is unknown
events <- events[-which(events$livng==1),]

# Drop rows where age is unknown
events <- events[-which(events$age<0),]

# Drop the one individual where ethnicity is missing
events <- events[-which(is.na(events$ethnic)),]

# Add a column to store the bin index for each record (determined by the
# person's age).
events <- cbind(events, deathbin=matrix(NA,nrow(events),1))

bin.deathlims <- c(0, 3, 6, 12, 20, 30, 40, 50, 60, 70, 80, 90, 100)
# First count number of person months in each bin
for (binnum in 1:length(bin.deathlims)) {
    # Works only because it reassigns binindices as it goes. Not the most 
    # efficient way of doing things.
    events[events$age>bin.deathlims[binnum],]$deathbin <- binnum
}

# Then count the number of death events per bin
deaths <- aggregate(events$livng==3, by=list(gender=events$gender,
                deathbin=events$deathbin), sum)

# Now process preg. Note that hhreg data is only available for pregnancy/births 
# Note that every live birth is a 3 and all other pregnancy statuses (1, 2, 4, 
# 5, and 6) are ignored. Males are always coded as -1 for preg status. NOTE: 
# This means that events$preg is only meaningful in conjunction with 
# events$livng to make sure that only live people are counted, and with 
# events$gender to ensure only women are counted.

# Preg status is only recorded in the hhreg data for women between the ages of 
# 18 and 45
events <- cbind(events, pregbin=matrix(NA,nrow(events),1))
bin.preglims <- c(0, 14, 16, 18, 20, 25, 30, 35, 40, 45)
for (binnum in 1:length(bin.preglims)) {
    events[events$age>bin.preglims[binnum],]$pregbin <- binnum
}
# Then count the number of death events per bin
births <- with(events[events$gender=="female" & !is.na(events$preg),],
        aggregate(preg==3, by=list(pregbin=pregbin), sum))
