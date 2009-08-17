###############################################################################
# Loads deaths and age at death from the ICPSR Restricted dataset DS0010, the 
# household registry data, and calculates several statistics (birth rates, 
# marriage rates, and mortality rates) for parameterizing the ChitwanABM model.
###############################################################################
library(Hmisc) # contains label function
load("/media/Restricted/Data/CVFS_R_format/hhreg.Rdata")

# Function to write out probabilities of events in the format required by the 
# ChitwanABM model.
make.txthazard <- function(probs, binlims, param.name) {
    # param.name is the name used by the ChitwanABM model for this parameter.
    txthazard <- paste("'", param.name, "' : [{", sep="")
    for (rownum in 1:length(probs)) {
        txthazard <- paste(txthazard, "(", binlims[rownum], ", ",
                binlims[rownum+1], "):", round(probs[rownum], digits=4),
                sep="")
        if (rownum<length(probs)) txthazard <- paste(txthazard, ", ", sep="")
    }
    txthazard <- paste(txthazard, "} | validate_hazard(", binlims[1], ", ",
            binlims[length(binlims)], ")]", sep="")
    return(txthazard)
}

# Before the reshape, add a new set of columns (53 columns) coding whether a 
# new marriage occurred prior to the survey in the month. Do this by using the 
# marit columns.
# 	First: recode 1 and 2 (married living with/without spouse) as 1, meaning 
# 	married in that month. Recode 3, 4, and 5 (unmarried, widowed, and 
# 	divorced) as 2, meaning unmarried. Recode 6 (separated, as 4).
#
# 	Next, Subtract the marit1-53 columns from the marit2-54 columns. Now, in 
# 	these new columns:
# 		0 = no change in marital status
# 		-1 = marriage ended
# 		1 = got married
# 		Other numbers have to do with separated -> other status. This is 
# 		ignored for now.
maritcolumns <- grep('^marit[0-9]*$', names(hhreg))
maritstatus <- hhreg[maritcolumns]
maritstatus[maritstatus==-4] <- NA
maritstatus[maritstatus==-3] <- NA
maritstatus[maritstatus==-1] <- NA
# Change maritchg NAs to 100
events$maritchg[is.na(events$maritchg)] <- 100
maritstatus[maritstatus==2] <- 1
maritstatus[maritstatus==3] <- 2
maritstatus[maritstatus==4] <- 2
maritstatus[maritstatus==5] <- 2
maritstatus[maritstatus==6] <- 4
maritstatus.chg <- maritstatus[2:54] - maritstatus[1:53]
# Add a column for time 1, which is only NAs as marital status is not known 
# prior to the first month, so no change can be calcualted.
maritstatus.chg <- cbind(marit1=matrix(NA, nrow(maritstatus.chg),1), maritstatus.chg)
# Rename the columns to maritchg so they do not interfere with the 'marit' 
# column
names(maritstatus.chg) <- sub('^marit', 'maritchg', names(maritstatus.chg))

hhreg <- cbind(hhreg, maritstatus.chg)

###############################################################################
# Now do the reshape.
###############################################################################
# Find the column indices of all columns that are repeated measurements
columns <- grep('^(livng|age|preg|marit|maritchg|place|spous|hhid)[0-9]*$', names(hhreg))
hhreg$gender <- factor(hhreg$gender, labels=c("male", "female"))

# Reshape age and livngs. Include columns 1, 3, and 4 as these are respid, 
# ethnic, and gender, respectively.
hhreg.reshaped <- reshape(hhreg[c(1, 3, 4, columns)], direction="long",
        varying=names(hhreg[columns]), idvar="respid", timevar="time", sep="")

events <- hhreg.reshaped

###############################################################################
# Process age/livngs/hasspouse/marr to recode and remove NAs, etc.
###############################################################################
# Add a new column "has spouse" that is 0 if a person is not married, 1 if a 
# person is married. Do they by recoding the MARIT data. Recode unmarried, 
# widowed, divorced and separated (3, 4, 5 and 6) as 0, and married living with 
# spouse and married not living with spouse (1 and 2) to 1.
events <- cbind(events, hasspouse=matrix(NA,nrow(events),1))
events$hasspouse <- events$marit
events$hasspouse[events$hasspouse==2] <- 1
events$hasspouse[events$hasspouse!=1] <- 0

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

# Drop rows where hasspouse has NAs
events <- events[-which(is.na(events$hasspouse)),]

###############################################################################
# Process deaths.
###############################################################################
# Add a column to store the bin index for each record (determined by the
# person's age).
events <- cbind(events, deathbin=matrix(NA,nrow(events),1))

deathlims <- c(0, 3, 6, 12, 20, 30, 40, 50, 60, 70, 80, 90, 199)
# First count number of person months in each bin
for (limindex in 1:(length(deathlims)-1)) {
    events[events$age>=deathlims[limindex] &
            events$age<deathlims[limindex+1],]$deathbin <- deathlims[limindex]
}

# Then count the number of death events per bin
deaths <- aggregate(events$livng==3, by=list(gender=events$gender,
        deathbin=events$deathbin), sum)
deathspsnmnths <- aggregate(events$livng==2, by=list(gender=events$gender,
        deathbin=events$deathbin), sum)
deathprob <- data.frame(gender=deaths$gender, bin=deaths$deathbin,
        prob=(deaths$x/deathspsnmnths$x)*12)

# Calculate the number of deaths per month
monthly.deaths <- with(events[events$livng==3,], aggregate(livng==3, by=list(time=time), sum))

###############################################################################
# Process births.
###############################################################################
# Note that hhreg data is only available for pregnancy/births Note that every 
# live birth is a 3 and all other pregnancy statuses (1, 2, 4, 5, and 6) are 
# ignored. Males are always coded as -1 for preg status. NOTE: This means that 
# events$preg is only meaningful in conjunction with events$livng to make sure 
# that only live people are counted, and with events$gender to ensure only 
# women are counted.

# Preg status is only recorded in the hhreg data for women between the ages of 
# 18 and 45
events <- cbind(events, pregbin=matrix(NA,nrow(events),1))
preglims <- c(0, 14, 15, 16, 18, 20, 23, 26, 30, 35, 40, 45, 199)
for (limindex in 1:(length(preglims)-1)) {
    events[events$age>=preglims[limindex] &
            events$age<preglims[limindex+1],]$pregbin <- preglims[limindex]
}
# Then count the number of death events per bin, only considering married women 
# (there are only 2 births to unmarried women)
fecund <- events[events$gender=="female" & !is.na(events$preg),]
births <- with(fecund[fecund$hasspouse==1,], aggregate(preg==3,
        by=list(pregbin=pregbin), sum))
birthpsnmnths <- aggregate(fecund$gender=="female",
        by=list(pregbin=fecund$pregbin), sum)
birthprob <- data.frame(bin=births$pregbin, prob=(births$x/birthpsnmnths$x)*12)

# Calculate the number of births per month
monthly.births <- with(events[events$preg==3,], aggregate(preg==3, by=list(time=time), sum))

# TODO: Also calculate the proportion of female/male births
###############################################################################
# Process marriages.
###############################################################################
events <- cbind(events, marrbin=matrix(NA,nrow(events),1))
marrlims <- c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 199)
for (limindex in 1:(length(marrlims)-1)) {
    events[events$age>=marrlims[limindex] &
            events$age<marrlims[limindex+1],]$marrbin <- marrlims[limindex]
}
# Calculate the number of marriages per month
monthly.marriages <- with(events[events$maritchg==1,], aggregate(maritchg==1, by=list(time=time), sum))
monthly.marriages.end <- with(events[events$maritchg==-1,], aggregate(maritchg==-1, by=list(time=time), sum))

###############################################################################
# Now write out probabilities to text
###############################################################################
make.txthazard(birthprob$prob, preglims, "hazard.birth")
make.txthazard(deathprob[deathprob$gender=="male",]$prob, deathlims,
        "hazard.death.male")
make.txthazard(deathprob[deathprob$gender=="female",]$prob, deathlims,
        "hazard.death.female")
make.txthazard(deathprob[deathprob$gender=="female",]$prob, deathlims,
        "hazard.marriage")
