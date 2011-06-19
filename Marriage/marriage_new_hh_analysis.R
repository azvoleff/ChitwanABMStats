#!/usr/bin/env Rscript
###############################################################################
# Calculates the percentage of men and percentage of women that move to their 
# new spouses household.
###############################################################################

# Hmisc is needed as hhreg is a "labelled" dataframe. If Hmisc is nto included, 
# will get errors saying "cannot coerce class "labelled" into a data.frame"
require(Hmisc)
require(ggplot2)

load("/media/Local_Secure/CVFS_R_format/hhreg.Rdata")

# TODO: Need to only consider neighborhoods with neighid < 151
# Make an indicator of whether or not a person is living with their spouse 
# 1=Yes, 0=No, NA=Not applicable
maritcolumns <- grep('^marit[0-9]*$', names(hhreg))
withspouse <- hhreg[maritcolumns]
withspouse[withspouse < 1] <- NA
withspouse[withspouse > 2] <- NA
# Recode 2 married not with spouse as 0, leaving married with spouse as 1
withspouse[withspouse==2] <- 0

# Recode 1 and 2 (married living with/without spouse) as 2, meaning married in 
# that month. Recode 3, 4, and 5 (unmarried, widowed, and divorced) as 1, 
# meaning unmarried. Recode 6 (separated, as NA).
#
# Next, Subtract the marit1-53 columns from the marit2-54 columns. Now, in 
# these new columns:
# 		1 = got married
# 		0 = no change in marital status
# 		-1 = marriage ended
maritstatus <- hhreg[maritcolumns]
maritstatus[maritstatus < 0] <- NA
maritstatus[maritstatus==1] <- 2
maritstatus[maritstatus==3] <- 1
maritstatus[maritstatus==4] <- 1
maritstatus[maritstatus==5] <- 1
maritstatus[maritstatus==6] <- NA
maritstatus.chg <- maritstatus[2:54] - maritstatus[1:53]

# We don't care about marriages that have ended, only those that are just 
# beginning. So use  maritstatus.chg as a mask to mask the withspouse column.  
# The resulting matrix will have new marriages where someone lives with their 
# spouse coded as 1, all others coded as 0.
maritstatus.chg <- maritstatus.chg * withspouse[2:54]

hhreg <- cbind(hhreg, maritstatus.chg)
hhreg <- cbind(hhreg, maritstatus)

###############################################################################
# Now do the reshape.
###############################################################################
# Find the column indices of all columns that are repeated measurements
columns <- grep('^(marit|maritchg|maritstat)[0-9]*$', names(hhreg))
hhreg$gender <- factor(hhreg$gender, labels=c("male", "female"))

# Reshape age and livngs. Include columns 1, 3, and 4 as these are respid, 
# ethnic, and gender, respectively.
events <- reshape(hhreg[c(1, 3, 4, columns)], direction="long",
        varying=names(hhreg[columns]), idvar="respid", timevar="time", sep="")

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

###############################################################################
# Process marriages.
###############################################################################
events <- cbind(events, marrbin=matrix(0,nrow(events),1))
marrlims <- c(0, 10, 14, 18, 22, 30, 40, 60, 199)
for (limindex in 1:(length(marrlims)-1)) {
    events[events$age>=marrlims[limindex] &
            events$age<marrlims[limindex+1],]$marrbin <- marrlims[limindex]
}

events$maritchg[is.na(events$maritchg)] <- 0
marriages <- aggregate(events$maritchg==1, by=list(gender=events$gender,
                marrbin=events$marrbin), sum)
# NEW STUFF
# Calculate the number of marriages per month
monthly.marriages <- with(events[events$maritchg==1,], aggregate(maritchg==1,
        by=list(time), sum))
# For simplicity of plotting - assume the mean number of marriages ocurred from 
# month prior to start of data collection to the first month of data 
# collection. That way the marriages timeseries will align with the rest of the 
# data - otherwise it would be one month shorter than the other series.
monthly.marriages <- rbind(c(1,floor(mean(monthly.marriages$x))), monthly.marriages)
monthly <- cbind(monthly, marriages=monthly.marriages$x)
# END NEW STUFF
# Remove NAs from maritstat
events$maritstat[is.na(events$maritstat)] <- 0
marrpsnmnths <- aggregate(events$maritstat==1, by=list(gender=events$gender,
        marrbin=events$marrbin), sum)
marrprob <- data.frame(gender=marriages$gender, bin=marriages$marrbin,
        prob=(marriages$x/marrpsnmnths$x)*12)

###############################################################################
# Now write out probabilities to text
###############################################################################

theme_update(theme_grey(base_size=18))
update_geom_defaults("line", aes(size=1))
update_geom_defaults("step", aes(size=1))

qplot(bin, prob*100, geom="step", colour=gender, xlab="Age (years)",
        ylab="Annual probability of marrying (%)", data=marrprob)
ggsave("marriage_prob.png", width=8.33, height=5.53, dpi=300)
