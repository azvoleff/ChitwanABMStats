#!/usr/bin/env Rscript
###############################################################################
# Loads deaths and age at death from the ICPSR Restricted dataset DS0010, the 
# household registry data, and calculates several statistics (birth rates, 
# marriage rates, and mortality rates) for parameterizing the ChitwanABM model.
###############################################################################

# Hmisc is needed as hhreg is a "labelled" dataframe. If Hmisc is nto included, 
# will get errors saying "cannot coerce class "labelled" into a data.frame"
require(Hmisc)
require(ggplot2)

load("/media/Local_Secure/CVFS_HHReg/hhreg126.Rdata")

# Make an indicator for if a person is a current resident of Chitwan residing 
# in a survey household
livng.cols <- grep('^livng[0-9]*$', names(hhreg))
place.cols <- grep('^place[0-9]*$', names(hhreg))
in.Chitwan <- hhreg[livng.cols]==2 & hhreg[place.cols]<=151
# Make a special indicator for deaths (as "livng" will not equal 2 for the 
# month they died, since it will be a 3 for "died in this month). So for deaths 
# only check the place variable to ensure they are were in one of the proper 
# neighborhoods.
in.Chitwan.deaths <- hhreg[place.cols]<=151

# Add a new set of columns coding whether a new marriage occurred prior to the 
# survey in the month. Do this by using the marit columns.
# First: recode 1 and 2 (married living with/without spouse) as 2, meaning 
# married in that month. Recode 3, 4, and 5 (unmarried, widowed, and divorced) 
# as 1, meaning unmarried. Recode 6 (separated, as 4).
#
# 	Next, Subtract the marit[1,end-1] columns from the marit[2,end-1] columns.  
# 	Now, in these new columns:
# 		1 = got married
# 		0 = no change in marital status
# 		-1 = marriage ended
# 		Other numbers have to do with separated -> other status. This is 
# 		ignored for now.
maritcolumns <- grep('^marit[0-9]*$', names(hhreg))
maritstatus <- hhreg[maritcolumns]
maritstatus[maritstatus==-4] <- NA
maritstatus[maritstatus==-3] <- NA
maritstatus[maritstatus==-1] <- NA
maritstatus[maritstatus==1] <- 2
maritstatus[maritstatus==3] <- 1
maritstatus[maritstatus==4] <- 1
maritstatus[maritstatus==5] <- 1
maritstatus[maritstatus==6] <- 4

maritstatus.chg <- maritstatus[2:ncol(maritstatus)] -
        maritstatus[1:(ncol(maritstatus)-1)]
# Add a column for time 1, which is only NAs as marital status is not known 
# prior to the first month, so no change can be calculated.
maritstatus.chg <- cbind(marit1=matrix(NA, nrow(maritstatus.chg),1), maritstatus.chg)
# Rename the columns to maritchg so they do not interfere with the 'marit' 
# column
names(maritstatus.chg) <- sub('^marit', 'maritchg', names(maritstatus.chg))

hhreg <- cbind(hhreg, maritstatus.chg)

###############################################################################
# Process deaths.
###############################################################################

# The 'monthly' dataframe will store the number of each event (marriages, 
# births, deaths) per month.
time.Robj = seq(as.Date("1997/02/15", "%Y/%m/%d"), as.Date("2007/07/15",
        "%Y/%m/%d"), by="months")
# Calculate the number of deaths per month
monthly.deaths <- apply(hhreg[livng.cols]==3 & in.Chitwan.deaths, 2, sum,
        na.rm=T)
monthly <- data.frame(time.Robj=time.Robj, deaths=monthly.deaths)

###############################################################################
# Process births.
###############################################################################
# Note that hhreg data is only available for pregnancy/births Note that every 
# live birth is a 3 and all other pregnancy statuses (1, 2, 4, 5, and 6) are 
# ignored. Males are always coded as -1 for preg status. NOTE: This means that 
# events$preg is only meaningful in conjunction with events$livng to make sure 
# that only live people are counted, and with events$gender to ensure only 
# women are counted.

# Calculate the number of births per month
preg.cols <- grep('^preg[0-9]*$', names(hhreg))
monthly.livebirths <- apply(hhreg[preg.cols]==3 & in.Chitwan, 2, sum, na.rm=T)
monthly <- cbind(monthly, livebirths=monthly.livebirths)

###############################################################################
# Process marriages.
###############################################################################
# Calculate the number of marriages per month
maritchg.cols <- grep('^maritchg[0-9]*$', names(hhreg))
monthly.marriages <- apply(hhreg[maritchg.cols]==1 & in.Chitwan, 2, sum, na.rm=T)
monthly <- cbind(monthly, marriages=monthly.marriages)

theme_update(theme_grey(base_size=18))
update_geom_defaults("line", aes(size=1))
update_geom_defaults("step", aes(size=1))

# Output plots and csv files of number of events per month for the actual CVFS 
# data (for comparison to model results).
write.csv(monthly, file="CVFS_monthly_events.csv", row.names=FALSE)
save(monthly, file="CVFS_monthly_events.Rdata")

monthly.stacked <- stack(monthly)
monthly.stacked <- cbind(time.Robj=rep(monthly$time.Robj, 3), monthly.stacked)
names(monthly.stacked)[2:3] <- c("events", "Event_type")
plt = qplot(time.Robj, events, geom="line", colour=Event_type, xlab="Year",
        ylab="Number of Events", data=monthly.stacked)
plt + geom_segment(aes(x=as.Date("2001/08/15"), y=0, xend=as.Date("2001/08/15"),
        yend=35), size=1, colour="black", alpha=.005) +
        geom_text(aes(x=as.Date("1999/03/15"), y=35,
        label="Parameterization dataset"), alpha=1, colour="black") +
        geom_text(aes(x=as.Date("2005/01/15"), y=35, label="Testing dataset"),
        alpha=1, colour="black")
ggsave("CVFS_monthly_events.png", width=8.33, height=5.53, dpi=300)
