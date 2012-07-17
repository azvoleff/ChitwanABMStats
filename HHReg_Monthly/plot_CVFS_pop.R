#!/usr/bin/env Rscript
###############################################################################
# Loads_deaths and age at_death from the ICPSR Restricted dataset DS0010, the 
# household registry data, and makes a plot of the monthly data (marriages, 
#_deaths, and births).
###############################################################################

# Hmisc is needed as hhreg is a "labelled" dataframe. If Hmisc is nto included, 
# will get errors saying "cannot coerce class "labelled" into a data.frame"
require(Hmisc)
require(ggplot2)
require(reshape)

load("V:/Nepal/CVFS_HHReg/hhreg126.Rdata")

# Make an indicator for if a person is a current resident of Chitwan residing 
# in a survey household
livng_cols <- grep('^livng[0-9]*$', names(hhreg))
place_cols <- grep('^place[0-9]*$', names(hhreg))
in_Chitwan <- hhreg[livng_cols]==2 & hhreg[place_cols]<=151
# Make a special indicator for_deaths (as "livng" will not equal 2 for the 
# month they died, since it will be a 3 for "died in this month). So for_deaths 
# only check the place variable to ensure they are were in one of the proper 
# neighborhoods.
in_Chitwan_deaths <- hhreg[place_cols]<=151

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

maritstatus_chg <- maritstatus[2:ncol(maritstatus)] -
        maritstatus[1:(ncol(maritstatus)-1)]
# Add a column for time 1, which is only NAs as marital status is not known 
# prior to the first month, so no change can be calculated.
maritstatus_chg <- cbind(marit1=matrix(NA, nrow(maritstatus_chg),1), maritstatus_chg)
# Rename the columns to maritchg so they do not interfere with the 'marit' 
# column
names(maritstatus_chg) <- sub('^marit', 'maritchg', names(maritstatus_chg))

hhreg <- cbind(hhreg, maritstatus_chg)

###############################################################################
# Process_deaths.
###############################################################################

# The 'monthly' dataframe will store the number of each event (marriages, 
# births,_deaths) per month.
time_Robj = seq(as.Date("1997/02/15", "%Y/%m/%d"), as.Date("2007/07/15",
        "%Y/%m/%d"), by="months")
# Calculate the number of_deaths per month
monthly_deaths <- apply(hhreg[livng_cols]==3 & in_Chitwan_deaths, 2, sum,
        na.rm=T)
monthly <- data.frame(time_Robj=time_Robj, deaths=monthly_deaths)

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
preg_cols <- grep('^preg[0-9]*$', names(hhreg))
monthly_livebirths <- apply(hhreg[preg_cols]==3 & in_Chitwan, 2, sum, na.rm=T)
monthly <- cbind(monthly, births=monthly_livebirths)

###############################################################################
# Process marriages.
###############################################################################
# Calculate the number of marriages per month
maritchg_cols <- grep('^maritchg[0-9]*$', names(hhreg))
monthly_marriages <- apply(hhreg[maritchg_cols]==1 & in_Chitwan, 2, sum, na.rm=T)
monthly <- cbind(monthly, marriages=monthly_marriages)

theme_update(theme_grey(base_size=18))
update_geom_defaults("line", aes(size=1))
update_geom_defaults("step", aes(size=1))

# Output plots and csv files of number of events per month for the actual CVFS 
# data (for comparison to model results).
write.csv(monthly, file="CVFS_monthly_events.csv", row.names=FALSE)
save(monthly, file="CVFS_monthly_events.Rdata")

monthly_melt <- melt(monthly, id.vars="time_Robj")
names(monthly_melt)[2:3] <- c("Event_type", "events")
plt = qplot(time_Robj, events, geom="line", colour=Event_type, 
            linetype=Event_type, xlab="Year", ylab="Number of Events",
            data=monthly_melt) +
        scale_color_discrete(name="Legend",
                            breaks=c("births", "deaths", "marriages"),
                            labels=c("Births", "Deaths", "Marriages")) + 
        scale_linetype_discrete(name="Legend",
                            breaks=c("births", "deaths", "marriages"),
                            labels=c("Births", "Deaths", "Marriages"))
plt + geom_segment(aes(x=as.Date("2001/08/15"), y=0, xend=as.Date("2001/08/15"),
        yend=35), size=1, colour="black", alpha=.005) +
        geom_text(aes(x=as.Date("1999/03/15"), y=35,
        label="Parameterization dataset"), alpha=1, colour="black") +
        geom_text(aes(x=as.Date("2005/01/15"), y=35, label="Testing dataset"),
        alpha=1, colour="black")

ggsave("CVFS_monthly_events.png", width=8.33, height=5.53, dpi=300)
