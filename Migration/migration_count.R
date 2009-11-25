###############################################################################
# Instead of looking at hazard of migration, considers number of migrants per 
# month in western Chitwan Valley (WCV) in several categories, to see if there 
# is any seasonality, separate from climate.
#     1) Local migration (from within WCV to within WCV)
#     3) Distant out-migration (from WCV to outside WCV)
#     2) Distant in-migration (from outside of WCV to WCV)
# In future, also consider WHERE within Chitwan migrants are primarily 
# locating, and from WHERE within Chitwan migrants are primarily leaving.
###############################################################################

# MONTHS.AWAY gives the number of months a person must be away for a move to be 
# considered a migration.
MONTHS.AWAY <- 6

library("Hmisc")
#hhreg <- sasxport.get("/media/Restricted/Data/ICPSR_0538_Restricted/da04538-0010_REST.xpt")
#hhreg <- sasxport.get("/media/ENCRYPTDRV/Data/ICPSR_0538_Restricted/da04538-0010_REST.xpt")
load("/media/RestData/Data/CVFS_HHReg/hhreg126.Rdata")

pdf(file=paste("migration_totals-", MONTHS.AWAY, "_months_away.pdf", sep=""))

# First assemble from hhreg the migration outcomes for each month, leaving out 
# any months after a move occurs.

# Add an indicator to hhreg to allow later exclusion of individuals who have 
# already migrated. This indicator starts out set at 0 (no migration) for all 
# individuals.
hhreg <- cbind(hhreg, migrated=0)

place.cols <- grep('^place[0-9]*$', names(hhreg))
places <- hhreg[place.cols]
age.cols <- grep('^age[0-9]*$', names(hhreg))

# Setup 'months' vector to use in plotting.  Months 1-126 correspond to 
# February 1997 - June 2007.
dates <- c(as.Date("2/1/1997", "%m/%d/%Y"))
# The +2 below is because the dates start in March
for (n in 3:(length(place.cols)+2)) {
    year <- 1997 + floor((n - 1) / 12)
    month <- (n - 1) %% 12 + 1
    dates <- c(dates, as.Date(paste(month,"1", year, sep="/"),
        "%m/%d/%Y"))
}
# Cut out months at end for which data is not collected for enough subsequent 
# months to determine if a migration of at least MONTHS.AWAY ocurred
dates <- dates[-c((length(dates)-MONTHS.AWAY-1):length(dates))]

# This function provides a mask to only consider people who have been living in 
# the same place for a certain number of months. That way, persons who 
# "migrated" and then returned to the same place after a month can be 
# discounted in the analysis.
mig.mask<- function(places, num.months) {
    places <- as.matrix(places)
    months <- array(0, c(nrow(places), ncol(places)-num.months, num.months+1))
    for (n in 1:(num.months+1)) {
        month <- places[,n:(ncol(places)-num.months+n-1)]
        months[,,n] <- month
    } 
    months <- apply(months, c(1,2), sum)
    month1.place <- places[,1:(ncol(places)-num.months)]
    months <- months / ((num.months+1)*month1.place)
    months[months != 1] <- 0
    return(months)
}

print("Making mask...")
mask.mig.duration <- mig.mask(places, MONTHS.AWAY)
# Drop first column in the mask, as we do not know where people were prior to 
# the first month, so we have no idea if a migration ocurred prior to the first 
# month.
mask.mig.duration <- mask.mig.duration[,-1]

# Setup place0 and place1 
place0.cols <- 1:(length(place.cols)-MONTHS.AWAY-1)
place1.cols <- 2:(length(place.cols)-MONTHS.AWAY)

# No migration is PLACEn=PLACEn+1. A local to local migration is anything where 
# the neighborhood ID changed from PLACEn to PLACEn+1, but the neighborhood ID 
# was <= 502. A distant to local migration is PLACEn+1 > 502.
print("Calculating total migrations...")
mig.all <- places[place1.cols] - places[place0.cols]
mig.all[mig.all<0] <- T
mig.all[mig.all>0] <- T
mig.all[mig.all==0] <- F
mig.all <- mig.all * mask.mig.duration
plot(dates, apply(mig.all, 2, sum, na.rm=T), type='l',
    main=paste("All migrations (months=", MONTHS.AWAY, ")", sep=""),
    xlab="Year")

# Now calculate local->distant and distant->local migrations only
print("Calculating LD and DL migrations...")
places.dist <- places
places.dist[places.dist<=502] <- 1 # local
places.dist[places.dist>502] <- 2 # distant
mig.dist <- places.dist[place1.cols] - places.dist[place0.cols]
mig.dist <- mig.dist * mask.mig.duration
mig.dist[mig.dist==1] <- "DL"
mig.dist[mig.dist==-1] <- "LD"
mig.dist[mig.dist==0] <- F
plot(dates, apply(mig.dist=="DL", 2, sum, na.rm=T), type='l',
    main=paste("Distant-local migrations (months=", MONTHS.AWAY, ")",
    sep=""), xlab="Year")
plot(dates, apply(mig.dist=="LD", 2, sum, na.rm=T), type='l',
    main=paste("Local-distant migrations (months=", MONTHS.AWAY, ")", sep=""), xlab="Year")

# Now calculate local->local migrations only
print("Calculating LL migrations...")
places.local <- places
places.local[places.local>502] <- NA
mig.local <- places.local[place1.cols] - places.local[place0.cols]
mig.local <- mig.local * mask.mig.duration
mig.local[mig.local!=0] <- "LL"
mig.local[mig.local==0] <- F
plot(dates, apply(mig.local=="LL", 2, sum, na.rm=T), type='l',
    main=paste("Local-local migrations (months=", MONTHS.AWAY, ")", sep=""),
    xlab="Year")

dev.off()
