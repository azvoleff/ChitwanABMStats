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
MONTHS.AWAY <- 12

library("Hmisc")
print("Loading data...")
load("/media/Local_Secure/CVFS_HHReg/hhreg126.Rdata")

pdf(file=paste("migration_totals-", MONTHS.AWAY, "_months_away_20101011.pdf", sep=""))

# First assemble from hhreg the migration outcomes for each month, leaving out 
# any months after a move occurs.

place.cols <- grep('^place[0-9]*$', names(hhreg))
places <- hhreg[place.cols]
age.cols <- grep('^age[0-9]*$', names(hhreg))

# This function provides a mask to only consider people who migrate and then 
# live in the same place for a certain number of months.
stayedput.mask <- function(places, num.months) {
    places.sum <- as.matrix(places)
    # To find out if a person stayed in a particular place for longer than n 
    # months, add together the place codes for month 0, month 1 ... month n, 
    # storing the total in month 0. Then divide the total (in the month zero 
    # location), by the original month 0 place code. If this equals one, it 
    # means that the person stayed in the location in month 0 for at least n 
    # months. Recode all other values to 0. The last n months should also be 
    # coded 0, as there is insufficient data to know if a respondent remained 
    # in the location for at least n months.
    for (n in 1:(num.months)) {
        places.sum <- places.sum[1:(ncol(places.sum) - n)] + places.sum[(n + 1):ncol(places.sum)]
    } 
    month1.place <- places[,1:(ncol(places)-num.months)]
    places.sum <- places.sum / ((num.months+1)*month1.place)
    places.array[places.array != 1] <- 0
    return(places.array)
}
#stayedput <- stayedput.mask(places, MONTHS.AWAY)

# This function makes a mask to see, for a given point in time t, if a person 
# EVER is found in that same neighborhood from time t+1 to time t+MIN where MIN
# is the minimum number of months a person must be away to be considered a 
# migrant.  This way, persons who "migrate" and then return to the same place 
# after a only a few months (less than MIN) can be discounted in the analysis.  
# MIN is passed to the function as "num.months".
everreturn.mask <- function(places, num.months) {
    ever_return <- matrix(FALSE, nrow(places), ncol(places)-num.months)
    for (n in 1:(num.months)) {
        same.place <- places[,1:(ncol(places)-num.months)] == places[,(1+n):(ncol(places)-num.months+n)]
        ever_return[same.place] <- TRUE
    } 
    return(ever_return)
}

print("Making mask...")
everreturn <- everreturn.mask(places, MONTHS.AWAY)

# Setup place0 and place1 
place0.cols <- 1:(length(place.cols)-MONTHS.AWAY)
place1.cols <- 2:(length(place.cols)-MONTHS.AWAY+1)

# No migration is PLACEn=PLACEn+1. A local to local migration is anything where 
# the neighborhood ID changed from PLACEn to PLACEn+1, but the neighborhood ID 
# was <= 502. A distant to local migration is PLACEn+1 > 502.
print("Calculating total migrations...")
mig.all <- places[place1.cols] - places[place0.cols]
mig.all[mig.all<0] <- T
mig.all[mig.all>0] <- T
mig.all[mig.all==0] <- F
mig.all[everreturn] <- F
# Months 1-126 correspond to February 1997 - June 2007.
mig.all.ts <- ts(apply(mig.all, 2, sum, na.rm=T), start=c(1997, 3), deltat=1/12)
plot(mig.all.ts, type='l',
    main=paste("All migrations (months=", MONTHS.AWAY, ")", sep=""),
    xlab="Year")

# Now calculate local->distant and distant->local migrations only
print("Calculating LD and DL migrations...")
places.dist <- places
places.dist[places.dist<=502 & places.dist > 1] <- 1 # local
places.dist[places.dist>502] <- 2 # distant
mig.dist <- places.dist[place1.cols] - places.dist[place0.cols]
mig.dist[mig.dist==1] <- "DL"
mig.dist[mig.dist==-1] <- "LD"
mig.dist[mig.dist==0] <- F
mig.dist[everreturn] <- F
mig.DL.ts <- ts(apply(mig.dist=="DL", 2, sum, na.rm=T), start=c(1997, 2), deltat=1/12)
plot(mig.DL.ts, type='l',
    main=paste("Distant-local migrations (months=", MONTHS.AWAY, ")",
    sep=""), xlab="Year")
mig.LD.ts <- ts(apply(mig.dist=="LD", 2, sum, na.rm=T), start=c(1997, 2), deltat=1/12)
plot(mig.LD.ts, type='l',
    main=paste("Local-distant migrations (months=", MONTHS.AWAY, ")", sep=""), xlab="Year")

# Now calculate local->local migrations only
print("Calculating LL migrations...")
places.local <- places
places.local[places.local>502] <- NA
mig.local <- places.local[place1.cols] - places.local[place0.cols]
mig.local[mig.local!=0] <- "LL"
mig.local[mig.local==0] <- F
mig.local[everreturn] <- F
mig.LL.ts <- ts(apply(mig.local=="LL", 2, sum, na.rm=T), start=c(1997, 2), deltat=1/12)
plot(mig.LL.ts, type='l',
    main=paste("Local-local migrations (months=", MONTHS.AWAY, ")", sep=""),
    xlab="Year")

dev.off()

rm(hhreg)
save.image(file=paste("migration_count-", MONTHS.AWAY, "_months_away_20101011.Rdata", sep=""))
