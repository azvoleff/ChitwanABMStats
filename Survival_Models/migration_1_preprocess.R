###############################################################################
# Preprocesses household registry data to prepare for survival analysis of 
# migration.
#     1) Local migration (from within WCV to within WCV)
#     3) Distant out-migration (from WCV to outside WCV)
#     2) Distant in-migration (from outside of WCV to WCV)
###############################################################################
# MONTHS.AWAY gives the number of months a person must be away for a move to be 
# considered a migration.
library(Hmisc)
library(ggplot2)

MONTHS.AWAY <- 3

PLOT_WIDTH = 8.33
PLOT_HEIGHT = 5.53
DPI=  300
theme_update(theme_grey(base_size=18))

print("Loading data...")
load("V:/Nepal/CVFS_HHReg/hhreg126.Rdata")

# First assemble from hhreg the migration outcomes for each month, leaving out 
# any months after a move occurs.
place.cols <- grep('^place[0-9]*$', names(hhreg))
places <- hhreg[place.cols]
names(places) <- gsub('place', 'migr', names(places))
age.cols <- grep('^age[0-9]*$', names(hhreg))
hhid.cols <- grep('^hhid[0-9]*$', names(hhreg))
ethnic.col <- grep('^ethnic$', names(hhreg))
gender.col <- grep('^gender$', names(hhreg)) # 0=male, 1=female
respid.col <- grep('^respid$', names(hhreg))

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
# Months 1-126 correspond to February 1997 - June 2007.

# Calculate local-> distant and distant-> local migrations only
print("Calculating LD and DL migrations...")
mig.type.dist <- places
mig.type.dist[mig.type.dist<=502 & mig.type.dist > 1] <- 1 # local
mig.type.dist[mig.type.dist>502] <- 2 # distant
mig.dist <- mig.type.dist[place1.cols] - mig.type.dist[place0.cols]
mig.dist[mig.dist==1] <- "DL"
mig.dist[mig.dist==-1] <- "LD"
mig.dist[mig.dist==0] <- F
mig.dist[everreturn] <- F
row.names(mig.dist) <- hhreg$respid

# Eliminate people who were not there at beginning of 1996 (note the second 
# column is used since the first is respid
mig.dist.1996 <- mig.dist[!is.na(mig.dist[,1]),]
# Eliminate people who migrated in (from distant) before migrating out - we 
# want only people migrating out of Chitwan as their first migration.
DL_migration_col_full <- apply(mig.dist.1996, 1, function(x) match("LD", x, nomatch=NA))
LD_migration_col_full <- apply(mig.dist.1996, 1, function(x) match("DL", x, nomatch=NA))
LDmigrations <- mig.dist.1996[which(is.na(LD_migration_col_full) |
                           (LD_migration_col_full < DL_migration_col_full)),]
# Code the average amount of time people are gone for.
LD_first_migration_col <- apply(LDmigrations, 1, function(x) match("LD", x, nomatch=NA))
DL_first_migration_col <- apply(LDmigrations, 1, function(x) match("DL", x, nomatch=NA))
time_outside <- LD_first_migration_col - DL_first_migration_col
#mean(time_outside, na.rm=T)
qplot(time_outside[time_outside<36], geom="histogram", xlab="Months Away", 
      ylab="Count", binwidth=3)
ggsave(paste("time_outside-", MONTHS.AWAY, "_months_away.png", sep=""), 
       width=PLOT_WIDTH, height=PLOT_HEIGHT, dpi=DPI)
save(time_outside, file=paste("time_outside-", MONTHS.AWAY, "_months_away.Rdata", sep=""))

# Find total number of LD migrations:
num_LD_migrants <- sum((!is.na(LD_first_migration_col)), na.rm=T)
# Find number of migrants who did not return:
num_LD_migrants_no_return <- sum((!is.na(LD_first_migration_col)) & is.na(DL_first_migration_col), na.rm=T)
(num_LD_migrants_no_return / num_LD_migrants)*100
# So have 1569 migrants in total over the 120 month period. 7.90% make an LD 
# migration and then do not return.

# Now code all cols after a LD migration as NA so they can be eliminated 
# later (after reshaping to long format):

print("Coding LD migration in wide format prior to long format reshape...")
pb <- txtProgressBar(min=0, max=nrow(LDmigrations), style=3)
for (rownum in 1:nrow(LDmigrations)) {
    setTxtProgressBar(pb, rownum)
    # Store NA values as we have to avoid coding columns with a 0 or 1 if they 
    # are already NA, as these are columns where we have no data on an 
    # individual.
    na_cols <- is.na(LDmigrations[rownum,])
    mig_col <- LD_first_migration_col[rownum]
    col.migration <- mig_col
    if (is.na(col.migration)) {
        # If there was no migration in this row, code all the columns as 0
        LDmigrations[rownum, 1:ncol(LDmigrations)] <- 0
        # And make sure NAs are retained:
        LDmigrations[rownum, na_cols] <- NA
        next
    }
    # In the case of migrations that occurred in the first col, the 
    # assignment below will be wrong. This will be fixed in a few lines as 
    # migrations are coded after this statement (which will overwrite this 
    # value if it is incorrect).
    cols.premigration <- 1:(mig_col)
    # Add 1 for the postmigration columns so that we don't overwrite the respid 
    # column or the migration col itself:
    if (mig_col+1 >= ncol(LDmigrations)) {
        # Catch a migration that occurred in the last col. In this case there 
        # are no post-migration cols.
        cols.postmigration <- c()
    } else {
        cols.postmigration <- (mig_col+1):ncol(LDmigrations)
    }
    # Code the cols preceding the LD migration as 0
    LDmigrations[rownum, cols.premigration] <- 0
    # Code the remaining cols after the LD migration as NA
    LDmigrations[rownum, cols.postmigration] <- NA
    # Code the LD migration as 1 (do this last as otherwise the cols 
    # preceding code will overwrite it due to R's annoying indexing).
    LDmigrations[rownum, col.migration] <- 1
    # Also code cols where we have no data on the respondent as NA
    LDmigrations[rownum, na_cols] <- NA
}

# Add columns with ethnicity, age, sex, and hhid. Don't include the last 
# MONTHS.AWAY cols of the age data, since we don't have migration data for 
# those months.  Also, start from the second age col so it lines up with the 
# place data.
originNBH <- places[place0.cols]
# First rename the originNBH columns using the names of the place1.cols - so 
# the names will start out at "place2", (instead of "place1") and line up with 
# the names of the age and migration outcome columns.
names(originNBH) <- names(places[place1.cols])
names(originNBH) <- gsub('migr', 'originNBH', names(originNBH))
# Add a respid column to LDmigrations dataframe (row.names were assigned 
# earlier for this dataframe).
LDmigrations <- cbind(respid=row.names(LDmigrations), LDmigrations)
indepvars <- cbind(hhreg[respid.col], hhreg[ethnic.col], hhreg[gender.col], originNBH, hhreg[age.cols][2:(length(age.cols)-MONTHS.AWAY+1)], hhreg[hhid.cols][2:(length(hhid.cols)-MONTHS.AWAY+1)])
LDmigrations.wide <- merge(indepvars, LDmigrations, by="respid", all.x=F, all.y=T)
# Need to order the data properly for it to be used in MLwiN
LDmigrations.wide <- LDmigrations.wide[order(LDmigrations.wide$respid),]
save(LDmigrations.wide, file=paste("migration_data_wideformat-", MONTHS.AWAY, "_months_away.Rdata", sep=""))
write.csv(LDmigrations.wide, file=paste("migration_data_wideformat-", MONTHS.AWAY, "_months_away.csv", sep=""), row.names=FALSE)

LDmig.migr.cols <- grep('^migr[0-9]*$', names(LDmigrations.wide))
LDmig.age.cols <- grep('^age[0-9]*$', names(LDmigrations.wide))
LDmig.hhid.cols <- grep('^hhid[0-9]*$', names(LDmigrations.wide))
LDmig.originNBH.cols <- grep('^originNBH[0-9]*$', names(LDmigrations.wide))
# Now construct the long-format dataset
LDmigrations.long <- reshape(LDmigrations.wide, idvar="respid", 
                             varying=list(LDmig.migr.cols, LDmig.age.cols,
                                          LDmig.hhid.cols, 
                                          LDmig.originNBH.cols), 
                             v.names=c("migr", "age", "hhid", "originNBH"),
                             direction="long", sep="")
LDmigrations.long <- LDmigrations.long[!is.na(LDmigrations.long$migr),]
LDmigrations.long <- LDmigrations.long[order(LDmigrations.long$originNBH, LDmigrations.long$respid),]
save(LDmigrations.long, file=paste("migration_data_longformat-", MONTHS.AWAY, "_months_away.Rdata", sep=""))
write.csv(LDmigrations.long, file=paste("migration_data_longformat-", MONTHS.AWAY, "_months_away.csv", sep=""), row.names=FALSE)
