###############################################################################
# Preprocesses household registry data to prepare for survival analysis of 
# migration.
#     1) Local migration (from within WCV to within WCV)
#     3) Distant out-migration (from WCV to outside WCV)
#     2) Distant in-migration (from outside of WCV to WCV)
# Treats LL, DL, DD, and LD migrations as competing risks. People are tracked 
# only up until their first migration, after which they are considered censored 
# (NA values inserted for remaining months of wide format).
###############################################################################
library(Hmisc)
library(ggplot2)
# Months.total is how many months of the household registry to include (max 
# number of months is 126, so to include all the months set LAST.MONTH to 126).
LAST.MONTH <- 36
# MONTHS.AWAY gives the number of months a person must be away for a move to be 
# considered a migration.
MONTHS.AWAY <- 1

PLOT_WIDTH = 8.33
PLOT_HEIGHT = 5.53
DPI=  300
theme_update(theme_grey(base_size=18))

print("Loading data...")
load("V:/Nepal/CVFS_HHReg/hhreg126.Rdata")
# Drop the appropriate monthly columns if LAST.MONTH is < 126
varying_cols <- grep('^[a-zA-Z]*[1-9][0-9]{0,2}$', names(hhreg))
varying_cols_times <- as.numeric(gsub('[a-zA-Z]', '', names(hhreg)[varying_cols]))
if (LAST.MONTH < max(varying_cols_times)) {
    drop_cols <- varying_cols[varying_cols_times > LAST.MONTH]
    hhreg <- hhreg[-drop_cols]
}

# TODO: livng is not used now, but use it as a check on the place variable.
# Use livng as a check on place columns. Recode it so 1 means in HH, 0 means 
# away
livng.cols <- grep('^livng[0-9]*$', names(hhreg))
livng.recode <- hhreg[livng.cols]
livng.recode[hhreg[livng.cols] == 1] <- 0 # Away from hh
livng.recode[hhreg[livng.cols] == 2] <- 1 # Living in hh
livng.recode[hhreg[livng.cols] == 3] <- NA # Died
livng.recode[hhreg[livng.cols] == 4] <- 1 # New HH member not in orig census
# TODO: Cleanup the recode of 5 to handle merge HHs
livng.recode[hhreg[livng.cols] == 5] <- NA # HH merged with other sample HH
livng.recode[hhreg[livng.cols] == 6] <- 0 # First month away

place.cols <- grep('^place[0-9]*$', names(hhreg))
age.cols <- grep('^age[0-9]*$', names(hhreg))
hhid.cols <- grep('^hhid[0-9]*$', names(hhreg))
ethnic.col <- grep('^ethnic$', names(hhreg))
gender.col <- grep('^gender$', names(hhreg)) # 0=male, 1=female
respid.col <- grep('^respid$', names(hhreg))

# Clean the data to convert unneeded missing value codes to NAs
hhreg[hhid.cols][hhreg[hhid.cols]=="     A"] <- NA # Inappropriate code is A

# First assemble from hhreg the migration outcomes for each month, leaving out 
# any months after a move occurs.
migr <- hhreg[place.cols]
names(migr) <- gsub('place', 'migr', names(migr))

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
everreturn <- everreturn.mask(migr, MONTHS.AWAY)

# No migration is PLACEn=PLACEn+1. A local to local migration is anything where 
# the neighborhood ID changed from PLACEn to PLACEn+1, but the neighborhood ID 
# was <= 502. A distant to local migration is PLACEn+1 > 502.
# Months 1-126 correspond to February 1997 - June 2007.

# Calculate local-> distant and distant-> local migrations only
print("Calculating migrations...")
place0.cols <- 1:(length(place.cols)-MONTHS.AWAY)
place1.cols <- 2:(length(place.cols)-MONTHS.AWAY+1)

# First setup mig.type as a matrix of NAs. "NM"s will fill each cell where 
# there is data for that person for that month (NM=no migration), and NAs fill 
# all other cells. NM values will be reassigned to migration indicators (LL, 
# LD, DL, DD) as necessary in the next few blocks of code.
mig.type <- is.na(migr[,2:ncol(migr),])
mig.type[mig.type==TRUE] <- NA
mig.type[mig.type==FALSE] <- "NM"
row.names(mig.type) <- hhreg$respid

# Now code the migration type in mig.type as:
# LL = Local-local
# DL = Distant-local
# LD = Local-distant
# DD = Distant-distant
# NM = No migration

# First do DL and LD:
mig.DLLD <- migr
mig.DLLD[migr > 1 & migr <= 502] <- 1 # local
mig.DLLD[migr > 502] <- 2 # distant
mig.DLLD <- mig.DLLD[place0.cols] - mig.DLLD[place1.cols]
mig.DLLD[mig.DLLD==1] <- "DL"
mig.DLLD[mig.DLLD==0] <- NA # No migration
mig.DLLD[mig.DLLD==-1] <- "LD"
mig.DLLD[everreturn] <- NA
mig.type[mig.DLLD=="DL"] <- "DL"
mig.type[mig.DLLD=="LD"] <- "LD"

# Now do DD:
mig.DD <- migr
mig.DD[migr > 1 & migr <= 502] <- NA # Ignore local
mig.DD <- mig.DD[place0.cols] - mig.DD[place1.cols]
mig.DD[mig.DD!=1] <- "DD"
mig.DD[mig.DD==0] <- NA # No migration
mig.DD[everreturn] <- NA
mig.type[mig.DD=="DD"] <- "DD"

# Now do LL:
mig.LL <- migr
mig.LL[migr > 502] <-  NA # Ignore distant
mig.LL <- mig.LL[place0.cols] - mig.LL[place1.cols]
mig.LL[mig.LL!=1] <- "LL"
mig.LL[mig.LL==0] <- NA # No migration
mig.LL[everreturn] <- NA
mig.type[mig.LL=="LL"] <- "LL"

# Code and plot the average amount of time people are gone for.
#LD_first_migration_col <- apply(mig.type, 1, function(x) match("LD", x, nomatch=NA))
#DL_first_migration_col <- apply(mig.type, 1, function(x) match("DL", x, nomatch=NA))
#time_outside <- LD_first_migration_col - DL_first_migration_col
#mean(time_outside, na.rm=T)
#qplot(time_outside[time_outside<36], geom="histogram", xlab="Months Away", 
#      ylab="Count", binwidth=3)
#ggsave(paste("data/time_outside-", MONTHS.AWAY, "_months_away-up_to_month_", LAST.MONTH, ".png", sep=""), 
#       width=PLOT_WIDTH, height=PLOT_HEIGHT, dpi=DPI)
#save(time_outside, file=paste("data/time_outside-", MONTHS.AWAY, "_months_away-up_to_month_", LAST.MONTH, ".Rdata", sep=""))

# Find total number of LD migrations:
#num_LD_migrants <- sum((!is.na(LD_first_migration_col)), na.rm=T)
# Find number of migrants who did not return:
#num_LD_migrants_no_return <- sum((!is.na(LD_first_migration_col)) & is.na(DL_first_migration_col), na.rm=T)
#(num_LD_migrants_no_return / num_LD_migrants)*100
# So have 1569 migrants in total over the 120 month period. 7.90% make an LD 
# migration and then do not return.

# Now eliminate people who not there AND local at beginning of 1996.
local_in_t0 <- (migr[,1] > 1) & (migr[,1] <= 502)
local_in_t0[is.na(local_in_t0)] <- FALSE
mig.type <- mig.type[local_in_t0,]
# Now censor the data by finding the first migration activity in each row, and 
# setting every cell in the row after that one to NA. Also censor every cell in 
# a row after the first NA in that row. The min(!is.na below is necessary to 
# find the first match of any type of migration.
print("Censoring data...")
censor_data <- function(record) {
    first_migr_column <- match(c('DL', 'LD', 'DD', 'LL'), record)
    first_migr_column <- min(first_migr_column[!is.na(first_migr_column)])
    first_migr_column[is.infinite(first_migr_column)] <- NA
    first_NA_column <- apply(mig.type, 1, function(x) match(TRUE, is.na(x)))
    if (is.na(first_migr_column) && is.na(first_NA_column)) {
        return(record)
    }
    else if (is.na(first_NA_column) | first_migr_column <= first_NA_column) {
             first_censored_col <- first_migr_column + 1
    }
    else {
        first_censored_col <- first_NA_column
    }
    if (first_censored_col < length(record)) {
        record[first_censored_col:length(record)] <- NA
    }
    return(record)
}
mig.type <- t(apply(mig.type, 1, censor_data))
# Add a respid column to mig.type dataframe (row.names were assigned earlier 
# for this dataframe).
mig.type <- cbind(respid=row.names(mig.type), mig.type)

originNBH <- migr$migr1

###############################################################################
# Output the data
###############################################################################
# First output in wide format

# Add columns with ethnicity, age, sex, and hhid. Don't include the last 
# MONTHS.AWAY cols of the age data, since we don't have migration data for 
# those months.  Also, start from the second age col so it lines up with the 
indepvars <- cbind(hhreg[respid.col], hhreg[ethnic.col], hhreg[gender.col], originNBH, hhreg[age.cols][2:(length(age.cols)-MONTHS.AWAY+1)], hhreg[hhid.cols][2:(length(hhid.cols)-MONTHS.AWAY+1)])
LDmigrations.wide <- merge(indepvars, mig.type, by="respid", all.x=F, all.y=T)
# Need to order the data properly for it to be used in MLwiN
LDmigrations.wide <- LDmigrations.wide[order(LDmigrations.wide$respid),]
save(LDmigrations.wide, file=paste("data/migration_data_wideformat-", MONTHS.AWAY, "_months_away-up_to_month_", LAST.MONTH, ".Rdata", sep=""))
write.csv(LDmigrations.wide, file=paste("data/migration_data_wideformat-", MONTHS.AWAY, "_months_away-up_to_month_", LAST.MONTH, ".csv", sep=""), row.names=FALSE)

# Now in long format
LDmig.migr.cols <- grep('^migr[0-9]*$', names(LDmigrations.wide))
LDmig.age.cols <- grep('^age[0-9]*$', names(LDmigrations.wide))
LDmig.hhid.cols <- grep('^hhid[0-9]*$', names(LDmigrations.wide))
# Now construct the long-format dataset
LDmigrations.long <- reshape(LDmigrations.wide, idvar="respid", 
                             varying=list(LDmig.migr.cols, LDmig.age.cols,
                                          LDmig.hhid.cols), 
                             v.names=c("migr", "age", "hhid"),
                             direction="long", sep="")
LDmigrations.long <- LDmigrations.long[!is.na(LDmigrations.long$migr),]
LDmigrations.long <- LDmigrations.long[order(LDmigrations.long$originNBH, LDmigrations.long$respid),]
save(LDmigrations.long, file=paste("data/migration_data_longformat-", MONTHS.AWAY, "_months_away-up_to_month_", LAST.MONTH, ".Rdata", sep=""))
write.csv(LDmigrations.long, file=paste("data/migration_data_longformat-", MONTHS.AWAY, "_months_away-up_to_month_", LAST.MONTH, ".csv", sep=""), row.names=FALSE)
