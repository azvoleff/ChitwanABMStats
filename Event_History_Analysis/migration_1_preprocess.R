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

library(ggplot2)
library(Hmisc)

#theme_update(theme_grey(base_size=10))
theme_update(theme_bw(base_size=10))
#update_geom_defaults("point", aes(size=2))
#update_geom_defaults("line", aes(size=.75))

update_geom_defaults("line", aes(size=1))
update_geom_defaults("smooth", aes(size=1))
theme_update(theme_grey(base_size=24))
update_geom_defaults("point", aes(size=3))
DPI <- 300
PLOT_WIDTH <- 9
PLOT_HEIGHT <- 5.67

# Months.total is how many months of the household registry to include (max 
# number of months is 126, so to include all the months set LAST.MONTH to 126).
LAST.MONTH <- 36
# MONTHS.AWAY gives the number of months a person must be away for a move to be 
# considered a migration.
MONTHS.AWAY <- 1

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
places <- hhreg[place.cols]

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
everreturn <- everreturn.mask(places, MONTHS.AWAY)

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
mig.type <- is.na(places[,place1.cols])
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
mig.DLLD <- places
mig.DLLD[places > 1 & places <= 502] <- 1 # local
mig.DLLD[places > 502] <- 2 # distant
mig.DLLD <- mig.DLLD[place0.cols] - mig.DLLD[place1.cols]
mig.DLLD[mig.DLLD==1] <- "DL"
mig.DLLD[mig.DLLD==0] <- NA # No migration
mig.DLLD[mig.DLLD==-1] <- "LD"
mig.DLLD[everreturn] <- NA
mig.type[mig.DLLD=="DL"] <- "DL"
mig.type[mig.DLLD=="LD"] <- "LD"

# Now do DD:
mig.DD <- places
mig.DD[places > 1 & places <= 502] <- NA # Ignore local
mig.DD <- mig.DD[place0.cols] - mig.DD[place1.cols]
mig.DD[mig.DD!=1] <- "DD"
mig.DD[mig.DD==0] <- NA # No migration
mig.DD[everreturn] <- NA
mig.type[mig.DD=="DD"] <- "DD"

# Now do LL:
mig.LL <- places
mig.LL[places > 502] <-  NA # Ignore distant
mig.LL <- mig.LL[place0.cols] - mig.LL[place1.cols]
mig.LL[mig.LL!=1] <- "LL"
mig.LL[mig.LL==0] <- NA # No migration
mig.LL[everreturn] <- NA
mig.type[mig.LL=="LL"] <- "LL"

# Code and plot the average amount of time people are gone for (for first 
# migrations outside of the valley).
LD_first_migration_col <- apply(mig.type, 1, function(x) match("LD", x, nomatch=NA))
DL_first_migration_col <- apply(mig.type, 1, function(x) match("DL", x, nomatch=NA))
time_outside <- DL_first_migration_col - LD_first_migration_col
time_outside[DL_first_migration_col < LD_first_migration_col] <- NA
mean(time_outside, na.rm=T)
qplot(time_outside[time_outside<36], geom="histogram", xlab="Months Away", 
      ylab="Number of People", binwidth=3)
ggsave(paste("data/time_outside-", MONTHS.AWAY, "_months_away-up_to_month_", LAST.MONTH, ".png", sep=""), 
       width=PLOT_WIDTH, height=PLOT_HEIGHT, dpi=DPI)
save(time_outside, file=paste("data/time_outside-", MONTHS.AWAY, "_months_away-up_to_month_", LAST.MONTH, ".Rdata", sep=""))

# Find total number of LD migrations:
#num_LD_migrants <- sum((!is.na(LD_first_migration_col)), na.rm=T)
# Find number of migrants who did not return:
#num_LD_migrants_no_return <- sum((!is.na(LD_first_migration_col)) & is.na(DL_first_migration_col), na.rm=T)
#(num_LD_migrants_no_return / num_LD_migrants)*100
# So have 1569 migrants in total over the 120 month period. 7.90% make an LD 
# migration and then do not return.

# Now eliminate people who not there AND local at beginning of 1996.
local_in_t0 <- (places[,1] > 1) & (places[,1] <= 502) & !is.na(hhreg[,hhid.cols[1]])
local_in_t0[is.na(local_in_t0)] <- FALSE
mig.type <- mig.type[local_in_t0,]
# Save the variables that will be needed later as independent variables, 
# including the originNBH for each person, and some other covariates.
indepvars <- cbind(hhreg[respid.col], originNBH=places$place1, hhreg[ethnic.col], hhreg[gender.col], hhreg[age.cols][2:(length(age.cols)-MONTHS.AWAY+1)], hhreg[hhid.cols][2:(length(hhid.cols)-MONTHS.AWAY+1)])
indepvars <- indepvars[local_in_t0,]
# Now censor the data by finding the first migration activity in each row, and 
# setting every cell in the row after that one to NA. Also censor every cell in 
# a row after the first NA in that row. The min(!is.na below is necessary to 
# find the first match of any type of migration.
print("Censoring data...")
censor_data <- function(record) {
    first_migr_column <- na.omit(match(c('DL', 'LD', 'DD', 'LL'), record))
    if (length(first_migr_column)>0) {
        first_migr_column <- min(first_migr_column[!is.na(first_migr_column)])
    } else first_migr_column <- NA
    first_NA_column <- match(TRUE, is.na(record))
    if (is.na(first_migr_column) && is.na(first_NA_column)) {
        return(record)
    } else if (is.na(first_migr_column)) {
        first_censored_col <- first_NA_column
    } else if (is.na(first_NA_column)) {
        first_censored_col <- first_migr_column + 1
    } else if (first_NA_column < first_migr_column) {
        first_censored_col <- first_NA_column
    } else {
        first_censored_col <- first_migr_column + 1
    }
    if (first_censored_col <= length(record)) {
        record[first_censored_col:length(record)] <- NA
    }
    return(record)
}
mig.type.temp <- t(apply(mig.type, 1, censor_data))
# Apply returned a matrix, and mig.type.temp lost its row and column names.  
# Reassign them so mig.type ends up as the final censored matrix with correct 
# row and column names.
mig.type <- mig.type.temp
mig.type.temp <- data.frame(mig.type.temp, row.names=row.names(mig.type))
names(mig.type.temp) <- gsub('place', 'migr', names(places[,2:(ncol(places)-MONTHS.AWAY+1)]))
mig.type <- mig.type.temp

# Add respid and column to mig.type dataframe (row.names were assigned earlier 
# for this dataframe).
mig.type <- data.frame(respid=row.names(mig.type), mig.type)

###############################################################################
# Output the data
###############################################################################
print("Outputting censored data...")
# First output in wide format

# Add columns with originNBH, ethnicity, age, sex, and hhid.
migrations.wide <- merge(indepvars, mig.type, by="respid", all.x=F, all.y=T)
# Need to order the data properly for it to be used in MLwiN
migrations.wide <- migrations.wide[order(migrations.wide$respid),]
save(migrations.wide, file=paste("data/migration_data_wideformat-", MONTHS.AWAY, "_months_away-up_to_month_", LAST.MONTH, ".Rdata", sep=""))
write.csv(migrations.wide, file=paste("data/migration_data_wideformat-", MONTHS.AWAY, "_months_away-up_to_month_", LAST.MONTH, ".csv", sep=""), row.names=FALSE)

# Now in long format
migr.cols <- grep('^migr[0-9]*$', names(migrations.wide))
age.cols <- grep('^age[0-9]*$', names(migrations.wide))
hhid.cols <- grep('^hhid[0-9]*$', names(migrations.wide))
# Now construct the long-format dataset
migrations.long <- reshape(migrations.wide, idvar="respid", 
                             varying=list(migr.cols, age.cols,
                                          hhid.cols), 
                             v.names=c("migr", "age", "hhid"),
                             direction="long", sep="")
migrations.long <- migrations.long[!is.na(migrations.long$migr),]
migrations.long <- migrations.long[order(migrations.long$originNBH, migrations.long$respid),]
save(migrations.long, file=paste("data/migration_data_longformat-", MONTHS.AWAY, "_months_away-up_to_month_", LAST.MONTH, ".Rdata", sep=""))
write.csv(migrations.long, file=paste("data/migration_data_longformat-", MONTHS.AWAY, "_months_away-up_to_month_", LAST.MONTH, ".csv", sep=""), row.names=FALSE)
