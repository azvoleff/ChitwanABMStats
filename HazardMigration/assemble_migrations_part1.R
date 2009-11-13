###############################################################################
# Processes DS0010 to code local and distant migrations, and saves the results 
# to an Rdata file.
###############################################################################

library("foreign")
library("Hmisc")
DS0010 <- sasxport.get("/media/Restricted/Data/ICPSR_0538_Restricted/da04538-0010_REST.xpt")

# First assemble from DS0010 the migration outcomes for each month, leaving out 
# any months after a move occurs.
# NOTE: Months 1-54 correspond to February 1997 - July 2001.
# PLACE1 - PLACE 54 are indexed as columns 227-280 in the dataframe.
# AGE1 - AGE54 are indexed as columns 7-61 in the dataframe.

# Add an indicator to DS0010 to allow later exclusion of individuals who have 
# already migrated. This indicator starts out set at 0 (no migration) for all 
# individuals.

DS0010 <- cbind(DS0010, MIGRATED=0)

for (month in 2:54) {
    print(month)
    # Check for no-moves, local and long-distance migrations.  No migration is 
    # PLACEn=PLACEn+1. A local move is anything where the neighborhood ID 
    # changed from PLACEn to PLACEn+1, but the neighborhood ID was <= 502.  A 
    # Long distance migration is PLACEn+1 > 502.
    
    # Column 226 is one column before the PLACE1 column. Pull out the place 
    # columns, then mask out data with invalid places (PLACE <=0, meaning 
    # the participant was not yet part of the survey in that month).
    place_0 <- DS0010[226+month-1]
    place_1 <- DS0010[226+month]
    
    # First pull out the no-moves (coded as 0)
    no_migr <- DS0010[DS0010$MIGRATED==0 & place_0==place_1 & place_0 >= 0 & place_1 >= 0,]
    # To get the ages: column 6 is one column before the AGE1 column.
    ages <- no_migr[6+month]
    names(ages) <- "AGE" # Rename column so it is AGE (not AGE1, AGE2, etc.)
    new_records <- data.frame(RESPID=no_migr$RESPID, MIGR=0, AGE=ages, TIME=month)

    # Now pull out the rows with local moves, storing the row numbers in 
    # local_migr indices
    local_migr_indices <- (DS0010$MIGRATED==0) & (place_0!=place_1) & (place_1<=502) & (place_0 >= 0) & (place_1 >= 0)
    local_migr <- DS0010[local_migr_indices,]
    ages <- local_migr[6+month]
    names(ages) <- "AGE"
    # Add the local migrations to the new_records dataframe
    new_records <- rbind(new_records, data.frame(RESPID=local_migr$RESPID, MIGR=1, AGE=ages, TIME=month))
    # And set the indicator in DS0010 to indicate the individual has already 
    # migrated, so that subsequent months can be excluded from the analysis
    DS0010$MIGRATED[local_migr_indices] <- 1

    # Now pull out the long-distance moves
    distant_migr_indices <- (DS0010$MIGRATED==0) & (place_0!=place_1) & (place_1>502) & (place_0 >= 0) & (place_1 >= 0)
    distant_migr <- DS0010[distant_migr_indices,]
    ages <- distant_migr[6+month]
    names(ages) <- "AGE"
    new_records <- rbind(new_records, data.frame(RESPID=distant_migr$RESPID, MIGR=2, AGE=ages, TIME=month))
    # And set the indicator in DS0010 to indicate the individual has already 
    # migrated, so that subsequent months can be excluded from the analysis
    DS0010$MIGRATED[distant_migr_indices] <- 2

    # Add the new records to the migrations dataframe
    new_records <- na.omit(new_records)
    if (month==2) migrations <- new_records else migrations <- rbind(migrations, new_records)
}

save(migrations, file="migrations_raw.Rdata")
