###############################################################################
# Processes hhreg to code local and distant migrations, and saves the results 
# to an Rdata file.
###############################################################################

library("Hmisc")
#hhreg <- sasxport.get("/media/Restricted/Data/ICPSR_0538_Restricted/da04538-0010_REST.xpt")
#hhreg <- sasxport.get("/media/ENCRYPTDRV/Data/ICPSR_0538_Restricted/da04538-0010_REST.xpt")
load("/home/azvoleff/Data/CVFS_HHReg/hhreg126.Rdata")

# First assemble from hhreg the migration outcomes for each month, leaving out 
# any months after a move occurs.
# NOTE: Months 1-54 correspond to February 1997 - July 2001.
# PLACE1 - PLACE 54 are indexed as columns 227-280 in the dataframe.
# age1 - age54 are indexed as columns 7-61 in the dataframe.

# Add an indicator to hhreg to allow later exclusion of individuals who have 
# already migrated. This indicator starts out set at 0 (no migration) for all 
# individuals.
hhreg <- cbind(hhreg, migrated=0)

place.cols <- grep('^place[0-9]*$', names(hhreg))
for (month in place.cols[-1]) {
    print(month)
    # Check for no-moves, local and long-distance migrations.  No migration is 
    # PLACEn=PLACEn+1. A local move is anything where the neighborhood ID 
    # changed from PLACEn to PLACEn+1, but the neighborhood ID was <= 502.  A 
    # Long distance migration is PLACEn+1 > 502.
    
    # Pull out the place columns, then mask out data with invalid places (PLACE 
    # <=0, meaning the participant was not yet part of the survey in that 
    # month).
    place_0 <- hhreg[month-1]
    place_1 <- hhreg[month]
    
    # First pull out the no-moves (coded as 0)
    no_migr <- hhreg[hhreg$migrated==0 & place_0==place_1 & place_0 >= 0 & place_1 >= 0,]
    # To get the ages: column 6 is one column before the age1 column.
    ages <- no_migr[6+month]
    names(ages) <- "age" # Rename column so it is age (not age1, age2, etc.)
    new_records <- data.frame(respid=no_migr$respid, migr=0, age=ages, time=month)

    # Now pull out the rows with local moves, storing the row numbers in 
    # local_migr indices
    local_migr_indices <- (hhreg$migrated==0) & (place_0!=place_1) & (place_1<=502) & (place_0 >= 0) & (place_1 >= 0)
    local_migr <- hhreg[local_migr_indices,]
    ages <- local_migr[6+month]
    names(ages) <- "age"
    # Add the local migrations to the new_records dataframe
    new_records <- rbind(new_records, data.frame(respid=local_migr$respid, migr=1, age=ages, time=month))
    # And set the indicator in hhreg to indicate the individual has already 
    # migrated, so that subsequent months can be excluded from the analysis
    hhreg$migrated[local_migr_indices] <- 1

    # Now pull out the long-distance moves
    distant_migr_indices <- (hhreg$migrated==0) & (place_0!=place_1) & (place_1>502) & (place_0 >= 0) & (place_1 >= 0)
    distant_migr <- hhreg[distant_migr_indices,]
    ages <- distant_migr[6+month]
    names(ages) <- "age"
    new_records <- rbind(new_records, data.frame(respid=distant_migr$respid, migr=2, age=ages, time=month))
    # And set the indicator in hhreg to indicate the individual has already 
    # migrated, so that subsequent months can be excluded from the analysis
    hhreg$migrated[distant_migr_indices] <- 2

    # Add the new records to the migrations dataframe
    new_records <- na.omit(new_records)
    if (month==min(place.cols[-1])) migrations <- new_records else migrations <- rbind(migrations, new_records)
}

save(migrations, file="migrations_raw.Rdata")
