###
# Calculates the crude birth rate over time for each neighborhood from the
# neighborhood history data,
###

library("foreign")
DS0002 <- read.xport("/media/Restricted/Data/da04538-0002_REST.xpt")
DS0004 <- read.xport("/media/Restricted/Data/da04538-0004_REST.xpt")
DS0010 <- read.xport("/media/Restricted/Data/da04538-0010_REST.xpt")
DS0014 <- read.xport("/media/Restricted/Data/da04538-0014_REST.xpt")

# TODO: See page 132 of "The R Book" for "aggregate" function.

###
# For DS0010:
# I only want data where PLACE1-PLACE47 (columns 227-273 of the dataframe) is 
# between 1-151 (only the CVFS sample neighborhoods). The data should then be 
# aggregated for each column from PLACE1-PLACE47 by this neighborhood ID.  
# Persons per neighborhood can be gotten from LIVNG1-47 (columns 117-163 of the 
# dataframe,  where the variable has a value of 2 (living in HH) or 4 (new HH 
# member living in HH)) and births per neighborhood can be gotten from PREG1-47 
# (columns 281-327 of the dataframe, where it takes on a value of 3 for a live 
# birth).
#
# NOTE: Months 1-47 correspond to February 1997 - December 2000. Still need to 
# figure out which neighborhoods to exclude (there should only be 138 not 151).
###

PLACE <- stack(DS0010, select=c(PLACE1:PLACE47))
LIVNG <- stack(DS0010, select=c(LIVNG1:LIVNG47))
PREG <- stack(DS0010, select=c(PREG1:PREG47))

DS0010Subset <- data.frame(PLACE=PLACE$values, LIVNG=LIVNG$values, PREG=PREG$values)

# Omit the NAs
DS0010Subset <- na.omit(DS0010Subset)

# Only include data within neighborhoods 1-151
DS0010Subset <- DS0010Subset[DS0010Subset$PLACE>0 & DS0010Subset$PLACE < 152,]

# For POP only include current HH residents (coded as 2 or 4)
POP <- DS0010Subset[DS0010Subset$LIVNG == 2 | DS0010Subset$LIVNG == 4,]
POP <- data.frame(Neighborhood=POP$PLACE, LIVNG=POP$LIVNG)
POP$LIVNG <- 1

# and for BIRTHS , only include the live births (coded as 3)
BIRTHS <- DS0010Subset[DS0010Subset$PREG == 3,]
BIRTHS <- data.frame(Neighborhood=BIRTHS$PLACE, LiveBirths=BIRTHS$PREG)
BIRTHS$LiveBirths <- 1

# Now aggregate the data by neighborhood for BIRTHS and POP

# Now calculate the crude birth rate for each neighborhood, equal to the mean 
# number of persons in the neighborhood divided by the total 
