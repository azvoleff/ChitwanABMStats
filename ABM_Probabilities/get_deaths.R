###############################################################################
# Loads deaths and age at death from the ICPSR Restricted dataset DS0010, the 
# household registry data.
###############################################################################

require("foreign")

hhreg <- read.xport("/media/Restricted/Data/ICPSR_0538_Restricted/da04538-0010_REST.xpt")
# A death is coded in the household registry data as a 3 in LIVNG1-LIVNG54. Any 
# other value indicates that the individual was alive. Ages are given in AGE1 - 
# AGE54. When doing the regex, LIVNGLT and AGELT (from the census data) need to 
# be excluded, so the $ is required to end the string.
livngs <- hhreg[grep('^LIVNG[0-9]*$', names(hhreg))]
ages <- hhreg[grep('^AGE[0-9]*$', names(hhreg))]

NUMMONTHS=54

# Here I will recode the data so any point where an individual was known alive 
# is a 2, the period in which they died is a 3, and all other times (including 
# when they were away (alive/dead unknown) is a 1.
livngs[livngs==4] <- 2 # Code new HH member as alive
livngs[livngs==5] <- 2 # Code HH merged as alive
livngs[livngs==6] <- 1 # Code away from HH as unknown
livngs[is.na(livngs)] <- 1 # Code NA as unknown

# Now, for each individual, find their age at death, or the age at which they 
# were last known to be alive. Do this by creating a vector of length 
# equivalent to the number of individuals in the dataset, and by then iterating 
# over the months, and updating it with each known alive or known dead event.  
lastalive <- na.omit(apply(livngs, 1, function(row) match(2, row)))
died <- na.omit(apply(livngs, 1, function(row) match(3, row)))

# BEGIN OLD METHOD:
status <- rep(NA, nrow(hhreg))
statusage <- rep(NA, nrow(hhreg))
statustime <- rep(NA, nrow(hhreg))
for (month in 1:NUMMONTHS) {
    age <- ages[,month]
    # First code the new deaths
    index <- livngs[,month]
    index[index!=3] <- 0 # This zero indicates this position WILL NOT be changed
    index[index==3] <- 1 # This zero indicates this position WILL be changed
    index <- as.logical(index)
    status[index] <- 1
    statusage[index] <- age[index]
    statustime[index] <- month
    # Now code the new "known-alives"
    index <- livngs[,month]
    index[index!=2] <- 0 # This zero indicates this position WILL NOT be changed
    index[index==2] <- 1 # This zero indicates this position WILL be changed
    index <- as.logical(index)
    status[index] <- 0
    statusage[index] <- age[index]
    statustime[index] <- month
}
status <- na.omit(status)
statusage <- na.omit(statusage)
statustime <- na.omit(statustime)

# Count the deaths within each group
bin.breakpoints <- c(0, 3, 6, 12, 20, 30, 40, 50, 60, 70, 80, 90, 100)
bin.yvalues<- rep(NA, length(bin.breakpoints)-1)
ndeaths <- rep(NA, length(bin.breakpoints)-1)
nmembers <- rep(NA, length(bin.breakpoints)-1)
for (n in 1:length(bin.breakpoints)-1) {
    agegroup.members.indices <- (statusage >= bin.breakpoints[n]) & (statusage < bin.breakpoints[n+1])
    agegroup.deaths.indices <- (status == 1) & agegroup.members.indices
    ndeaths[n] <- length(status[agegroup.deaths.indices])
    nmembers[n] <- length(status[agegroup.members.indices])
}

# Divide the hazards by NUMMONTHS/12 to convert them to be yearly hazards of 
# death.
hazard.death <- (ndeaths/nmembers) / (NUMMONTHS/12)
deathhazard <- cbind(matrix(bin.breakpoints[-1]), matrix(hazard.death))

save(deathhazard, ndeaths, nmembers, status, statusage, statustime, file="chitwan_deaths.Rdata")
