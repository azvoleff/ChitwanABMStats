###############################################################################
# Creates a survival model to calculate the hazard of death at a range of ages, 
# to be used in the calibration of the Chitwan Valley ABM.
###############################################################################

require("foreign")
require("survival")

hhreg <- read.xport("/media/Restricted/Data/ICPSR_0538_Restricted/da04538-0010_REST.xpt")

# A death is coded in the household registry data as a 3 in LIVNG1-LIVNG54. Any 
# other value indicates that the individual was alive.
# LIVNG1 - LIVNG54 are indexed as columns 117-170 in the dataframe.
# AGE1 - AGE54 are indexed as columns 7-60 in the dataframe.
livngs <- hhreg[117:170]
ages <- hhreg[7:60]

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
status <- matrix(NA, nrow(hhreg), 1)
statusage <- matrix(NA, nrow(hhreg), 1)
for (month in 1:54) {
    print(month)
    age <- ages[,month]
    # First code the new deaths
    index <- livngs[,month]
    index[index!=3] <- 0 # This zero indicates this position WILL NOT be changed
    index[index==3] <- 1 # This zero indicates this position WILL be changed
    index <- as.logical(index)
    status[index] <- 1
    statusage[index] <- age[index]
    # Now code the new "known-alives"
    index <- livngs[,month]
    index[index!=2] <- 0 # This zero indicates this position WILL NOT be changed
    index[index==2] <- 1 # This zero indicates this position WILL be changed
    index <- as.logical(index)
    status[index] <- 0
    statusage[index] <- age[index]
    print(summary(status))
}
deaths <- Surv(statusage, status)
plot(survfit(deaths), xlab="Age (years)", ylab="Survivorship", main="Chitwan mortality")

model <- survfit(deaths~statusage, dist="weibull")
plot(model, lty=c(1,3), ylab="Survivorship", xlab="Age (years)")
