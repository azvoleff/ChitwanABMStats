###############################################################################
# Prints the statistics for the selected sample neighborhoods.
###############################################################################

library(Hmisc)

load("/media/Restricted/Data/CVFS_R_format/hhreg.Rdata")
load("/media/Restricted/Data/CVFS_R_format/t3ag.Rdata")

samp.neigh <- c('062', '063', '064', '065', '067', '089', '090', '094', '095', '138')

samp  <- t3ag[t3ag$t3.neigh %in% samp.neigh,]

pdf()

# Print ethnicity

# Stove type
samp$t3c44 <- factor(samp$t3c44, labels=c('No', 'Yes'))
plot(samp$t3c44, main="Do you have a gas stove?")

# Fuelwood usage
samp$t3e15 <- factor(samp$t3e15, labels=c('No', 'Yes'))
plot(samp$t3e15, main="Do you currently use firewood...?")
samp$t3e16 <- factor(samp$t3e16, labels=c('None', 'All', '> half', 'half', '< half'))
plot(samp$t3e16, main="How do you obtain firewood... Do you buy...?")
#samp$t3e36 <- factor(samp$t3e36, labels=c('All', 'most', '~half', '< half'))
#plot(samp$t3e36, main="Of firewood, fodder... from CFDo you buy...?")

samp$t3e37 <- factor(samp$t3e37, labels=c('No', 'Yes'))
plot(samp$t3e37, main="Last time when a NP opened... Did you go...?")

plot(samp$t3e16, samp$t3e37)
# HH size

dev.off()
