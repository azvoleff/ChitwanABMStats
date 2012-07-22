###############################################################################
# Makes a plot showing the sources of fuelwood and fodder collection.
###############################################################################

library("foreign")

pdf(file="firewood_fodder_sources_t1-3.pdf", paper="letter")
par(mfrow=c(2,1))

###############################################################################
# First plot for t1
t1ag <- read.xport("/media/Restricted/Data/ICPSR_0538_Restricted/da04538-0002_REST.xpt")
# BAB17 is fodder gathering location
t1ag$BAB17 <- factor(t1ag$BAB17, labels=c("Khet/bari", "Comm", "Forest", "Other"), exclude=-1)
# T3E17 is firewood gathering location
t1ag$BAE17 <- factor(t1ag$BAE17)

plot(t1ag$BAB15[-t1ag$BAB15==-1], main="Fodder gathering location (t1ag$BAB15)")
plot(t1ag$BAE17, main="Firewood gathering location (t1ag$BAE17)")

###############################################################################
# Now plot for t2
t2ag <- read.csv("/media/Restricted/Data/ICPSR_SupplementalData/Survey_conv/t2ag.csv")

# T2B17 is fodder gathering location
t2ag$T2B17 <- factor(t2ag$T2B17, labels=c("No resp", "Farm land", "Comm", "Forest", "Other", "Inapp"))
# T2E17 is firewood gathering location
t2ag$T2E17 <- factor(t2ag$T2E17)

plot(t2ag$T2B17, main="Fodder gathering location (t2ag$T2B17)")
plot(t2ag$T2E17, main="Firewood gathering location (t2ag$T2E17)")

###############################################################################
# Now plot for t3
t3hhca <- read.csv("/media/Restricted/Data/ICPSR_SupplementalData/Survey_conv/t3hhca.csv")

# T3B17 is fodder gathering location
t3hhca$T3B17 <- factor(t3hhca$T3B17, labels=c("Farm land", "Common land", "Forest", "Other", "Inapp"))
# T3E17 is firewood gathering location
t3hhca$T3E17 <- factor(t3hhca$T3E17)

plot(t3hhca$T3B17, main="Fodder gathering location (t3hhca$T3B17)")
plot(t3hhca$T3E17, main="Firewood gathering location (t3hhca$T3E17)")

dev.off()
