###############################################################################
# Creates a series of models to calculate the hazard of death at a range of 
# ages, to be used in the calibration of the Chitwan Valley ABM.
###############################################################################

require("survival")
load("chitwan_deaths.Rdata")

pdf()
par(mfrow=c(2,2))

# Divide the hazards by 54/12 to convert them to be yearly hazards of death.
hazard.death <- (ndeaths/nmembers) / (54/12)
plot(bin.breakpoints[-1], hazard.death, type="s", xlab="Age (years)", ylab="Hazard of death", main="Chitwan valley death hazard")

deaths <- Surv(statustime, status)
plot(survfit(deaths), xlab="Time interval", ylab="Survivorship", main="Chitwan survival")

model.1 <- survfit(deaths~statusage)
plot(model, ylab="Survivorship", xlab="Age (years)")

hist(statusage[status==1], breaks=20, xlab="Age at death (years)", main="Chitwan Valley, age at death")
hist(statusage, breaks=20, xlab="Age of last observation (years)", main="Chitwan Valley, age at last known status")
dev.off()
