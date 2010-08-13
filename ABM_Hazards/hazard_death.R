###############################################################################
# Creates a series of models to calculate the hazard of death at a range of 
# ages, to be used in the calibration of the Chitwan Valley ABM.
###############################################################################

require("survival")

load("chitwan_deaths.Rdata")

pdf()
par(mfrow=c(2,2))

plot(deathhazard, type="s", xlab="Age (years)", ylab="Hazard of death",
    main="Chitwan valley death hazard")

deaths <- Surv(statustime, status)
plot(survfit(deaths), xlab="Time interval", ylab="Survivorship",
    main="Chitwan survival")

model <- survfit(deaths~statusage)
plot(model, ylab="Survivorship", xlab="Age (years)")

hist(statusage[status==1], breaks=20, xlab="Age at death (years)",
    main="Chitwan Valley, age at death")
hist(statusage, breaks=20, xlab="Age of last observation (years)",
    main="Chitwan Valley, age at status")

dev.off()

# Save the death hazards to a text file, after converting the hazards to 
# percentages
deathhazard[,2] <- deathhazard[,2]*100
write(t(deathhazard), file="death_hazard.txt", ncolumns=ncol(deathhazard),
    sep=",")
