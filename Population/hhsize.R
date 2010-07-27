###############################################################################
# Calculates household size for each month of hhreg.
###############################################################################

library("Hmisc")
load("/media/Secure_Data/CVFS_HHReg/hhreg126.Rdata")

hhreg <- cbind(hhreg, migrated=0)

livng.cols <- grep('^livng[0-9]*$', names(hhreg))
hhid.cols <- grep('^hhid[0-9]*$', names(hhreg))

# Recode livng vars so that they are presense/absence of each indiv in the 
# household
hhreg[livng.cols][hhreg[livng.cols] == 1] <- 0 # 1 is originally resp away from household
hhreg[livng.cols][hhreg[livng.cols] == 2] <- 1
hhreg[livng.cols][hhreg[livng.cols] == 4] <- 1
hhreg[livng.cols][hhreg[livng.cols] == 5] <- 1
hhreg[livng.cols][hhreg[livng.cols] != 1] <- 0

livngs.data <- cbind(hhreg[livng.cols], hhreg[hhid.cols])

hhsize.mean = c()
numhs = c()
pop = c()
for (colnum in 1:126) {
    livng.col <- grep(paste('^livng', colnum, '$', sep=''), names(livngs.data))
    hhid.col <- grep(paste('^hhid', colnum, '$', sep=''), names(livngs.data))

    hhsizes.new <- aggregate(livngs.data[,livng.col],
            by=list(livngs.data[,hhid.col]), sum)

    hhsize.mean <- c(hhsize.mean, mean(hhsizes.new$x, na.rm=TRUE))
    numhs <- c(numhs, length(hhsizes.new$x[!is.na(hhsizes.new$x)]))
    pop <- c(pop, sum(hhsizes.new$x, na.rm=TRUE))
}

hhsize.mean <- ts(hhsize.mean, start=c(1997, 2), deltat=1/12)
numhs <- ts(numhs, start=c(1997, 2), deltat=1/12)
pop <- ts(pop, start=c(1997, 2), deltat=1/12)

save(hhsize.mean, file="hhsize.mean.Rdata")
save(numhs, file="numhs.Rdata")
save(pop, file="pop.Rdata")
