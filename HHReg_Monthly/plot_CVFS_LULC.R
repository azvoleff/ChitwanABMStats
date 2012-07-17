#!/usr/bin/env Rscript
# Plots the LULC data from the CVFS neighborhood mapping data.
require(ggplot2)
require(foreign)

lulc <- read.xport("/media/Local_Secure/ICPSR_SupplementalData/Survey_converted/landuse.xpt")

# Exclude neighborhoods 152-172
lulc$NEIGHID <- as.ordered(lulc$NEIGHID)
lulc <- lulc[lulc$NEIGHID <= 151,]

categories.new <- c('agveg', 'nonagveg', 'privbldg', 'pubbldg', 'other', 'total')
categories.old <- list(c('BARI', 'IKHET', 'RKHET'),
        c('GRASSC', 'GRASSP', 'PLANTC', 'PLANTP'),
        c('HHRESID', 'MILL', 'OTRBLD'),
        c('ROAD', 'SCHOOL', 'TEMPLE'),
        c('CANAL', 'POND', 'RIVER', 'SILT', 'UNDVP'),
        c('TOTAL'))

lulc.new <- data.frame(neighid=lulc$NEIGHID)
for (timestep in 1:3) {
    for (catnum in 1:length(categories.old)) {
        cat.grepexp <- lapply(categories.old[catnum], paste, timestep, sep="",
                collapse="|")
        cat.cols <- grep(cat.grepexp, names(lulc))
        lulc.new <- cbind(lulc.new, apply(lulc[cat.cols], 1, sum))
        names(lulc.new)[ncol(lulc.new)] <- paste(categories.new[catnum],
                timestep, sep=".")
    }
}

# Convert land areas expressed in square feet to square meters
lulc.new[2:length(lulc.new)]  <- lulc.new[2:length(lulc.new)] * .09290304

# Make plots of LULC for CVFS data
time.Robj <- as.Date(c("1996/1/15", "2000/2/15", "2007/2/15"), format="%Y/%m/%d")

agveg.cols <- grep('^agveg.[0-9]*$', names(lulc.new))
nonagveg.cols <- grep('^nonagveg.[0-9]*$', names(lulc.new))
pubbldg.cols <- grep('^pubbldg.[0-9]*$', names(lulc.new))
privbldg.cols <- grep('^privbldg.[0-9]*$', names(lulc.new))
other.cols <- grep('^other.[0-9]*$', names(lulc.new))
total.cols <- grep('^total.[0-9]*$', names(lulc.new))

# Calculate the total land area of each neighborhood
nbh.area.1 <- apply(cbind(lulc.new$agveg.1, lulc.new$nonagveg.1,
        lulc.new$pubbldg.1, lulc$privbldg.1, lulc$other.1), 1, sum)
nbh.area.2 <- apply(cbind(lulc.new$agveg.2, lulc.new$nonagveg.2,
        lulc.new$pubbldg.2, lulc$privbldg.2, lulc$other.2), 1, sum)
nbh.area.3 <- apply(cbind(lulc.new$agveg.3, lulc.new$nonagveg.3,
        lulc.new$pubbldg.3, lulc$privbldg.3, lulc$other.3), 1, sum)
nbh.area <- cbind(nbh.area.1, nbh.area.2, nbh.area.3)

# And convert the LULC measurements from units of square meters to units that 
# are a percentage of total neighborhood area.
#lulc.sd <- lulc.new[2:length(lulc.new)]/nbh.area
# Use the below calculation to be consistent with the ABM definition of total 
# area.
lulc.sd <- lulc.new[2:length(lulc.new)]/lulc.new$total.1
lulc.sd <- cbind(neighid=lulc.new$neighid, lulc.sd)

lulc.sd.mean <- data.frame(time.Robj=time.Robj,
        agveg=apply(lulc.sd[agveg.cols], 2, mean),
        nonagveg=apply(lulc.sd[nonagveg.cols], 2, mean),
        pubbldg=apply(lulc.sd[pubbldg.cols], 2, mean),
        privbldg=apply(lulc.sd[privbldg.cols], 2, mean),
        other=apply(lulc.sd[other.cols], 2, mean), row.names=NULL)

write.csv(lulc.sd.mean, file="CVFS_monthly_LULC.csv", row.names=FALSE)
save(lulc.sd.mean, file="CVFS_monthly_LULC.Rdata")

# Stack lulc.mean so it can easily be used with ggplot2 faceting
lulc.sd.mean <- stack(lulc.sd.mean)
lulc.sd.mean <- cbind(time.Robj=rep(time.Robj,5), lulc.sd.mean)
names(lulc.sd.mean)[2:3] <- c("area", "LULC_type")

theme_update(theme_grey(base_size=18))
update_geom_defaults("line", aes(size=1))

qplot(time.Robj, area, geom="line", colour=LULC_type, xlab="Year",
        ylab="Mean Percentage of Neighborhood", data=lulc.sd.mean)
ggsave("CVFS_monthly_LULC_pct_NBH.png", width=8.33, height=5.53, dpi=300)

# Make this plot to compare to the CVFS results in Axinn and Ghimire, 2007
qplot(time.Robj, area, geom="line", colour=LULC_type, xlab="Year",
        ylab="Percentage of Chitwan", data=lulc.sd.mean)
ggsave("CVFS_monthly_LULC_pct_Chitwan.png", width=8.33, height=5.53, dpi=300)
