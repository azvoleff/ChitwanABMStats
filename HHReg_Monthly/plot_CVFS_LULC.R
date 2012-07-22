#!/usr/bin/env Rscript
# Plots the LULC data from the CVFS neighborhood mapping data.
require(ggplot2)
require(foreign)
require(reshape)

lulc <- read.xport("V:/Nepal/ICPSR_SupplementalData/Survey_converted/landuse.xpt")

# Exclude neighborhoods 152-172
lulc$NEIGHID <- as.ordered(lulc$NEIGHID)
lulc <- lulc[lulc$NEIGHID <= 151,]

categories_new <- c('agveg', 'nonagveg', 'privbldg', 'pubbldg', 'other', 'total')
categories_old <- list(c('BARI', 'IKHET', 'RKHET'),
        c('GRASSC', 'GRASSP', 'PLANTC', 'PLANTP'),
        c('HHRESID', 'MILL', 'OTRBLD'),
        c('ROAD', 'SCHOOL', 'TEMPLE'),
        c('CANAL', 'POND', 'RIVER', 'SILT', 'UNDVP'),
        c('TOTAL'))

lulc_recode <- data.frame(neighid=lulc$NEIGHID)
for (timestep in 1:3) {
    for (catnum in 1:length(categories_old)) {
        cat_grepexp <- lapply(categories_old[catnum], paste, timestep, sep="",
                collapse="|")
        cat_cols <- grep(cat_grepexp, names(lulc))
        lulc_recode <- cbind(lulc_recode, apply(lulc[cat_cols], 1, sum))
        names(lulc_recode)[ncol(lulc_recode)] <- paste(categories_new[catnum],
                timestep, sep="_")
    }
}

# Convert land areas expressed in square feet to square meters
lulc_recode[2:length(lulc_recode)]  <- lulc_recode[2:length(lulc_recode)] * .09290304

lulc_recode$veg_class=cut(((lulc_recode$agveg_1+lulc_recode$nonagveg_1)/lulc_recode$total_1)*100, breaks=c(0, 50, 75, 90, 100))
lulc_recode$bldg_class=cut(((lulc_recode$privbldg_1+lulc_recode$pubbldg_1)/lulc_recode$total_1)*100, breaks=c(0, 10, 25, 75, 100))

# Make plots of LULC for CVFS data
time_Robj <- as.Date(c("1996/1/15", "2000/2/15", "2007/2/15"), format="%Y/%m/%d")

agveg_cols <- grep('^agveg_[1-3]*$', names(lulc_recode))
nonagveg_cols <- grep('^nonagveg_[1-3]*$', names(lulc_recode))
pubbldg_cols <- grep('^pubbldg_[1-3]*$', names(lulc_recode))
privbldg_cols <- grep('^privbldg_[1-3]*$', names(lulc_recode))
other_cols <- grep('^other_[1-3]*$', names(lulc_recode))
total_cols <- grep('^total_[1-3]*$', names(lulc_recode))

# Calculate the total land area of each neighborhood
nbh_area_1 <- apply(cbind(lulc_recode$agveg_1, lulc_recode$nonagveg_1,
        lulc_recode$pubbldg_1, lulc$privbldg_1, lulc$other_1), 1, sum)
nbh_area_2 <- apply(cbind(lulc_recode$agveg_2, lulc_recode$nonagveg_2,
        lulc_recode$pubbldg_2, lulc$privbldg_2, lulc$other_2), 1, sum)
nbh_area_3 <- apply(cbind(lulc_recode$agveg_3, lulc_recode$nonagveg_3,
        lulc_recode$pubbldg_3, lulc$privbldg_3, lulc$other_3), 1, sum)
nbh_area <- cbind(nbh_area_1, nbh_area_2, nbh_area_3)
total_areas <- apply(nbh_area, 2, sum, na.rm=TRUE)

# And convert the LULC measurements from units of square meters to units that 
# are a percentage of total neighborhood area.
#lulc_sd <- lulc_recode[2:length(lulc_recode)]/nbh_area
# Use the below calculation to be consistent with the ABM definition of total 
# area.
lulc_sd <- lulc_recode[2:length(lulc_recode)]/lulc_recode$total_1
lulc_sd <- cbind(neighid=lulc_recode$neighid, lulc_sd)
lulc_pct_nbh <- data.frame(time_Robj=time_Robj,
        agveg=apply(lulc_sd[agveg_cols], 2, mean) * 100,
        nonagveg=apply(lulc_sd[nonagveg_cols], 2, mean) * 100,
        pubbldg=apply(lulc_sd[pubbldg_cols], 2, mean) * 100,
        privbldg=apply(lulc_sd[privbldg_cols], 2, mean) * 100,
        other=apply(lulc_sd[other_cols], 2, mean) * 100, row.names=NULL)
lulc_pct_nbh <- melt(lulc_pct_nbh, id.vars="time_Robj")
names(lulc_pct_nbh)[2:3] <- c("LULC_type", "area")

lulc_recode_nototal <- lulc_recode[-total_cols]
lulc_pct_chit <- apply(lulc_recode_nototal[2:length(lulc_recode_nototal)], 2, sum)
t1_cols <- grep('_1$', names(lulc_pct_chit))
t2_cols <- grep('_2$', names(lulc_pct_chit))
t3_cols <- grep('_3$', names(lulc_pct_chit))
lulc_pct_chit[t1_cols] <- (lulc_pct_chit[t1_cols] / total_areas[1]) * 100
lulc_pct_chit[t2_cols] <- (lulc_pct_chit[t2_cols] / total_areas[2]) * 100
lulc_pct_chit[t3_cols] <- (lulc_pct_chit[t3_cols] / total_areas[3]) * 100

lulc_pct_chit <- cbind(time_Robj=time_Robj, lulc_pct_chit)

lulc_pct_chit <- melt(lulc_pct_chit, id.vars="time_Robj")
names(lulc_pct_chit)[2:3] <- c("LULC_type", "area")

write.csv(lulc_pct_nbh, file="CVFS_monthly_LULC.csv", row.names=FALSE)
save(lulc_pct_nbh, file="CVFS_monthly_LULC.Rdata")

theme_update(theme_grey(base_size=18))
update_geom_defaults("line", aes(size=1))

qplot(time_Robj, area, geom="line", colour=LULC_type, xlab="Year",
        ylab="Mean Percentage of Neighborhood", data=lulc_pct_nbh)
ggsave("CVFS_monthly_LULC_pct_NBH.png", width=8.33, height=5.53, dpi=300)

# Make this plot to compare to the CVFS results in Axinn and Ghimire, 2007
qplot(time_Robj, area, geom="line", colour=LULC_type, xlab="Year",
        ylab="Percentage of Chitwan", data=lulc_pct_chit)
ggsave("CVFS_monthly_LULC_pct_Chitwan.png", width=8.33, height=5.53, dpi=300)
