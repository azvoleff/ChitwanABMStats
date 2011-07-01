###############################################################################
# Calculates and prints summary statistics for time 1, 2, and 3 of the CVFS 
# land use mapping data.

library("foreign")
library("ggplot2")

DPI <- 300
WIDTH <- 6
HEIGHT <- 4
theme_update(theme_grey(base_size=24))
update_geom_defaults("point", aes(size=3))

lulc.orig <- read.xport("/media/Local_Secure/ICPSR_SupplementalData/Survey_converted/landuse.xpt")
lulc.orig$NEIGHID <- as.ordered(lulc.orig$NEIGHID)
lulc.orig <- lulc.orig[lulc.orig$NEIGHID <= 151,]
lulc.orig <- lulc.orig[order(names(lulc.orig))]

# Reorder columns for convenience.
lulc.orig <- lulc.orig[c(24, 8, 49, 53, 7, 1:6, 9:23, 25:48, 50:52, 54:ncol(lulc.orig))]
# Handle the land use data. Import land use data and make 5 different classes, 
# as is done by Axinn (2007). The classes are:
# 	Agricultural vegetation - BARI1, IKHET1, RKHET1
# 	Non-agricultural vegetation - GRASSC1, GRASSP1, PLANTC1, PLANTP1
# 	Private buildings - HHRESID1, MILL1, OTRBLD1
# 	Public buildings - ROAD1, SCHOOL1, TEMPLE1
# 	Other uses - CANAL1, POND1, RIVER1, SILT1, UNDVP1

# Convert land areas expressed in square feet to hectares
lulc.orig[5:length(lulc.orig)]  <- lulc.orig[5:length(lulc.orig)] * (9.290304*(10^-6))

lulc <-data.frame(agveg.1=with(lulc.orig, rowSums(cbind(BARI1, IKHET1, RKHET1))))
lulc <-cbind(lulc, nonagveg.1=with(lulc.orig, rowSums(cbind(GRASSC1, GRASSP1, PLANTC1, PLANTP1))))
lulc <-cbind(lulc, privbldg.1=with(lulc.orig, rowSums(cbind(HHRESID1, MILL1, OTRBLD1))))
lulc <-cbind(lulc, pubbldg.1=with(lulc.orig, rowSums(cbind(ROAD1, SCHOOL1, TEMPLE1))))
lulc <-cbind(lulc, other.1=with(lulc.orig, rowSums(cbind(CANAL1, POND1, RIVER1, SILT1, UNDVP1))))

lulc <-cbind(lulc, agveg.2=with(lulc.orig, rowSums(cbind(BARI2, IKHET2, RKHET2))))
lulc <-cbind(lulc, nonagveg.2=with(lulc.orig, rowSums(cbind(GRASSC2, GRASSP2, PLANTC2, PLANTP2))))
lulc <-cbind(lulc, privbldg.2=with(lulc.orig, rowSums(cbind(HHRESID2, MILL2, OTRBLD2))))
lulc <-cbind(lulc, pubbldg.2=with(lulc.orig, rowSums(cbind(ROAD2, SCHOOL2, TEMPLE2))))
lulc <-cbind(lulc, other.2=with(lulc.orig, rowSums(cbind(CANAL2, POND2, RIVER2, SILT2, UNDVP2))))

lulc <-cbind(lulc, agveg.3=with(lulc.orig, rowSums(cbind(BARI3, IKHET3, RKHET3))))
lulc <-cbind(lulc, nonagveg.3=with(lulc.orig, rowSums(cbind(GRASSC3, GRASSP3, PLANTC3, PLANTP3))))
lulc <-cbind(lulc, privbldg.3=with(lulc.orig, rowSums(cbind(HHRESID3, MILL3, OTRBLD3))))
lulc <-cbind(lulc, pubbldg.3=with(lulc.orig, rowSums(cbind(ROAD3, SCHOOL3, TEMPLE3))))
lulc <-cbind(lulc, other.3=with(lulc.orig, rowSums(cbind(CANAL3, POND3, RIVER3, SILT3, UNDVP3))))

# Makes sure that percentage calculations are not included in these columns
time1_cols <- grep('.*[.]1$', names(lulc))
time2_cols <- grep('.*[.]2$', names(lulc))
time3_cols <- grep('.*[.]3$', names(lulc))

lulc <- cbind(lulc, perc=(lulc[time1_cols]/rowSums(lulc[time1_cols]))*100)
lulc <- cbind(lulc, perc=(lulc[time2_cols]/rowSums(lulc[time2_cols]))*100)
lulc <- cbind(lulc, perc=(lulc[time3_cols]/rowSums(lulc[time3_cols]))*100)

lulc_summary <- colSums(lulc[time1_cols])
lulc_summary <- cbind(lulc_summary, colSums(lulc[time2_cols]))
lulc_summary <- cbind(lulc_summary, colSums(lulc[time3_cols]))
lulc_summary <- cbind(lulc_summary,
        lulc_summary[1:5,1]/sum(lulc_summary[1:5,1])*100)
lulc_summary <- cbind(lulc_summary,
        lulc_summary[1:5,2]/sum(lulc_summary[1:5,2])*100)
lulc_summary <- cbind(lulc_summary,
        lulc_summary[1:5,3]/sum(lulc_summary[1:5,3])*100)
lulc_summary <- rbind(lulc_summary, colSums(lulc_summary))
rownames(lulc_summary) <- c("agveg", "nonagveg", "privbldg", "pubbldg",
        "other", "total")
colnames(lulc_summary) <- c("1996", "2001", "2008", "1996_perc",
        "2001_perc", "2008_perc")
lulc_summary <- round(lulc_summary, 1)
write.csv(lulc_summary, file="lulc_summary_stats.csv")

#qplot()
#ggsave("lulc_summary.png", width=WIDTH, height=HEIGHT, dpi=DPI)
