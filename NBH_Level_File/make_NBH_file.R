library(foreign)
library(Hmisc)

# And add percentage and log percentage columns for agricultural vegetation
nbhhist <- read.xport("W:/Nepal/ICPSR_0538_Restricted/da04538-0014_REST.xpt")

ID_cols <- grep('^(NEIGHID|STRATA|NX|NY)$', names(nbhhist))
nbh_recode <- nbhhist[ID_cols]
nbh_recode$ELEC_1996 <- nbhhist$ELEC52
nbh_recode$NEIGHID <- factor(sprintf("%03d", nbh_recode$NEIGHID))
nbh_recode$STRATA <- factor(nbh_recode$STRATA)

# Make a variable giving the average number of years non-family services have 
# been available in the neighborhood (for consistency with established models).
nbh_serv_cols <- grep('^(SCHLFT|HLTHFT|BUSFT|MARFT|EMPFT)[1-5][0-9]$', 
                      names(nbhhist))
nbh_recode$avg_yrs_services_lt15_1996 <- rowSums(nbhhist[nbh_serv_cols] <= 15 ) / 5
nbh_recode$avg_yrs_services_lt30_1996  <- rowSums(nbhhist[nbh_serv_cols] <= 30 ) / 5

# Merge the 1996 non-family services data.  Below are the variables for 
# non-family services w/in a 1 hour walk at age 12.
# 	school SCHLFT52
# 	health HLTHFT52
# 	bus BUSFT52
# 	employer EMPFT52
# 	market MARFT52
nonfam1996 <- nbhhist[grep('^(SCHLFT52|HLTHFT52|BUSFT52|EMPFT52|MARFT52)$', 
                           names(nbhhist))]
names(nonfam1996) <- sub('52', '_1996', names(nonfam1996))
nbh_recode <- cbind(nbh_recode, nonfam1996)

# Calculate the distance of each neighborhood from Narayanghat (using the 
# coordinates of the center of the road in the middle of the downtown area of 
# Narayanghat), and convert from meters to kilometers
nbh_recode$dist_nara <- (sqrt((nbh_recode$NX - 245848)**2 + (nbh_recode$NY - 3066013)**2)) / 1000

# Load and recode the LULC data
lulc_orig <- read.xport("W:/Nepal/ICPSR_SupplementalData/Survey_converted/landuse.xpt")

agveg_1996 <- with(lulc_orig, rowSums(cbind(BARI1, IKHET1, RKHET1)))
nonagveg_1996 <- with(lulc_orig, rowSums(cbind(GRASSC1, GRASSP1, PLANTC1, PLANTP1)))
privbldg_1996 <- with(lulc_orig, rowSums(cbind(HHRESID1, MILL1, OTRBLD1)))
pubbldg_1996 <- with(lulc_orig, rowSums(cbind(ROAD1, SCHOOL1, TEMPLE1)))
other_1996 <- with(lulc_orig, rowSums(cbind(CANAL1, POND1, RIVER1, SILT1, UNDVP1)))

agveg_2000 <- with(lulc_orig, rowSums(cbind(BARI2, IKHET2, RKHET2)))
nonagveg_2000 <- with(lulc_orig, rowSums(cbind(GRASSC2, GRASSP2, PLANTC2, PLANTP2)))
privbldg_2000 <- with(lulc_orig, rowSums(cbind(HHRESID2, MILL2, OTRBLD2)))
pubbldg_2000 <- with(lulc_orig, rowSums(cbind(ROAD2, SCHOOL2, TEMPLE2)))
other_2000 <- with(lulc_orig, rowSums(cbind(CANAL2, POND2, RIVER2, SILT2, UNDVP2)))

agveg_2007 <- with(lulc_orig, rowSums(cbind(BARI3, IKHET3, RKHET3)))
nonagveg_2007 <- with(lulc_orig, rowSums(cbind(GRASSC3, GRASSP3, PLANTC3, PLANTP3)))
privbldg_2007 <- with(lulc_orig, rowSums(cbind(HHRESID3, MILL3, OTRBLD3)))
pubbldg_2007 <- with(lulc_orig, rowSums(cbind(ROAD3, SCHOOL3, TEMPLE3)))
other_2007 <- with(lulc_orig, rowSums(cbind(CANAL3, POND3, RIVER3, SILT3, UNDVP3)))

lulc <- data.frame(NEIGHID=lulc_orig$NEIGHID, agveg_1996, nonagveg_1996, privbldg_1996, 
                   pubbldg_1996, other_1996, agveg_2000, nonagveg_2000, privbldg_2000, 
                   pubbldg_2000, other_2000, agveg_2007, nonagveg_2007, privbldg_2007, 
                   pubbldg_2007, other_2007)
# Convert land areas expressed in square feet to square meters
lulc[2:ncol(lulc)]  <- lulc[2:ncol(lulc)] * .09290304

lulc$total_1996 <- apply(lulc[grep('_1996$', names(lulc))], 1, sum)
lulc$total_2000 <- apply(lulc[grep('_2000$', names(lulc))], 1, sum)
lulc$total_2007 <- apply(lulc[grep('_2007$', names(lulc))], 1, sum)

lulc$percagveg_1996 <- with(lulc, (agveg_1996/total_1996)*100)
lulc$percagveg_2000 <- with(lulc, (agveg_2000/total_2000)*100)
lulc$percagveg_2007 <- with(lulc, (agveg_2007/total_2007)*100)
lulc$logpercagveg_1996 <- log(lulc$percagveg_1996 + 1)
lulc$logpercagveg_2000 <- log(lulc$percagveg_2000 + 1)
lulc$logpercagveg_2007 <- log(lulc$percagveg_2007 + 1)

nbh_recode <- merge(nbh_recode, lulc)

save(nbh_recode, file="recoded_NBH_data.Rdata")
write.csv(nbh_recode, file="recoded_NBH_data.csv", row.names=FALSE)
