library(foreign)
library(Hmisc)

# And add percentage and log percentage columns for agricultural vegetation
nbhhist <- read.xport("V:/Nepal/ICPSR_0538_Restricted/da04538-0014_REST.xpt")

ID_cols <- grep('^(NEIGHID|STRATA|NX|NY)$', names(nbhhist))
nbh_recode <- nbhhist[ID_cols]
nbh_recode$elec_avail <- nbhhist$ELEC52
nbh_recode$NEIGHID <- factor(sprintf("%03d", nbh_recode$NEIGHID))
nbh_recode$STRATA <- factor(nbh_recode$STRATA)

# Make a variable giving the average number of years non-family services have 
# been available in the neighborhood (for consistency with established models).
nbh_serv_cols <- grep('^(SCHLFT|HLTHFT|BUSFT|MARFT|EMPFT)[1-5][0-9]$', 
                      names(nbhhist))
nbh_recode$avg_yrs_services_lt15 <- rowSums(nbhhist[nbh_serv_cols] <= 15 ) / 5
nbh_recode$avg_yrs_services_lt30 <- rowSums(nbhhist[nbh_serv_cols] <= 30 ) / 5

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
lulc_orig <- read.xport("V:/Nepal/ICPSR_SupplementalData/Survey_converted/landuse.xpt")

agveg_t1 <- with(lulc_orig, rowSums(cbind(BARI1, IKHET1, RKHET1)))
nonagveg_t1 <- with(lulc_orig, rowSums(cbind(GRASSC1, GRASSP1, PLANTC1, PLANTP1)))
privbldg_t1 <- with(lulc_orig, rowSums(cbind(HHRESID1, MILL1, OTRBLD1)))
pubbldg_t1 <- with(lulc_orig, rowSums(cbind(ROAD1, SCHOOL1, TEMPLE1)))
other_t1 <- with(lulc_orig, rowSums(cbind(CANAL1, POND1, RIVER1, SILT1, UNDVP1)))

agveg_t2 <- with(lulc_orig, rowSums(cbind(BARI2, IKHET2, RKHET2)))
nonagveg_t2 <- with(lulc_orig, rowSums(cbind(GRASSC2, GRASSP2, PLANTC2, PLANTP2)))
privbldg_t2 <- with(lulc_orig, rowSums(cbind(HHRESID2, MILL2, OTRBLD2)))
pubbldg_t2 <- with(lulc_orig, rowSums(cbind(ROAD2, SCHOOL2, TEMPLE2)))
other_t2 <- with(lulc_orig, rowSums(cbind(CANAL2, POND2, RIVER2, SILT2, UNDVP2)))

agveg_t3 <- with(lulc_orig, rowSums(cbind(BARI3, IKHET3, RKHET3)))
nonagveg_t3 <- with(lulc_orig, rowSums(cbind(GRASSC3, GRASSP3, PLANTC3, PLANTP3)))
privbldg_t3 <- with(lulc_orig, rowSums(cbind(HHRESID3, MILL3, OTRBLD3)))
pubbldg_t3 <- with(lulc_orig, rowSums(cbind(ROAD3, SCHOOL3, TEMPLE3)))
other_t3 <- with(lulc_orig, rowSums(cbind(CANAL3, POND3, RIVER3, SILT3, UNDVP3)))

lulc <- data.frame(NEIGHID=lulc_orig$NEIGHID, agveg_t1, nonagveg_t1, privbldg_t1, 
                   pubbldg_t1, other_t1, agveg_t2, nonagveg_t2, privbldg_t2, 
                   pubbldg_t2, other_t2, agveg_t3, nonagveg_t3, privbldg_t3, 
                   pubbldg_t3, other_t3)
# Convert land areas expressed in square feet to square meters
lulc[2:ncol(lulc)]  <- lulc[2:ncol(lulc)] * .09290304

lulc$total_t1 <- apply(lulc[grep('_t1$', names(lulc))], 1, sum)
lulc$total_t2 <- apply(lulc[grep('_t2$', names(lulc))], 1, sum)
lulc$total_t3 <- apply(lulc[grep('_t3$', names(lulc))], 1, sum)

lulc$percagveg_t1 <- with(lulc, (agveg_t1/total_t1)*100)
lulc$percagveg_t2 <- with(lulc, (agveg_t2/total_t2)*100)
lulc$percagveg_t3 <- with(lulc, (agveg_t3/total_t3)*100)
lulc$logpercagveg_t1 <- log(lulc$percagveg_t1 + 1)
lulc$logpercagveg_t2 <- log(lulc$percagveg_t2 + 1)
lulc$logpercagveg_t3 <- log(lulc$percagveg_t3 + 1)

nbh_recode <- merge(nbh_recode, lulc)

save(nbh_recode, file="recoded_NBH_data.Rdata")
write.csv(nbh_recode, file="recoded_NBH_data.csv", row.names=FALSE)
