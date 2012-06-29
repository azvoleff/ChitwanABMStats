require(foreign)

lu <- read.xport("/media/truecrypt5/Nepal/ICPSR_SupplementalData/Survey_converted/landuse.xpt")
# Exclude neighborhoods 152-172
lu$NEIGHID <- as.ordered(lu$NEIGHID)
lu <- lu[lu$NEIGHID <= 151,]

land.agveg1 <- with(lu, rowSums(cbind(BARI1, IKHET1, RKHET1)))
land.nonagveg1 <- with(lu, rowSums(cbind(GRASSC1, GRASSP1, PLANTC1, PLANTP1)))
land.privbldg1 <- with(lu, rowSums(cbind(HHRESID1, MILL1, OTRBLD1)))
land.pubbldg1 <- with(lu, rowSums(cbind(ROAD1, SCHOOL1, TEMPLE1)))
land.other1 <- with(lu, rowSums(cbind(CANAL1, POND1, RIVER1, SILT1, UNDVP1)))
land.total1 <- with(lu, rowSums(cbind(land.agveg1, land.nonagveg1, land.privbldg1, land.other1)))
lu.processed <- data.frame(NEIGHID=lu$NEIGHID, land.agveg1, land.nonagveg1, land.privbldg1, land.pubbldg1, land.other1, land.total1)

land.agveg2 <- with(lu, rowSums(cbind(BARI2, IKHET2, RKHET2)))
land.nonagveg2 <- with(lu, rowSums(cbind(GRASSC2, GRASSP2, PLANTC2, PLANTP2)))
land.privbldg2 <- with(lu, rowSums(cbind(HHRESID2, MILL2, OTRBLD2)))
land.pubbldg2 <- with(lu, rowSums(cbind(ROAD2, SCHOOL2, TEMPLE2)))
land.other2 <- with(lu, rowSums(cbind(CANAL2, POND2, RIVER2, SILT2, UNDVP2)))
land.total2 <- with(lu, rowSums(cbind(land.agveg2, land.nonagveg2, land.privbldg2, land.other2)))
lu.processed <- cbind(lu.processed, land.agveg2, land.nonagveg2, land.privbldg2, land.pubbldg2, land.other2, land.total2)

land.agveg3 <- with(lu, rowSums(cbind(BARI3, IKHET3, RKHET3)))
land.nonagveg3 <- with(lu, rowSums(cbind(GRASSC3, GRASSP3, PLANTC3, PLANTP3)))
land.privbldg3 <- with(lu, rowSums(cbind(HHRESID3, MILL3, OTRBLD3)))
land.pubbldg3 <- with(lu, rowSums(cbind(ROAD3, SCHOOL3, TEMPLE3)))
land.other3 <- with(lu, rowSums(cbind(CANAL3, POND3, RIVER3, SILT3, UNDVP3)))
land.total3 <- with(lu, rowSums(cbind(land.agveg3, land.nonagveg3, land.privbldg3, land.other3)))
lu.processed <- cbind(lu.processed, land.agveg3, land.nonagveg3, land.privbldg3, land.pubbldg3, land.other3, land.total3)

# Convert land areas expressed in square feet to square meters
lu.processed[2:6]  <- lu.processed[2:6] * .09290304

# Prior to merging, convert factor levels in lu (the NEIGHID column) to 
# numeric, then back to factor, so that neighIDs with less than three 
# characters (like 001) are instead represented as integers (001 -> 1). If this 
# is not done, neighborhoods will be lost as merge will think neighborhood 001 
# in dataframe lu has no complement in the neigh dataframe (where it is 
# represented with a NEIGHID of 1 instead of 001).
lu.processed$NEIGHID <- factor(as.numeric(lu.processed$NEIGHID))

t123lulc <- lu.processed

save(t123lulc, file="t123lulc.Rdata")
write.csv(t123lulc, "t123lulc.csv", row.names=FALSE)
