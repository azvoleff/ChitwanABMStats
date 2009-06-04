###############################################################################
# Loads the Chitwan Valley neighborhood locatino data, as well as DEM data 
# files.
###############################################################################

require("sp")
require("maptools")

psn <- read.csv("/home/azvoleff/Documents/SDSU/Wolong/data/wlpsn")
hs <- read.csv("/home/azvoleff/Documents/SDSU/Wolong/data/wlhs")

names(psn) <- c("psnID", "age", "hsID", "edu", "male", "RelationIndex", "momID", "dadID", "spouseStatus")

names(hs) <- c("hsID", "X", "Y", "landArea", "elePrice0", "eleVoltage0", "eleOutage0", "location")

# Calculate HH sizes
numPsns = data.frame()
for (ID in unique(psn$hsID)) {
    numPsn <- sum(psn$hsID==ID)
    numPsns <- rbind(numPsns, data.frame(hsID=ID, hsSize=numPsn))
}

hs <- merge(hs, numPsns)

# Calculate the grid box each household is in (to reference them to the DEM / 
# slope / LULC classes. For a resolution of 360 m, the Wolong landscape is 106 
# x 86 pixels (X * Y). The below values are taken from ModelSwarm.java lines 
# 88-100.
resolution = 360 # in meters
worldX = 106 # number of grid boxes in horizontal (x) direction
worldY = 86 # number of grid boxes in vertical (y) direction
ulXCoord = 310000 # in UTM?
ulYCoord = 3452000 # in UTM?

gridX = floor((hs$X - ulXCoord) / resolution)
gridY = floor((ulYCoord - hs$Y) / resolution)
hs <- cbind(hs, gridX, gridY)

# Now convert hs to a SpatialPointsDataFrame in UTM 48N
CRSString = "+proj=utm +zone=48 +ellps=WGS84 +units=m +no_defs"
hs <- SpatialPointsDataFrame(cbind(hs$X, hs$Y), hs, coords.nrs=c(2,3), proj4string=CRS(CRSString))

# Load the DEM
# readAsciiGrid function is in maptools

DEM <- readAsciiGrid("/home/azvoleff/Documents/SDSU/Wolong/data/demwlpart2_esrigrid.txt", proj4string=CRS(CRSString))
slope <- readAsciiGrid("/home/azvoleff/Documents/SDSU/Wolong/data/slopewlpart2_esrigrid.txt", proj4string=CRS(CRSString))
landclass <- readAsciiGrid("/home/azvoleff/Documents/SDSU/Wolong/data/classwlpart2_esrigrid.txt", proj4string=CRS(CRSString))

save(hs, psn, slope, DEM, landclass, file="wolong.Rdata")
