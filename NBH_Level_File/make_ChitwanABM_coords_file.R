library(maptools)
library(raster)
library(rgdal)

aster <- raster('R:/Data/Nepal/GIS/DEM/ASTERv2_N27E084/ASTGTM2_N27E084_dem.tif')
cvfs_nbhs <- readOGR('R:/Data/Nepal/GIS/CVFS_Data', 'cvfs_aster_elev_rivers')
orig_projection <- projection(cvfs_nbhs)
cvfs_nbhs$NEIGHID <- sprintf("%03i", cvfs_nbhs$NID)
cvfs_nbhs <- spTransform(cvfs_nbhs, CRS=CRS(projection(aster)))

#aster <- projectRaster(aster, crs=projection(cvfs_nbhs))

rapti_near_pts <- SpatialPoints(cbind(cvfs_nbhs$RAPTI_X, cvfs_nbhs$RAPTI_Y), 
                                proj4string=CRS(orig_projection))
rapti_near_pts <- spTransform(rapti_near_pts, CRS=CRS(projection(aster)))
narayani_near_pts <- SpatialPoints(cbind(cvfs_nbhs$NARAYANI_X, cvfs_nbhs$NARAYANI_Y), 
                                proj4string=CRS(orig_projection))
narayani_near_pts <- spTransform(narayani_near_pts, CRS=CRS(projection(aster)))

RAPTI_EV <- extract(aster, rapti_near_pts)
cvfs_nbhs <- spCbind(cvfs_nbhs, RAPTI_EV)
NARAYANI_EV <- extract(aster, narayani_near_pts)
cvfs_nbhs <- spCbind(cvfs_nbhs, NARAYANI_EV)
cvfs_nbhs$NEAR_R <- 'Rapti'
cvfs_nbhs$NEAR_R[cvfs_nbhs$NARAYANI_D < cvfs_nbhs$RAPTI_DIST] <- 'Narayani'

cvfs_nbhs$NEAR_R_DI <- cvfs_nbhs$RAPTI_DIST
cvfs_nbhs$NEAR_R_DI[cvfs_nbhs$NEAR_R == 'Narayani'] <- cvfs_nbhs[cvfs_nbhs$NEAR_R == 'Narayani', ]$NARAYANI_D
cvfs_nbhs$NEAR_R_EV <- cvfs_nbhs$RAPTI_EV
cvfs_nbhs$NEAR_R_EV[cvfs_nbhs$NEAR_R == 'Narayani'] <- cvfs_nbhs[cvfs_nbhs$NEAR_R == 'Narayani', ]$NARAYANI_EV
cvfs_nbhs$NEAR_R_EVD <-  cvfs_nbhs$ELEV_ASTER - cvfs_nbhs$NEAR_R_EV

cvfs_nbhs <- spTransform(cvfs_nbhs,
                         CRS=CRS('+proj=utm +zone=44 +ellps=WGS84 +datum=WGS84 +units=m +no_defs'))
cvfs_nbhs_out <- data.frame(NEIGHID_Long=cvfs_nbhs$NEIGHBORHO)
cvfs_nbhs_out$NEIGHID_Long <- substr(cvfs_nbhs_out$NEIGHID_Long, 2, 7)
cvfs_nbhs_out$NEIGHID <- as.numeric(substr(cvfs_nbhs_out$NEIGHID_Long, 4, 6))
cvfs_nbhs_out$x <- coordinates(cvfs_nbhs)[, 1]
cvfs_nbhs_out$y <- coordinates(cvfs_nbhs)[, 2]
cvfs_nbhs_out$elevation_above_river <- cvfs_nbhs$NEAR_R_EVD

cvfs_nbhs_out <- cvfs_nbhs_out[order(cvfs_nbhs_out$NEIGHID), ]

write.csv(cvfs_nbhs_out, file='neigh_coords.csv', row.names=FALSE)

# Ensure the data lines up properly:
#spplot(aster)
#points(cvfs_nbhs)
#spplot(cvfs_nbhs[cvfs_nbhs$NEAR_R_EV_DIFF < 0,], c('NEAR_R_EV_DIFF'))
