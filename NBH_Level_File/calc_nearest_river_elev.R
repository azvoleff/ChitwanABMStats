library(maptools)
library(raster)
library(rgdal)

aster <- raster('R:/Data/Nepal/GIS/DEM/ASTERv2_N27E084/ASTGTM2_N27E084_dem.tif')
srtm <- raster('R:/Data/Nepal/GIS/DEM/srtm_53_07/srtm_53_07.tif')
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

# plot(aster)
# points(cvfs_nbhs)
# mean(nbh_aster_elevs)
# nbh_aster_elevs <- extract(aster, cvfs_nbhs)

# plot(srtm)
# points(cvfs_nbhs)
# nbh_srtm_elevs <- extract(srtm, cvfs_nbhs)
# mean(nbh_srtm_elevs)

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
cvfs_nbhs$NEAR_R_EVD <- cvfs_nbhs$NEAR_R_EV - cvfs_nbhs$ELEV_ASTER

writeOGR(cvfs_nbhs, '.', 'cvfs_nbhs_river_elev_data',
         driver='ESRI Shapefile', overwrite=TRUE)
cvfs_nbhs_river_elevs <- as.data.frame(cvfs_nbhs)
save(cvfs_nbhs_river_elevs, file='cvfs_nbhs_river_elev_data.Rdata')

# Ensure the data lines up properly:
#spplot(aster)
#points(cvfs_nbhs)
#spplot(cvfs_nbhs[cvfs_nbhs$NEAR_R_EV_DIFF < 0,], c('NEAR_R_EV_DIFF'))

start_pts <- coordinates(cvfs_nbhs)
rapti_end_pts <- coordinates(rapti_near_pts)
narayani_end_pts <- coordinates(narayani_near_pts)
near_river_end_pts <- rapti_end_pts
near_river_end_pts[cvfs_nbhs$NEAR_R == 'Narayani', ] <- narayani_end_pts[cvfs_nbhs$NEAR_R == 'Narayani', ]
narayani_line_list <- list()
rapti_line_list <- list()
nearest_river_line_list<- list()
for (n in 1:nrow(start_pts)) {
    narayani_line_list[[n]] <- Lines(Line(rbind(start_pts[n, ], narayani_end_pts[n, ])), ID=n)
    rapti_line_list[[n]] <- Lines(Line(rbind(start_pts[n, ], rapti_end_pts[n, ])), ID=n)
    nearest_river_line_list[[n]] <- Lines(Line(rbind(start_pts[n, ], near_river_end_pts[n, ])), ID=n)
}
narayani_lines <- SpatialLines(narayani_line_list, proj4string=CRS(projection(cvfs_nbhs)))
narayani_lines <- SpatialLinesDataFrame(narayani_lines, 
                                        data=data.frame(NEIGHID=cvfs_nbhs$NEIGHID))
rapti_lines <- SpatialLines(rapti_line_list, proj4string=CRS(projection(cvfs_nbhs)))
rapti_lines <- SpatialLinesDataFrame(rapti_lines, 
                                     data=data.frame(NEIGHID=cvfs_nbhs$NEIGHID))
nearest_river_lines <- SpatialLines(nearest_river_line_list, proj4string=CRS(projection(cvfs_nbhs)))
nearest_river_lines <- SpatialLinesDataFrame(nearest_river_lines, 
                                             data=data.frame(NEIGHID=cvfs_nbhs$NEIGHID))
writeOGR(narayani_lines, '.', 'nearest_river_narayani_lines',
         driver='ESRI Shapefile', overwrite=TRUE)
writeOGR(rapti_lines, '.', 'nearest_river_rapti_lines',
         driver='ESRI Shapefile', overwrite=TRUE)
writeOGR(nearest_river_lines, '.', 'nearest_river_lines',
         driver='ESRI Shapefile', overwrite=TRUE)
