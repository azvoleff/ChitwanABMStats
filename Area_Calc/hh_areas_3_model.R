#!/usr/bin/Rscript
require(ggplot2)
require(foreign)

theme_update(theme_grey(base_size=18))
update_geom_defaults("line", aes(size=1))
update_geom_defaults("step", aes(size=1))

make_txtprob <- function(probs, binlims, param.name) {
    # param.name is the name used by the ChitwanABM model for this parameter.
    binlims <- paste(round(binlims, digits=4), collapse=", ")
    probs <- paste(round(probs, digits=4), collapse=", ")
    txtprob <- paste("'", param.name, "' : [((", binlims, "), (", probs,  "))] | validate_prob_dist]", sep="")
    return(txtprob)
}

hh_areas <- read.csv("hh_areas_T1_sq_meters.csv")
hh_areas <- hh_areas[hh_areas$area <= 1000, , drop=FALSE]
hh_areas <- hh_areas[hh_areas$area >= 40, , drop=FALSE]

neigh <- read.xport("V:/Nepal/ICPSR_0538_Restricted/da04538-0014_REST.xpt")
# Calculate the distance of each neighborhood from Narayanghat (using the 
# coordinates of the center of the road in the middle of the downtown area of 
# Narayanghat).
dist_nar_km <- sqrt((neigh$NX - 245848)**2 + (neigh$NY - 3066013)**2)
# Now convert from meters to kilometers
dist_nar_km <- (dist_nar_km / 1000)
neigh <- data.frame(NEIGHID=neigh$NEIGHID, dist_nar_km=dist_nar_km)

hh_areas <- merge(hh_areas, neigh, all.x=TRUE, all.y=FALSE)
hh_areas <- hh_areas[hh_areas$NEIGHID <= 151,]

hh_area_model <- lm(hh_areas$area ~ hh_areas$dist_nar_km)
summary(hh_area_model)

# Model provides essentially NO predictive power.
