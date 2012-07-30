forest_dist <- read.csv("V:/Nepal/ICPSR_0538_Restricted/Recode/CVFS_NBHs_forest_distances.csv")

forest_dist$closest_type[forest_dist$BZ_meters < forest_dist$CNP_meters] <- "BZ"
forest_dist$closest_type[forest_dist$BZ_meters >= forest_dist$CNP_meters] <- "CNP"
forest_dist$closest_meters[forest_dist$closest_type == "BZ"] <- forest_dist$BZ_meters[forest_dist$closest_type == "BZ"]
forest_dist$closest_meters[forest_dist$closest_type == "CNP"] <- forest_dist$CNP_meters[forest_dist$closest_type == "CNP"]

table(forest_dist$closest_type)

hist(forest_dist$closest_meters)
forest_dist$closest_meters_cat <- cut(forest_dist$closest_meters, breaks=seq(0, 12000, 3000))
table(forest_dist$closest_meters_cat)
plot(forest_dist$closest_meters_cat)
sum(table(forest_dist$closest_meters_cat))

hist(forest_dist$CNP_meters)
forest_dist$CNP_dist_cat <- cut(forest_dist$CNP_meters, breaks=seq(0, 15000, 3000))
table(forest_dist$CNP_dist_cat)
plot(forest_dist$CNP_dist_cat)
sum(table(forest_dist$CNP_dist_cat))

hist(forest_dist$BZ_meters)
forest_dist$BZ_dist_cat <- cut(forest_dist$BZ_meters, breaks=c(0, 5000, 10000, 15000, 22000))
table(forest_dist$BZ_dist_cat)
plot(forest_dist$BZ_dist_cat)
sum(table(forest_dist$BZ_dist_cat))

save(forest_dist, file="CVFS_NBHs_forest_distances_recode.Rdata")
write.csv(forest_dist, file="CVFS_NBHs_forest_distances_recode.csv", row.names=FALSE)
