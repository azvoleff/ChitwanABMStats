###############################################################################
# Instead of looking at hazard of migration, considers number of migrants per 
# month in western Chitwan Valley (WCV) in several categories, to see if there 
# is any seasonality, separate from climate.
#     1) Local migration (from within WCV to within WCV)
#     3) Distant out-migration (from WCV to outside WCV)
#     2) Distant in-migration (from outside of WCV to WCV)
# In future, also consider WHERE within Chitwan migrants are primarily 
# locating, and from WHERE within Chitwan migrants are primarily leaving.
###############################################################################

library("Hmisc")
load("/media/Restricted/Data/CVFS_R_format/hhreg.Rdata")

place.cols <- grep("place[0-9]*$", names(hhreg))
migr <- hhreg[place.cols[-1]] -  hhreg[place.cols[-length(place.cols)]]

# Don't count births as migrants
age.cols <- grep("age[0-9]*$", names(hhreg))
age.cols <- age.cols[-1]
migr[hhreg[age.cols]==1] <- 0

migr.any <- migr
migr.any[migr.any!=0] <- 1

