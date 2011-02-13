###############################################################################
# Reshapes the household registry data so that it can be used in long format, 
# simplifying calculation of crude birth rates, crude death rates, household 
# sizes, etc.

load("/media/Secure_Data/CVFS_HHReg/hhreg126.Rdata")

hhid.cols <- grep('^hhid[0-9]*$', names(hhreg))
livng.cols <- grep('^livng[0-9]*$', names(hhreg))
preg.cols <- grep('^preg[0-9]*$', names(hhreg))
place.cols <- grep('^place[0-9]*$', names(hhreg))

neededcols <- c(hhid.cols, livng.cols, preg.cols, place.cols)

# Note that column 1 of the hrheg dataframe is the respid column
hhregSubset <- data.frame(hhreg[c(1, neededcols)])
hhreg.reshaped <- reshape(hhregSubset, direction="long", varying=2:ncol(hhregSubset), sep="", idvar="respid", ids=hhregSubset$respid, timevar="month")

save(hhreg.reshaped, file="/media/Secure_Data/CVFS_HHRe/hhreg126_reshaped.Rdata")
