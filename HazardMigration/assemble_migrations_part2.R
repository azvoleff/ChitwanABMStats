###############################################################################
# Reads in the raw migration Rdata file, and processes datasets hhcensus, t1ag, 
# and lhc to add additional covariates to the migrations data.
###############################################################################
library("foreign")
library("Hmisc")

# Load the migration data (stored in a dataframe as person-months)
load("migrations_raw.Rdata")

# Gather vars from hhcensus (Household Census)
#hhcensus <- sasxport.get("/media/Restricted/Data/ICPSR_0538_Restricted/da04538-0004_REST.xpt")
hhcensus <- sasxport.get("/media/ENCRYPTDRV/Data/ICPSR_0538_Restricted/da04538-0004_REST.xpt")

hh_vars <- with(hhcensus, data.frame(respid, hhid, neighid, cengendr))
# Make cenGENDER 1 for female, 0 for male
hh_vars$cengendr <- hh_vars$cengendr - 1

# MAKE "birth cohorts":
age1524 <- hhcensus$cenage
age1524[15<=age1524 & age1524<=24] <- 1
age1524[age1524!=1] <- 0

age2534 <- hhcensus$cenage
age2534[25<=age2534 & age2534<=34] <- 1
age2534[age2534!=1] <- 0

age3544 <- hhcensus$cenage
age3544[35<=age3544 & age3544<=44] <- 1
age3544[age3544!=1] <- 0

age4559 <- hhcensus$cenage
age4559[45<=age4559 & age4559<=59] <- 1
age4559[age4559!=1] <- 0

hh_vars <- cbind(hh_vars, age1524, age2534, age3544, age4559)
migrations <- merge(hh_vars, migrations)
rm(hhcensus)

# Gather vars from t1ag (Baseline Agriculture Data)
#t1ag <- sasxport.get("/media/Restricted/Data/da04538-0002_REST.xpt")
t1ag <- sasxport.get("/media/ENCRYPTDRV/Data/ICPSR_0538_Restricted/da04538-0002_REST.xpt")
baseag_vars <- with(t1ag, data.frame(hhid, bae25, baa43, bab4))
bae2 <- t1ag$bae2
bae2[bae2!=2] <- 0
bae2[bae2==2] <- 1
baseag_vars <- cbind(baseag_vars, bae2)
migrations <- merge(baseag_vars, migrations)
rm(t1ag)

# Gather vars from lhc (Life History Calendar)
lhc <- sasxport.get("/home/azvoleff/Data/CVFS_Public/DS0013/04538-0013-Data.xpt")
lhc_vars <- with(lhc, data.frame(respid, salyn))
# Need to also calculate "years of schooling (prior to 1996)" from the 
# schl1994-2053 variables. (Time is expressed in Nepali years).  schl1994-2053 
# are columns 4031 through 4090. These columns give years of schooling for the 
# respondent. The respondent's childrens' years of schooling have to be dealt 
# with separately.
yrsschl <- lhc[,4031:4090] # STORE schl1994-schl2053 in YRSCschl
yrsschl[is.na(yrsschl)] <- 0 # Treat a year prior to birth as zero
yrsschl[yrsschl==1] <- .5 # Treat a year when school started as .5
yrsschl[yrsschl==2] <- 1 # Treat a year full year of school as 1
yrsschl[yrsschl==3] <- 1 # Treat a year when school ended as .5
yrsschl[yrsschl==4] <- 0 # Treat a year when school started and ended as .5
yrsschl <- apply(yrsschl, 1, sum)
lhc_vars <- cbind(lhc_vars, yrsschl)
# Also compute schl1996 (whether in school in 1996)
schl1996 <- lhc[,4090] # Column 4090 is schl2053 (western year 1996)
schl1996[is.na(schl1996)] <- 0 # Treat a year prior to birth as not in school
schl1996[schl1996==1] <- 1 # Treat a year when school started as in school
schl1996[schl1996==2] <- 1 # Treat a year full year of school as in school
schl1996[schl1996==3] <- 0 # Treat a year when school ended as not in school
schl1996[schl1996==4] <- 0 # Treat a year when school started and ended as not in school
lhc_vars <- cbind(lhc_vars, schl1996)
# Now deal with the children. There dataset stores data on 15 separate 
# children. Years of school are coded as, for instance: C1S1994, which would be 
# a dichotomous variable for whether child 1 was in school in 1994.
# So loop over the child IDs (from child 1 to child 15). For each child ID sum 
# the years of education, and get their respids from the CIDn variable (where n 
# is the child ID). CID1 is in column 916.
# C1S1994 is in column 1832, and C1S2053 is in column 1891.
#for (child_num in 1:15) {
#    child_respid <- lhc[,915 + child_num]
#    child_yrsschl <- 
#    apply(lhc[,1832+(60*(child_num-1)):1831+(60*(child_num))], 1, sum)
#    child_yrsschl <- data.frame(respid=child_respid, yrsschl=child_yrsschl)
#}
migrations <- merge(lhc_vars, migrations)
rm(lhc)
save(migrations, file="migrations_allvars.Rdata")
