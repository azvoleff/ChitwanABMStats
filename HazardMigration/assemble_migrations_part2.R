###############################################################################
# Reads in the raw migration Rdata file, and processes datasets DS0004, DS0002, 
# and DS0013 to add additional covariates to the migrations data.
###############################################################################
library("foreign")
library("Hmisc")

# Load the migration data (stored in a dataframe as person-months)
load("migrations_raw.Rdata")

# Gather vars from DS0004 (Household Census)
DS0004 <- sasxport.get("/media/Restricted/Data/ICPSR_0538_Restricted/da04538-0004_REST.xpt")
hh_vars <- with(DS0004, data.frame(RESPID, HHID, NEIGHID, CENGENDR))
# Make CENGENDER 1 for female, 0 for male
hh_vars$CENGENDR <- hh_vars$CENGENDR - 1
# MAKE "birth cohorts":
AGE1524 <- DS0004$CENAGE
AGE1524[15<=AGE1524 & AGE1524<=24] <- 1
AGE1524[AGE1524!=1] <- 0

AGE2534 <- DS0004$CENAGE
AGE2534[25<=AGE2534 & AGE2534<=34] <- 1
AGE2534[AGE2534!=1] <- 0

AGE3544 <- DS0004$CENAGE
AGE3544[35<=AGE3544 & AGE3544<=44] <- 1
AGE3544[AGE3544!=1] <- 0

AGE4559 <- DS0004$CENAGE
AGE4559[45<=AGE4559 & AGE4559<=59] <- 1
AGE4559[AGE4559!=1] <- 0

hh_vars <- cbind(hh_vars, AGE1524, AGE2534, AGE3544, AGE4559)
migrations <- merge(hh_vars, migrations)
rm(DS0004)

# Gather vars from DS0002 (Baseline Agriculture Data)
DS0002 <- read.xport("/media/Restricted/Data/da04538-0002_REST.xpt")
baseag_vars <- with(DS0002, data.frame(HHID, BAE25, BAA43, BAB4))
BAE2 <- DS0002$BAE2
BAE2[BAE2!=2] <- 0
BAE2[BAE2==2] <- 1
baseag_vars <- cbind(baseag_vars, BAE2)
migrations <- merge(baseag_vars, migrations)
rm(DS0002)

# Gather vars from DS0013 (Life History Calendar)
DS0013 <- read.xport("/media/Public_Data/CVFSData/ICPSR_04538/DS0013/04538-0013-Data.xpt")
lhc_vars <- with(DS0013, data.frame(RESPID, SALYN))
# Need to also calculate "years of schooling (prior to 1996)" from the 
# SCHL1994-2053 variables. (Time is expressed in Nepali years).  SCHL1994-2053 
# are columns 4031 through 4090. These columns give years of schooling for the 
# respondent. The respondent's childrens' years of schooling have to be dealt 
# with separately.
YRSSCHL <- DS0013[,4031:4090] # STORE SCHL1994-SCHL2053 in YRSCSCHL
YRSSCHL[is.na(YRSSCHL)] <- 0 # Treat a year prior to birth as zero
YRSSCHL[YRSSCHL==1] <- .5 # Treat a year when school started as .5
YRSSCHL[YRSSCHL==2] <- 1 # Treat a year full year of school as 1
YRSSCHL[YRSSCHL==3] <- 1 # Treat a year when school ended as .5
YRSSCHL[YRSSCHL==4] <- 0 # Treat a year when school started and ended as .5
YRSSCHL <- apply(YRSSCHL, 1, sum)
lhc_vars <- cbind(lhc_vars, YRSSCHL)
# Also compute SCHL1996 (whether in school in 1996)
SCHL1996 <- DS0013[,4090] # Column 4090 is SCHL2053 (western year 1996)
SCHL1996[is.na(SCHL1996)] <- 0 # Treat a year prior to birth as not in school
SCHL1996[SCHL1996==1] <- 1 # Treat a year when school started as in school
SCHL1996[SCHL1996==2] <- 1 # Treat a year full year of school as in school
SCHL1996[SCHL1996==3] <- 0 # Treat a year when school ended as not in school
SCHL1996[SCHL1996==4] <- 0 # Treat a year when school started and ended as not in school
lhc_vars <- cbind(lhc_vars, SCHL1996)
# Now deal with the children. There dataset stores data on 15 separate 
# children. Years of school are coded as, for instance: C1S1994, which would be 
# a dichotomous variable for whether child 1 was in school in 1994.
# So loop over the child IDs (from child 1 to child 15). For each child ID sum 
# the years of education, and get their RESPIDs from the CIDn variable (where n 
# is the child ID). CID1 is in column 916.
# C1S1994 is in column 1832, and C1S2053 is in column 1891.
#for (child_num in 1:15) {
#    child_RESPID <- DS0013[,915 + child_num]
#    child_YRSSCHL <- 
#    apply(DS0013[,1832+(60*(child_num-1)):1831+(60*(child_num))], 1, sum)
#    child_YRSSCHL <- data.frame(RESPID=child_RESPID, YRSSCHL=child_YRSSCHL)
#}
migrations <- merge(lhc_vars, migrations)
rm(DS0013)
save(migrations, file="migrations_allvars.Rdata")
