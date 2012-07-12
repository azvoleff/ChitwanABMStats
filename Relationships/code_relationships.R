# Code relationships by respondent ID

# This file is used to encode the household relationship in a more usable 
# format. Rather than coding relationships by subject ID as is done in the 
# grid, recode the relationships by full respondent ID, with one row per 
# respondent.
#
# The final respondent ID file is useful for tracking household fission as it 
# can allow tracking if, for example, oldest sons or oldest daughters move out 
# of the household at different rates than younger chilove out of the household 
# at different rates than younger children. The respondent ID file is also 
# necessary for the multinomial model of spouse age, as it allows retrieving 
# the ID and age of each person's spouse.

###############################################################################
# Load data and setup functions

# Hmisc is needed as hhreg is a "labelled" dataframe. If Hmisc is not included, 
# will get errors saying "cannot coerce class "labelled" into a data.frame"
library(Hmisc)
library(ggplot2)
library(foreign)

# The below code is borrowed, verbatim, from the ChitwanABM data_preprocess.R 
# script that is used in initializing the ABM. The only difference here is that 
# resampling is not used to fill in the NAs in the data.

###############################################################################
# First handle DS0004 - the census dataset
census <- read.xport("V:/Nepal/ICPSR_0538_Restricted/da04538-0004_REST.xpt")
load("V:/Nepal/CVFS_HHReg/hhreg126.Rdata")
# Exclude neighborhoods 152-172
census <- census[census$NEIGHID <= 151,]
# 5 people don't know their age, coded as -3 in dataset. Exclude these 
# individuals.
census$CENAGE[census$CENAGE==-3] <- NA
census.processed <- with(census, data.frame(RESPID, NEIGHID, CENAGE, CENGENDR))
census.processed$CENGENDR[census.processed$CENGENDR==1] <- "male"
census.processed$CENGENDR[census.processed$CENGENDR==2] <- "female"
# Eliminate the 5 people with unknown ages, and the 2 other NAs in the dataset
census.processed <- na.omit(census.processed)

###############################################################################
# Now handle DS0012, the individual data, to get desired family size 
# preferences and educational histories.
t1indiv <- read.xport("V:/Nepal/ICPSR_0538_Restricted/da04538-0012_REST.xpt")
# Exclude neighborhoods 152-172
t1indiv <- t1indiv[t1indiv$NEIGHID <= 151,]
columns <- grep('^(RESPID|F7)$', names(t1indiv))
desnumchild <- t1indiv[columns]
names(desnumchild)[2] <- "desnumchild"
# People who said "it is god's will" were coded as 97, and reasked the 
# question, in F9.
godswill <- which(desnumchild$desnumchild==97)
desnumchild[godswill,]$desnumchild <- desnumchild$F9[godswill]
# 2 people said a range from low to high. Here, take an average of the low and 
# high number, stored in F7A and F7B.
# TODO: Fix this:
child_range <- which(desnumchild$desnumchild==95)
desnumchild[child_range,]$desnumchild <- desnumchild$F7B[child_range]
#desnumchild[child_range,]$desnumchild <- desnumchild$F7A[child_range] / desnumchild$F7B[child_range]
# 28 people said they don't know. This is coded as -3 in the CVFS data.
# TODO: Also there are 22 individuals with # kids wanted in the thousands...  
# ask Dirgha what these are.
desnumchild$desnumchild[desnumchild$desnumchild > 1000] <- NA
desnumchild$desnumchild[desnumchild$desnumchild < 0] <- NA

# Now get years schooling data
schooling_col <- grep('^A1$', names(t1indiv))
schooling <- t1indiv[schooling_col]
names(schooling) <- "schooling"
# Leave schooling coded as years of schooling
schooling <- cbind(RESPID=t1indiv$RESPID, schooling)

# Now get childhood community context data. Below are the variables for 
# non-family services w/in a 1 hour walk at age 12.
# 	school D2
# 	health D8
# 	bus D12
# 	employer D16
# 	markey D22
cc_cols <- grep('^(D2|D8|D12|D16|D22)$', names(t1indiv))
ccchild <- t1indiv[cc_cols]
names(ccchild)[grep('^D2$', names(ccchild))] <- "child_school_1hr"
names(ccchild)[grep('^D8$', names(ccchild))] <- "child_health_1hr"
names(ccchild)[grep('^D12$', names(ccchild))] <- "child_bus_1hr"
names(ccchild)[grep('^D16$', names(ccchild))] <- "child_emp_1hr"
names(ccchild)[grep('^D22$', names(ccchild))] <- "child_market_1hr"
ccchild[ccchild < 0] <- NA
ccchild <- cbind(RESPID=t1indiv$RESPID, ccchild)

# Now get parent's characteristics data:
# parent's contraceptive use
# I17 father's work
# I11 father school (ever)
# I15 mother's work
# I7 mother school (ever)
# I19 mother's number of children
# I21 parents birth control ever
parents_char_cols <- grep('^(I17|I11|I15|I7|I19|I21)$', names(t1indiv))
parents_char <- t1indiv[parents_char_cols]
names(parents_char)[grep('^I17$', names(parents_char))] <- "father_work"
names(parents_char)[grep('^I11$', names(parents_char))] <- "father_school"
names(parents_char)[grep('^I15$', names(parents_char))] <- "mother_work"
names(parents_char)[grep('^I7$', names(parents_char))] <- "mother_school"
names(parents_char)[grep('^I19$', names(parents_char))] <- "mother_num_children"
names(parents_char)[grep('^I21$', names(parents_char))] <- "parents_contracep_ever"
parents_char[parents_char < 0] <- NA 
parents_char <- cbind(RESPID=t1indiv$RESPID, parents_char)

###############################################################################
# Now handle DS0013, the life history calendar data, to get information on what 
# women had births in the past year (so they can start out ineligible for 
# pregnancy).
lhc <- read.xport("M:/Data/Nepal/CVFS_Public/DS0013/04538-0013-Data.xpt")
# NOTE: lhc contains no NEIGHID column. Respondents in excluded neighborhoods 
# (greather than 151) will be dropped when the lhc data is merged later on in 
# the process.
cols.childL2053 <- grep('^C[0-9]*L2053$', names(lhc))
recent_birth <- lhc[cols.childL2053]
# Count "born" (coded as 1), "born, died in same year" (coded as 5), and "born, 
# lived away 6 months in same year" (coded as 6), as recent births. Recode 
# these three events as 1, code all other events as 0.
recent_birth[recent_birth == 5] <- 1
recent_birth[recent_birth == 6] <- 1
recent_birth[recent_birth != 1] <- 0
recent_birth[is.na(recent_birth)] <- 0
# Now add up across the rows - anything greather than or equal to 1 means there 
# was (at least) one birth in year 2053.
recent_birth <- apply(recent_birth, 1, sum, na.rm=TRUE)
recent_birth[recent_birth > 1] <- 1
recent_births <- data.frame(RESPID=lhc$RESPID, recent_birth)

###############################################################################
# Now handle DS0016 - the household relationship grid. Merge the census data 
# with the relationship grid.
hhrel <- read.xport("V:/Nepal/ICPSR_0538_Restricted/da04538-0016_REST.xpt")
# Exclude neighborhoods 152-172
hhrel <- hhrel[hhrel$NEIGHID <= 151,]
hhrel_processed  <- with(hhrel, data.frame(RESPID, HHID, SUBJECT, PARENT1, PARENT2, SPOUSE1, SPOUSE2, SPOUSE3))
hhrel_processed  <- merge(hhrel_processed, census.processed, by="RESPID")

# Merge the desnumchild data for desired family size. Note that I do not have 
# data for all individuals.
hhrel_processed <- merge(hhrel_processed, desnumchild, all.x=TRUE)

# Add the recent birth tags onto hhrel.procbessed
hhrel_processed <- merge(hhrel_processed, recent_births, all.x=TRUE)

# Merge the education data
hhrel_processed <- merge(hhrel_processed, schooling, all.x=TRUE)

# Merge the childhood non-family services data
hhrel_processed <- merge(hhrel_processed, ccchild, all.x=TRUE)
child_cols <- grep("child_", names(hhrel_processed))

# Merge the parent's characteristics data
hhrel_processed <- merge(hhrel_processed, parents_char, all.x=TRUE)
parents_char_cols <- grep("^(father_work|father_school|mother_work|mother_school|mother_num_children|parents_contracep_ever)$", names(hhrel_processed))



# Note that the HHID in hhrel_processed is the old format 5 character HHID, so 
# it should be NNNHH.  But when it was read in, the leading zeros were omitted.  
# So first reformat it to add in the leading zeros. Then chop off the last two 
# characters and add in a leading zero. In the end the HHID will be NNNHHH to 
# be consistent with the new format.
HHID_house_only <- substr(sprintf("%05i", as.integer(hhrel_processed$HHID)), 4, 5)
HHID_house_only <- sprintf("%03i", as.integer(HHID_house_only))
HHID_nbh_only <- substr(sprintf("%05i", as.integer(hhrel_processed$HHID)), 1, 3)
hhrel_processed$HHID <- paste(HHID_nbh_only, HHID_house_only, sep="")
# The same formatting problem exists for the RESPID columns as well. Fix them 
# so they will match up with the new format:
old_respID <- sprintf("%07i", hhrel_processed$RESPID)
NBHID <- sprintf("%03i", as.numeric(substr(old_respID,1,3)))
HHID <- sprintf("%03i", as.numeric(substr(old_respID,4,5)))
SUBJID <- sprintf("%03i", as.numeric(substr(old_respID,6,7)))
hhrel_processed$RESPID <- paste(NBHID, HHID, SUBJID, sep="")
# And also exists for the NEIGHID column. Fix it so it will be three 
# characters:
hhrel_processed$NEIGHID <- sprintf("%03i", as.integer(hhrel_processed$NEIGHID))

# Now read in the household registry data to get the ethnicity data. This 
# couldn't be done earlier because the RESPIDs in the hhreg data are in new 
# format, so this data can't be merged until after the above correction of the 
# RESPIDs to be in new format.
load("V:/Nepal/CVFS_HHReg/hhreg126.Rdata")
columns <- grep('^(respid|ethnic)$', names(hhreg))
hhreg <- hhreg[columns]
hhreg$ethnic <- factor(hhreg$ethnic, levels=c(1,2,3,4,5,6), labels=c("UpHindu",
        "HillTibeto", "LowHindu", "Newar", "TeraiTibeto", "Other"))
names(hhreg)[names(hhreg)=="respid"] <- "RESPID"
hhrel_processed  <- merge(hhrel_processed, hhreg, by="RESPID")

# Now recode the relationship data to replace the subject IDs used in the 
# dataframe with the actual respondent IDs for each subject. Respondent IDs are 
# defined as NNNHHHSSS where NNN is the neighborhood ID, HHH is the household 
# ID, and SSS is the subject ID.
convert_to_respIDs <- function(subject_columns, HHID) {
    # Ensure that anything where the subject ID is unknown is assigned an NA 
    # for the full ID, not just for the subject ID part.
    SUBJIDs <- sprintf("%03i", as.integer(subject_columns))
    RESPIDs <- paste(HHID, SUBJIDs, sep="")
    RESPIDs[grepl('NA', RESPIDs)] <- NA
    return(RESPIDs)
}
subject_cols <- grep('^(PARENT1|PARENT2|SPOUSE1|SPOUSE2|SPOUSE3)$', 
                    names(hhrel_processed))
# Make sure that subjects that are not in the data do not get subject IDs 
# assigned. If they are not in the survey their ID should be NA.
hhrel_processed[subject_cols][hhrel_processed[subject_cols]==0] <- NA
converted_columns <-t(mapply(convert_to_respIDs, 
                    as.list(as.data.frame(t(hhrel_processed[subject_cols]))), 
                    hhrel_processed$HHID))
hhrel_processed[subject_cols] <- converted_columns

# Now save the new hhrel_processed matrix as a CSV and an Rdata file.
hhrel_with_respIDs <- hhrel_processed
write.csv(hhrel_with_respIDs, "hhrel_with_respIDs.csv", row.names=FALSE)
save(hhrel_with_respIDs, file="hhrel_with_respIDs.Rdata")
