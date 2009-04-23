###############################################################################
# Runs logistics regressions to predict the hazard of migration from a set of 
# variables using lmer, and glm.
###############################################################################
library("foreign")
library("lme4")

# Load the migration data (stored in a dataframe as person-months)
load("migrations_allvars.Rdata")

# Merge the climate data
climate_data <- read.csv("indicator_2.txt")
climate_data$CLIMATE <- climate_data$CLIMATE / 100 # express climate in 100s of mm
migrations <- merge(climate_data, migrations)

migrations$BAE25 <- migrations$BAE25 / 100
migrations$BAB25 <- migrations$BAB25 / 100

# First calculate summary statistics (at the individual level, so make a new 
# dataframe that only has one record per RESPID. Here we don't care about the 
# MIGR variable, or time-dependent variables, since we only want to know about 
# variables that were defined in 1996.
unique_indiv <- migrations[!duplicated(migrations$RESPID),]
#print(summary(unique_indiv))

# Convert TIME to represent seasons
MONTH <- migrations$TIME%%12
migrations <- cbind(migrations, MONTH)

SEASON <- MONTH
SEASON[MONTH<2] <- 0
SEASON[MONTH>5] <- 0
SEASON[MONTH!=0] <- 1

# Fit the local migration models
# Consider only local migrations (coded as 1)
print("Fitting the local migration models...")
outcome <- migrations$MIGR
outcome[outcome!=1] <- 0
#local_mixed_model <- with(migrations, lmer(outcome ~ CENGENDR + SALYN +
#    SCHL1996 + YRSSCHL + AGE1524 + AGE2534 + AGE3544 + TIME + TIME**2 +
#    (BAE2 + BAE25 + BAA43 + BAB4 | HHID), family=binomial))

local_model <- with(migrations, glm(outcome ~ CENGENDR + SALYN + SCHL1996 + YRSSCHL + 
    AGE1524 + AGE2534 + AGE3544 + TIME + TIME**2 + BAE2 + BAE25 + BAA43 + BAB4 + SEASON,
    family=binomial))

local_model_climate <- with(migrations, glm(outcome ~ CENGENDR + SALYN + SCHL1996 + YRSSCHL + 
    AGE1524 + AGE2534 + AGE3544 + TIME + TIME**2 + BAE2 + BAE25 + BAA43 + BAB4 + CLIMATE,
    family=binomial))

results <- summary(local_model_climate)$coefficients[,c(1,3)]
results[,1] <- exp(results[,1])
results <- round(results, digits=2)
write.matrix(results, file="local_model_climate.csv", sep=",")

# Now fit the distant migration models
# Now fit the distant migration models
print("Fitting the distant migration models...")
outcome <- migrations$MIGR
outcome[outcome!=2] <- 0
outcome[outcome==2] <- 1
#distant_mixed_model <- with(migrations, lmer(outcome ~ CENGENDR + SALYN +
#    SCHL1996 + YRSSCHL + AGE1524 + AGE2534 + AGE3544 + TIME + TIME**2 +
#    (BAE2 + BAE25 + BAA43 + BAB4 | HHID), family=binomial))

distant_model <- with(migrations, glm(outcome ~ CENGENDR + SALYN + SCHL1996 + YRSSCHL + 
    AGE1524 + AGE2534 + AGE3544 + TIME + TIME**2 + BAE2 + BAE25 + BAA43 + BAB4,
    family=binomial))

distant_model_climate <- with(migrations, glm(outcome ~ CENGENDR + SALYN + SCHL1996 + YRSSCHL + 
    AGE1524 + AGE2534 + AGE3544 + TIME + TIME**2 + BAE2 + BAE25 + BAA43 + BAB4 + CLIMATE,
    family=binomial))

results <- summary(distant_model_climate)$coefficients[,c(0,1,3)]
results[,1] <- exp(results[,1])
results <- round(results, digits=2)
write.matrix(results, file="distant_model_climate.csv", sep=",")

save(local_model, local_model_climate, distant_model, distant_model_climate, file="models_logistic.Rdata")
