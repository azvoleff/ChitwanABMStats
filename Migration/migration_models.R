###############################################################################
# Runs logistics regressions to predict the hazard of migration from a set of 
# variables using lmer, and glm.
###############################################################################
require("foreign")
require("lme4")
require("MASS")

# Load the migration data (stored in a dataframe as person-months)
load("migrations_allvars.Rdata")

# Merge the climate data
climate_data <- read.csv("indicator_2.txt")
climate_data$climate <- climate_data$climate / 100 # express climate in 100s of mm
migrations <- merge(climate_data, migrations)

migrations$bae25 <- migrations$bae25 / 100
migrations$bab25 <- migrations$bab25 / 100

# First calculate summary statistics (at the individual level, so make a new 
# dataframe that only has one record per respid. Here we don't care about the 
# migr variable, or time-dependent variables, since we only want to know about 
# variables that were defined in 1996.
unique_indiv <- migrations[!duplicated(migrations$respid),]
#print(summary(unique_indiv))

# Convert time to represent seasons
month <- migrations$time%%12
migrations <- cbind(migrations, month)

season <- month
season[month<2] <- 0
season[month>5] <- 0
season[month!=0] <- 1

# Fit the local migration models
# Consider only local migrations (coded as 1)
print("Fitting the local migration models...")
outcome.local <- migrations$migr
outcome.local[outcome.local!=1] <- 0
#local_mixed_model <- with(migrations, lmer(outcome.local ~ cengendr + salyn +
#    schl1996 + yrsschl + age1524 + age2534 + age3544 + time + time**2 +
#    (bae2 + bae25 + baa43 + bab4 | HHID), family=binomial))

local_model <- with(migrations, glm(outcome.local ~ cengendr + salyn + schl1996 +
    yrsschl + age1524 + age2534 + age3544 + time + time**2 + bae2 + bae25 +
    baa43 + bab4, family=binomial))
results.local <- summary(local_model)$coefficients[,c(0,1,3)]
results.local[,1] <- exp(results.local[,1])
results.local <- round(results.local, digits=2)
write.matrix(results.local, file="local_model.csv", sep=",")

local_model_climate <- with(migrations, glm(outcome.local ~ cengendr + salyn +
    schl1996 + yrsschl + age1524 + age2534 + age3544 + time + time**2 + bae2 +
    bae25 + baa43 + bab4 + climate, family=binomial))
results.local.climate <- summary(local_model_climate)$coefficients[,c(1,3)]
results.local.climate[,1] <- exp(results.local.climate[,1])
results.local.climate <- round(results.local.climate, digits=2)
write.matrix(results.local.climate, file="local_model_climate.csv", sep=",")

# Now fit the distant migration models
print("Fitting the distant migration models...")
outcome.distant <- migrations$migr
outcome.distant[outcome.distant!=2] <- 0
outcome.distant[outcome.distant==2] <- 1
#distant_mixed_model <- with(migrations, lmer(outcome.distant ~ cengendr + salyn +
#    schl1996 + yrsschl + age1524 + age2534 + age3544 + time + time**2 +
#    (bae2 + bae25 + baa43 + bab4 | HHID), family=binomial))

distant_model <- with(migrations, glm(outcome.distant ~ cengendr + salyn + schl1996 +
    yrsschl + age1524 + age2534 + age3544 + time + time**2 + bae2 + bae25 +
    baa43 + bab4, family=binomial))
results.distant <- summary(distant_model)$coefficients[,c(0,1,3)]
results.distant[,1] <- exp(results.distant[,1])
results.distant <- round(results.distant, digits=2)
write.matrix(results.distant, file="distant_model.csv", sep=",")

distant_model_climate <- with(migrations, glm(outcome.distant ~ cengendr + salyn +
    schl1996 + yrsschl + age1524 + age2534 + age3544 + time + time**2 + bae2 +
    bae25 + baa43 + bab4 + climate, family=binomial))
results.distant.climate <- summary(distant_model_climate)$coefficients[,c(0,1,3)]
results.distant.climate[,1] <- exp(results.distant.climate[,1])
results.distant.climate <- round(results.distant.climate, digits=2)
write.matrix(results.distant.climate, file="distant_model_climate.csv", sep=",")

save(local_model, local_model_climate, distant_model, distant_model_climate, file="models_logistic.Rdata")
