###############################################################################
# Runs logistics regressions to predict the hazard of migration from a set of 
# variables using lmer, and glm.
###############################################################################
library(Hmisc)
require(lme4)

# Load the migration data (stored in a dataframe as person-months)
#load("migrations_longformat-6_months_away.Rdata")
load("migrations_longformat-1_months_away.Rdata")
migrations <- LDmigrations.long

# Calculate summary statistics (at the individual level, so make a new 
# dataframe that only has one record per respid. Here we don't care about the 
# migr variable, or time-dependent variables, since we only want to know about 
# variables that were defined in 1996.
#unique_indiv <- migrations[!duplicated(migrations$respid),]
#print(summary(unique_indiv))

# Convert time to represent seasons
#month <- migrations$time%%12
#migrations <- cbind(migrations, month)

# Now fit the distant migration models
print("Fitting the distant migration models...")
# Eliminate the two rows that have FALSE instead of just 0/1 coding:
migrations <- migrations[!(migrations$place==FALSE),]
migrations$place <- as.numeric(migrations$place)
# Recode "other" ethnicity to Terai tibeto-burmese
migrations$ethnic[migrations$ethnic==6] <- 5
migrations$ethnic <- as.factor(migrations$ethnic)
migrations$gender <- as.factor(migrations$gender)

# Note that place variable is the outcome variable. 1 is an LD migration, 0 is 
# no movement.
distant_model_random_int <- glmer(place ~ ethnic + gender + age + I(age**2)  + 
                                  time + I(time**2) + (1 | originNBH), 
                                  data=migrations, family=binomial)
distant_model_random_int_2level <- glmer(place ~ ethnic + gender + age + 
                                         I(age**2) + time + I(time**2) +
                                         (1 | respid) + (1 | originNBH), 
                                         data=migrations, family=binomial)
distant_model_fixed <- glm(place ~ ethnic + gender + age + I(age**2) + time + 
                           I(time**2), data=migrations, family=binomial)

exp(fixef(distant_model_random_int))
exp(fixef(distant_model_random_int_2level))
exp(coef(distant_model_fixed))

write.matrix(results.distant, file="distant_model.csv", sep=",")
