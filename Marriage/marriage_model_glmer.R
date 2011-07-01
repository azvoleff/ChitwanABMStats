###############################################################################
# Runs logistics regressions to predict the hazard of marriage from a set of 
# variables using lmer, and glm.
###############################################################################

require("lme4")
require("MASS")
require("ggplot2")

# Load the marriage data (stored in a dataframe as person-months)
#load("marriage_marriages_censored.Rdata")
load("marriage_times.Rdata")
# Load the marriage data (stored in a dataframe with censoring information)
load("marriage_events_censored.Rdata")

# Consider only local marriages (coded as 1)
print("Fitting the models...")
marriages$respid <- factor(marriages$respid)

marriages <- cbind(marriages, logagveg=log(marriages$perc_agveg*100))
marriages$logagveg[is.infinite(marriages$logagveg)] <- 0
marriages$logagveg[marriages$logagveg < 0] <- 0

mixed_model <- with(marriages, glmer(marriage_outcome ~ gender +
        ethnic + status_age + I(status_age**2) + (logagveg | nid),
        family=binomial, na.action="na.omit"))
results.mixed <- summary(mixed_model)$coefficients[,c(0,1,3)]
results.mixed[,1] <- exp(results.mixed[,1])
results.mixed <- round(results.mixed, digits=2)
write.csv(results.mixed, file="mixed_model.csv")
