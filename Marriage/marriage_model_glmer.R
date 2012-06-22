###############################################################################
# Runs logistics regressions to predict the hazard of marriage from a set of 
# variables using lmer, and glm.
###############################################################################

require("lme4")
require("Hmisc")
require("MASS")
require("ggplot2")
require("ggplot2")
require("arm") # for se.coef, se.fixef

# Load the marriage data (stored in a dataframe with censoring information)
load("marriage_events_censored.Rdata")
load("nbh_data_dirgha.Rdata")

events <- merge(events, nbh_data)

events$respid <- factor(events$respid)
events$nid <- factor(events$nid)
events$marit <- factor(events$marit)

events <- cbind(events, logagveg=log(events$perc_agveg*100))
events$logagveg[is.infinite(events$logagveg)] <- 0
events$logagveg[events$logagveg < 0] <- 0
events <- cbind(events, month=as.factor(events$time%%12))

mixed_model <- with(events, glmer(marit ~ gender + ethnic + age + I(age**2) +
        logagveg + schlft52 + hlthft52 + busft52 + marft52 + empft52 + month + (1 | nid),
        nAGQ=100, family=binomial, na.action="na.omit"))
results.mixed <- data.frame(coef=fixef(mixed_model), odds_ratios=exp(fixef(mixed_model)),
        se=se.fixef(mixed_model))
results.mixed <- round(results.mixed, 2)

write.csv(results.mixed, file="mixed_model_20110805.csv")
