###############################################################################
# Runs logistics regressions to predict the hazard of marriage from a set of 
# variables using lmer, and glm.
###############################################################################

require("lme4")
require("MASS")
require("survival")
require("ggplot2")

# Load the migration data (stored in a dataframe as person-months)
#load("marriage_marriages_censored.Rdata")
# Load the migration data (stored in a dataframe with censoring information)
load("marriage_times.Rdata")
load("marriage_events_censored.Rdata")

# First calculate summary statistics (at the individual level, so make a new 
# dataframe that only has one record per respid. Here we don't care about the 
# migr variable, or time-dependent variables, since we only want to know about 
# variables that were defined in 1996.
#unique_indiv <- marriages[!duplicated(marriages$respid),]
#print(summary(unique_indiv))

# Fit the local migration models
# Consider only local marriages (coded as 1)
print("Fitting the models...")
marriages$respid <- factor(marriages$respid)

marriages <- cbind(marriages, logagveg=log(marriages$perc_agveg*100))
marriages$logagveg[is.infinite(marriages$logagveg)] <- 0
marriages$logagveg[marriages$logagveg < 0] <- 0

#mixed_model <- with(marriages, glmer(marriage_outcome ~ gender +
#        ethnic + status_age + I(status_age**2) + (logagveg | nid),
#        family=binomial, na.action="na.omit"))
#results.mixed <- summary(mixed_model)$coefficients[,c(0,1,3)]
#results.mixed[,1] <- exp(results.mixed[,1])
#results.mixed <- round(results.mixed, digits=2)
#write.csv(results.mixed, file="mixed_model.csv")

events <- cbind(events, logagveg=log(events$perc_agveg*100))
events$logagveg[is.infinite(events$logagveg)] <- 0
events$logagveg[events$logagveg < 0] <- 0
events_model <- with(events, glm(marit ~ logagveg + gender +
        ethnic + age + I(age**2), family=binomial,
        na.action="na.omit"))
results.events <- summary(events_model)$coefficients[,c(0,1,3)]
results.events[,1] <- exp(results.events[,1])
results.events <- round(results.events, digits=2)
write.csv(results.events, file="events_model.csv")
# Plot some example predictions
exp_logagveg <- log(seq(0,100,10))
exp_logagveg[1] <- 0
exp_gender <- factor(rep(1,11), levels=c(0,1), labels=c("male", "female"))
exp_ethnic <- factor(rep(1,11), levels=c(1,2,3,4,5,6), labels=c("UpHindu",
        "HillTibeto", "LowHindu", "Newar", "TeraiTibeto", "Other"))
exp_status_age <- rep(20,11)
prediction_data <- data.frame(logagveg=exp_logagveg, gender=exp_gender, ethnic=exp_ethnic, status_age=exp_status_age)

qplot(exp_logagveg, predict(events_model, prediction_data))

###############################################################################
# Now do survival analysis models.
#
surv_object <- with(marriages, Surv(time=start_time, time2=end_time, event=marriage_outcome))
#km.res <- with(marriages, survfit(surv_object ~ logagveg + gender +
#        ethnic + status_age + I(status_age**2), type="kaplan-meier"))
#asfr<-km.res$n.event/km.res$n.risk
#plot(km.res$time, asfr, type=’s’)

simple_survival <- coxph(surv_object ~ logagveg + gender +
        ethnic + status_age + I(status_age**2), marriages)

# Plot some example predictions
exp_logagveg <- log(seq(0,100,10))
exp_logagveg[1] <- 0
exp_gender <- factor(rep(1,11), levels=c(0,1), labels=c("male", "female"))
exp_ethnic <- factor(rep(1,11), levels=c(1,2,3,4,5,6), labels=c("UpHindu",
        "HillTibeto", "LowHindu", "Newar", "TeraiTibeto", "Other"))
exp_status_age <- rep(20,11)
prediction_data <- data.frame(logagveg=exp_logagveg, gender=exp_gender, ethnic=exp_ethnic, status_age=exp_status_age)

qplot(exp_logagveg, predict(simple_survival, prediction_data))

###############################################################################
# Old version using the monthly data
# Convert time to represent seasons
#month <- marriages$time%%12
#marriages <- cbind(marriages, month)
#marriages$month <- factor(marriages$month)

#season <- month
#season[month<2] <- 0
#season[month>5] <- 0
#season[month!=0] <- 1
