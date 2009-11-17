###############################################################################
# Runs survival models to predict the hazard of migration from a set of 
# variables.
###############################################################################

library(survival)

load("migrations_allvars.Rdata")

time1 = migrations$time-1
time2 = migrations$time

outcome <- migrations$migr
outcome[outcome!=1] <- 0
survival_obj <- Surv(time1, time2, outcome)
local_cox <- with(migrations, coxph(survival_obj ~ cengendr +
    salyn + schl1996 + yrsschl + age1524 + age2534 + age3544 + bae2 + bae25
    + baa43 + bab4))

outcome <- migrations$migr
survival_obj <- Surv(time1, time2, outcome, type="interval")
local_survreg <- with(migrations, survreg(survival_obj ~ cengendr +
    salyn + schl1996 + yrsschl + age1524 + age2534 + age3544 + bae2 + bae25
    + baa43 + bab4))

outcome <- migrations$migr
outcome[outcome!=2] <- 0
outcome[outcome==2] <- 1
survival_obj <- Surv(time1, time2, outcome)
distant_cox <- with(migrations, coxph(survival_obj ~ cengendr +
    salyn + schl1996 + yrsschl + age1524 + age2534 + age3544 + bae2 + bae25
    + baa43 + bab4))

###########################################
# Climate models
climate_data <- read.csv("indicator_2.txt")
climate_data <- climate_data / 100
migrations <- merge(climate_data, migrations)

time1 = migrations$time-1
time2 = migrations$time

outcome <- migrations$migr
outcome[outcome!=2] <- 0
outcome[outcome==2] <- 1
survival_obj <- Surv(time1, time2, outcome)
distant_cox_climate <- with(migrations, coxph(survival_obj ~ cengendr +
    salyn + schl1996 + yrsschl + age1524 + age2534 + age3544 + bae2 + bae25
    + baa43 + bab4 + climate))

outcome <- migrations$migr
outcome[outcome!=1] <- 0
survival_obj <- Surv(time1, time2, outcome)
local_cox_climate <- with(migrations, coxph(survival_obj ~ cengendr +
    salyn + schl1996 + yrsschl + age1524 + age2534 + age3544 + bae2 + bae25
    + baa43 + bab4 + climate))

save(local_model, local_model_climate, distant_model, distant_model_climate, file="models_survival.Rdata")
