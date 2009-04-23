###############################################################################
# Runs survival models to predict the hazard of migration from a set of 
# variables.
###############################################################################

load("migrations_allvars.Rdata")

time1 = migrations$TIME-1
time2 = migrations$TIME

outcome <- migrations$MIGR
outcome[outcome!=1] <- 0
survival_obj <- Surv(time1, time2, outcome)
local_cox <- with(migrations, coxph(survival_obj ~ CENGENDR +
    SALYN + SCHL1996 + YRSSCHL + AGE1524 + AGE2534 + AGE3544 + BAE2 + BAE25
    + BAA43 + BAB4))

outcome <- migrations$MIGR
survival_obj <- Surv(time1, time2, outcome, type="interval")
local_survreg <- with(migrations, survreg(survival_obj ~ CENGENDR +
    SALYN + SCHL1996 + YRSSCHL + AGE1524 + AGE2534 + AGE3544 + BAE2 + BAE25
    + BAA43 + BAB4))

outcome <- migrations$MIGR
outcome[outcome!=2] <- 0
outcome[outcome==2] <- 1
survival_obj <- Surv(time1, time2, outcome)
distant_cox <- with(migrations, coxph(survival_obj ~ CENGENDR +
    SALYN + SCHL1996 + YRSSCHL + AGE1524 + AGE2534 + AGE3544 + BAE2 + BAE25
    + BAA43 + BAB4))

###########################################
# Climate models
climate_data <- read.csv("indicator_2.txt")
climate_data <- climate_data / 100
migrations <- merge(climate_data, migrations)

time1 = migrations$TIME-1
time2 = migrations$TIME

outcome <- migrations$MIGR
outcome[outcome!=2] <- 0
outcome[outcome==2] <- 1
survival_obj <- Surv(time1, time2, outcome)
distant_cox_climate <- with(migrations, coxph(survival_obj ~ CENGENDR +
    SALYN + SCHL1996 + YRSSCHL + AGE1524 + AGE2534 + AGE3544 + BAE2 + BAE25
    + BAA43 + BAB4 + CLIMATE))

outcome <- migrations$MIGR
outcome[outcome!=1] <- 0
survival_obj <- Surv(time1, time2, outcome)
local_cox_climate <- with(migrations, coxph(survival_obj ~ CENGENDR +
    SALYN + SCHL1996 + YRSSCHL + AGE1524 + AGE2534 + AGE3544 + BAE2 + BAE25
    + BAA43 + BAB4 + CLIMATE))

save(local_model, local_model_climate, distant_model, distant_model_climate, file="models_survival.Rdata")
