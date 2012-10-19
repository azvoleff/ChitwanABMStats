#!/usr/bin/env Rscript
###############################################################################
# Calculates the birth interval probability distribution for parameterizing the 
# ChitwanABM model.
###############################################################################

# Hmisc is needed as hhreg is a "labelled" dataframe. If Hmisc is nto included, 
# will get errors saying "cannot coerce class "labelled" into a data.frame"
require(Hmisc, quietly=TRUE)
require(ggplot2, quietly=TRUE)

# LAST_MONTH is how many months of the household registry to include (max 
# number of months is 126, so to include all the months set LAST_MONTH to 126).  
# Here LAST_MONTH is set to 60 to leave an independent set of data for model 
# verification and validation - only the first 60 months of the household 
# registry data is used here for parameterization.
LAST_MONTH <- 60

load("V:/Nepal/CVFS_HHReg/hhreg126.Rdata")
# Drop the appropriate monthly columns if LAST_MONTH is < 126
varying_cols <- grep('^[a-zA-Z]*[1-9][0-9]{0,2}$', names(hhreg))
varying_cols_times <- as.numeric(gsub('[a-zA-Z]', '', names(hhreg)[varying_cols]))
if (LAST_MONTH < max(varying_cols_times)) {
    drop_cols <- varying_cols[varying_cols_times > LAST_MONTH]
    hhreg <- hhreg[-drop_cols]
}

hhreg$gender <- factor(hhreg$gender, labels=c("male", "female"))

make_txt_prob_dist <- function(probs, binlims, param.name) {
    # param.name is the name used by the ChitwanABM model for this parameter.
    binlims <- paste(round(binlims, digits=6), collapse=", ")
    probs <- paste(round(probs, digits=6), collapse=", ")
    txtprob <- paste("'", param.name, "' : [((", binlims, "), (", probs,  "))] | validate_prob_dist]", sep="")
    return(txtprob)
}

# Function to write out probabilities of events in the format required by the 
# ChitwanABM model.
###############################################################################
# Birth Interval
###############################################################################

# Now find the index of the first birth of each row (only in the rows where 
# there was a marriage)
preg_cols <- grep('^preg[0-9]*$', names(hhreg))
births <- hhreg[hhreg$gender=="female", preg_cols]
live_birth <- births==3 | births==5

remove_first_birth <- function(birthcol, preg_row) {
    if (is.na(birthcol)) return(preg_row)
    preg_row[birthcol] <- FALSE
    return(preg_row)
}
# Adjust the MAX_NUM_CHILDREN variable as needed depending on the number of 
# months of household registry data that are included. It should be equal to 
# the maximum number of children a woman could possibly have in the period of 
# time covered by the dataset.
MAX_NUM_CHILDREN <- ceil(ncol(live_birth)/9)
birth_month <- matrix(NA, nrow(live_birth), MAX_NUM_CHILDREN)
for (n in 1:MAX_NUM_CHILDREN) {
    # Find the month of the first birth for each row
    firstbirth_month <- apply(live_birth, 1, function(x) match(TRUE, x))
    birth_month[,n] <- firstbirth_month
    # Set each of these "first" births to FALSE so we can consider the next 
    # birth on the next run through the loop (they are only REAL first births 
    # on the first go through the loop, and only first births recorded in the 
    # survey, not first births after marriage as here we consider all married 
    # women regardless of duration).
    live_birth <- t(mapply(remove_first_birth, firstbirth_month, as.list(as.data.frame(t(live_birth)))))
}

birth_int <- birth_month[,(2:ncol(birth_month))] - birth_month[,1:(ncol(birth_month)-1)]
birth_int <- data.frame(respid=hhreg[hhreg$gender=="female", ]$respid, birth_int)
birth_int_col_names <- paste('birth_int', seq(1:(MAX_NUM_CHILDREN-1)), sep="")
names(birth_int)[2:ncol(birth_int)] <- birth_int_col_names
birth_int_long <- reshape(birth_int, direction="long", idvar="respid", varying=birth_int_col_names, timevar="time", sep="")
birth_int_long <- birth_int_long[!is.na(birth_int_long$birth_int), ]

birth_int_lims <- c(9, 12, 18, 24, 30, 36, 42, 48, 54)
birth_int_bin <- cut(birth_int_long$birth_int, birth_int_lims)
birth_int_prob <- data.frame(prob=table(birth_int_bin))
names(birth_int_prob) <- c('bin', 'prob')
birth_int_prob$prob <- birth_int_prob$prob / sum(birth_int_prob$prob)

mean(birth_int_long$birth_int)
sd(birth_int_long$birth_int)

txtprobs <- make_txt_prob_dist(birth_int_prob$prob, birth_int_lims, 
                               "probability.birth_interval")
write.csv(birth_int_prob, file="birth_int_probs.csv", row.names=FALSE)
write(txtprobs, file="birth_int_probs.txt")

# Set a color blind compatible palette
theme_update(theme_grey(base_size=16))
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
qplot(bin, prob*100, geom="bar", xlab="Birth Interval (months)",
        ylab="Probability (%)", data=birth_int_prob)
ggsave("prob_birth_interval.png", width=8.33, height=5.53, dpi=300)
