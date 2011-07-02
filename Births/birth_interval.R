#!/usr/bin/env Rscript
###############################################################################
# Loads deaths and age at death from the ICPSR Restricted dataset DS0010, the 
# household registry data, and calculates several statistics (birth rates, 
# marriage rates, and mortality rates) for parameterizing the ChitwanABM model.
###############################################################################

# Hmisc is needed as hhreg is a "labelled" dataframe. If Hmisc is nto included, 
# will get errors saying "cannot coerce class "labelled" into a data.frame"
require(Hmisc, quietly=TRUE)
require(ggplot2, quietly=TRUE)

load("/media/Local_Secure/CVFS_R_format/hhreg.Rdata")
hhreg$gender <- factor(hhreg$gender, labels=c("male", "female"))

# Function to write out probabilities of events in the format required by the 
# ChitwanABM model.
make.txthazard <- function(probs, binlims, param.name) {
    # param.name is the name used by the ChitwanABM model for this parameter.
    txthazard <- paste("'", param.name, "' : [{", sep="")
    for (rownum in 1:length(probs)) {
        txthazard <- paste(txthazard, "(", binlims[rownum], ", ",
                binlims[rownum+1], "):", round(probs[rownum], digits=4),
                sep="")
        if (rownum<length(probs)) txthazard <- paste(txthazard, ", ", sep="")
    }
    txthazard <- paste(txthazard, "} | validate_hazard(", binlims[1], ", ",
            binlims[length(binlims)], ")]", sep="")
    return(txthazard)
}

make.txtprob <- function(probs, binlims, param.name) {
    # param.name is the name used by the ChitwanABM model for this parameter.
    binlims <- paste(round(binlims, digits=4), collapse=", ")
    probs <- paste(round(probs, digits=4), collapse=", ")
    txtprob <- paste("'", param.name, "' : [((", binlims, "), (", probs,  "))] | validate_prob_dist]", sep="")
    return(txtprob)
}

plot.hazard <- function(probs, plottitle, plotfile) {
    qplot(bin, prob, geom="line", xlab="Age (years)",
            ylab="Annual probability", main=plottitle, data=probs)
    ggsave(plotfile, width=8.33, height=5.53, dpi=300)
}

###############################################################################
# Birth Interval
###############################################################################

# Now find the index of the first birth of each row (only in the rows where 
# there was a marriage)
preg.cols <- grep('^preg[0-9]*$', names(hhreg))
births <- hhreg[hhreg$gender=="female", preg.cols]
births <- births==3 | births==5

remove_first_birth <- function(record) {
    firstbirth_column <- match(TRUE, record)
    record[firstbirth_column] <- FALSE
    return(record)
}

MAX_NUM_CHILDREN <- 15
birth_month <- matrix(NA, nrow(births), MAX_NUM_CHILDREN)
for (n in 1:MAX_NUM_CHILDREN) {
    # Find the month of the first birth for each row
    firstbirth.month <- apply(births, 1, function(x) match(TRUE, x))
    # Set each of these "first" births (they are only REAL first births on the 
    # first go through the loop, and only first births recorded in the survey, 
    # not first births after marriage as here we consider all married women 
    # regardless of duration) to FALSE so we can consider the next birth on the 
    # next run through the loop.
    births <- apply(births, 1, remove_first_birth)
    birth_month[,n] <- firstbirth.month
}

birth_interval <- birth_month[,(2:MAX_NUM_CHILDREN)] - birth_month[,1:(MAX_NUM_CHILDREN-1)]

txthazards <- c(txthazards,
        make.txthazard(marrprob[marrprob$gender=="female",]$prob,
        marrlims, "hazard.marriage.female"))
write(txthazards, file="hazards.txt")

theme_update(theme_grey(base_size=18))
update_geom_defaults("line", aes(size=1))
update_geom_defaults("step", aes(size=1))

qplot(bin, prob*100, geom="step", xlab="Age (years)",
        ylab="Annual Probability of Live Birth (%)",
        data=birthprob)
ggsave("prob_birth.png", width=8.33, height=5.53, dpi=300)
