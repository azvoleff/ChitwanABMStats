load("time_outside-1_months_away.Rdata")

time_outside <- time_outside[!is.na(time_outside)]

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

plot.hazard <- function(probs, plottitle, plotfile) {
    qplot(bin, prob, geom="line", xlab="Age (years)",
            ylab="Annual probability", data=probs)
    ggsave(plotfile, width=8.33, height=5.53, dpi=300)
}


lims <- c(1, 2, 4, 8, 12, 18, 24, 30, 36)
count <- c()
for (limindex in 1:(length(lims)-1)) {
    count <- c(count, sum((time_outside >= lims[limindex]) &
            (time_outside < lims[limindex+1])))
}

txtprobs <- make.txthazard(count/sum(count), lims, 'prob.migration.lengths')

miglengthprob <- data.frame(bin=lims, prob=c(count/sum(count),0))
miglengthprob$prob[nrow(miglengthprob)] <- miglengthprob$prob[nrow(miglengthprob)-1]
qplot(bin, prob, geom="step", xlab="Migration Length (months)",
        ylab="Probability of Migration Length (%)",
        data=miglengthprob)
ggsave("prob.migration.lengths.png", width=8.33, height=5.53, dpi=300)
