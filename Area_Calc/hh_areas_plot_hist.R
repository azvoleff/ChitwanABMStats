#!/usr/bin/Rscript
# Plots the LULC data from a model run.
require(ggplot2)
require(MASS)

theme_update(theme_grey(base_size=18))
update_geom_defaults("line", aes(size=1))
update_geom_defaults("step", aes(size=1))

make.txtprob <- function(probs, binlims, param.name) {
    # param.name is the name used by the ChitwanABM model for this parameter.
    binlims <- paste(round(binlims, digits=4), collapse=", ")
    probs <- paste(round(probs, digits=4), collapse=", ")
    txtprob <- paste("'", param.name, "' : [((", binlims, "), (", probs,  "))] | validate_prob_dist]", sep="")
    return(txtprob)
}

hh.areas <- read.csv("hh_areas_T1_sq_meters.csv")

qplot(hh.areas$area, geom="histogram",
        xlab="Household Area (square meters)", ylab="Count")
ggsave("hh.areas_hist.png", width=8.33, height=5.53, dpi=300)

qplot(hh.areas$area, geom="histogram", xlim=c(0,10000),
        xlab="Household Area (square meters)", ylab="Count", binwidth=300)
ggsave("hh.areas_hist_cut.png", width=8.33, height=5.53, dpi=300)

hh.areas.binned <- bin(hh.areas$area, binwidth=300)
lim.upper <- ceiling(hh.areas.binned$x[2:26])
lim.upper[25] <- 10000
hh.areas.prob <- data.frame(lim.upper=lim.upper,
        prob=hh.areas.binned$count[2:26])
hh.areas.prob <- hh.areas.prob[-c(13, 14),]
hh.areas.prob <- hh.areas.prob[-c(16:22),]
hh.areas.prob$prob <- hh.areas.prob$prob/sum(hh.areas.prob$prob)

# And plot a histogram from this distribution
qplot(lim.upper, prob*100, geom="step", xlab="Household Area (square meters)",
        ylab="Probability (%)", data=hh.areas.prob)
ggsave("hh.areas_hist_prob.png", width=8.33, height=5.53, dpi=300)

write(make.txtprob(hh.areas.prob$prob, c(hh.areas.prob$lim.upper, 12000), "lulc.area.hh"), file="lulc.area.hh.txt")

print(paste("Mean:", mean(hh.areas$area)))
print(paste("SD:", sd(hh.areas$area)))
print(paste("n:", length(hh.areas$area)))
