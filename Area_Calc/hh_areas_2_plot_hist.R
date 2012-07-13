#!/usr/bin/Rscript
# Plots the LULC data from a model run.
require(ggplot2)
require(MASS)

theme_update(theme_grey(base_size=18))
update_geom_defaults("line", aes(size=1))
update_geom_defaults("step", aes(size=1))

make_txtprob <- function(probs, binlims, param.name) {
    # param.name is the name used by the ChitwanABM model for this parameter.
    binlims <- paste(round(binlims, digits=4), collapse=", ")
    probs <- paste(round(probs, digits=4), collapse=", ")
    txtprob <- paste("'", param.name, "' : [((", binlims, "), (", probs,  "))] | validate_prob_dist]", sep="")
    return(txtprob)
}

hh_areas <- read.csv("hh_areas_T1_sq_meters.csv")

qplot(hh_areas$area, geom="histogram",
        xlab="Area of Household Plot (square meters)", ylab="Count")
ggsave("hh_areas_T1_hist.png", width=8.33, height=5.53, dpi=300)

hh_areas <- hh_areas[hh_areas$area <= 1000, , drop=FALSE]

qplot(hh_areas$area, geom="histogram", xlab="Area of Household Plot (square meters)",
      ylab="Count")
ggsave("hh_areas_T1_hist_cut.png", width=8.33, height=5.53, dpi=300)

hh_areas_binned <- hist(hh_areas$area, plot=FALSE)
lim_upper <- hh_areas_binned$breaks[2:length(hh_areas_binned$breaks)]
hh_areas_prob <- data.frame(lim_upper=lim_upper,
        prob=hh_areas_binned$counts)
hh_areas_prob$prob <- hh_areas_prob$prob/sum(hh_areas_prob$prob)

# And plot a histogram from this distribution
qplot(lim_upper, prob*100, geom="step", xlab="Area of Household Plot (square meters)",
        ylab="Probability (%)", data=hh_areas_prob)
ggsave("hh_areas_T1_hist_prob.png", width=8.33, height=5.53, dpi=300)

write(make_txtprob(hh_areas_prob$prob, c(0, hh_areas_prob$lim_upper), "lulc.area.hh"), file="lulc_area_hh_T1.txt")

print(paste("Mean:", mean(hh_areas$area)))
print(paste("SD:", sd(hh_areas$area)))
print(paste("n:", length(hh_areas$area)))
