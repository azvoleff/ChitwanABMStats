#!/usr/bin/Rscript
require(ggplot2)

theme_update(theme_grey(base_size=14))

make_txtprob <- function(probs, binlims, param.name) {
    # param.name is the name used by the ChitwanABM model for this parameter.
    binlims <- paste(round(binlims, digits=4), collapse=", ")
    probs <- paste(round(probs, digits=4), collapse=", ")
    txtprob <- paste("'", param.name, "' : [((", binlims, "), (", probs,  "))] | validate_prob_dist]", sep="")
    return(txtprob)
}

hh_areas <- read.csv("hh_areas_T1_sq_meters.csv")
hh_areas <- hh_areas[hh_areas$NEIGHID <= 151,]

qplot(hh_areas$area, geom="histogram",
        xlab="Area of Household Plot (square meters)", ylab="Count")
ggsave("hh_areas_T1_hist.png", width=8.33, height=5.53, dpi=300)

hh_areas <- hh_areas[hh_areas$area <= 1000, , drop=FALSE]

qplot(hh_areas$area, geom="histogram", xlab="Area of Household Plot (square meters)",
      ylab="Count")
ggsave("hh_areas_T1_hist_cut.png", width=8.33, height=5.53, dpi=300)

hh_areas_lims <- seq(0, 1000, 100)
hh_areas_binned <- cut(hh_areas$area, hh_areas_lims)
hh_areas_prob <- data.frame(prob=table(hh_areas_binned))
names(hh_areas_prob) <- c('bin', 'prob')
hh_areas_prob$prob <- hh_areas_prob$prob / sum(hh_areas_prob$prob)

qplot(bin, prob*100, geom="bar", xlab="Area of Household Plot (square meters)",
        ylab="Probability (%)", data=hh_areas_prob)
ggsave("hh_areas_T1_hist_prob.png", width=8.33, height=5.53, dpi=300)

write(make_txtprob(hh_areas_prob$prob, c(0, hh_areas_prob$lim_upper), "lulc.area.hh"), file="lulc_area_hh_T1.txt")

print(paste("Mean:", mean(hh_areas$area)))
print(paste("SD:", sd(hh_areas$area)))
print(paste("n:", length(hh_areas$area)))
