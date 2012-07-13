#!/usr/bin/Rscript
# Plots the LULC data from a model run.
require(ggplot2)
require(MASS)

theme_update(theme_grey(base_size=18))
update_geom_defaults("line", aes(size=1))
update_geom_defaults("step", aes(size=1))

nfo.areas <- read.csv("nfo_areas_T1_sq_meters.csv")
nfo.areas <- nfo.areas[-which(nfo.areas$area>20000),]
nfo.areas <- nfo.areas[-which(nfo.areas$area<10),]

qplot(nfo.areas$area, geom="histogram", xlim=c(0, 10000),
        xlab="NFO Area (square meters)", ylab="Count")
ggsave("nfo_areas_hist_cut.png", width=8.33, height=5.53, dpi=300)

qplot(area, facets=nfo_type~., geom="histogram", xlim=c(0, 10000),
        xlab="NFO Area (square meters)", ylab="Count", data=nfo.areas)
ggsave("nfo_areas_hist_type.png", width=8.33, height=5.53, dpi=300)

binlims <- c(10, 250, 2000, 10000)
prob <- c(1, 2/15, 1/15)
prob <- prob/sum(prob)
# The below line is necessary for plotting
prob <- c(prob, prob[3])
# And plot a histogram from this distribution
qplot(binlims, prob*100, geom="step", xlab="NFO Area (square meters)",
        ylab="Probability")
ggsave("nfo_areas_hist_prob.png", width=8.33, height=5.53, dpi=300)

print(paste("Mean:", mean(nfo.areas$area)))
print(paste("SD:", sd(nfo.areas$area)))
print(paste("n:", length(nfo.areas$area)))
