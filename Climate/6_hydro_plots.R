library(lubridate)
library(ggplot2)
library(gridExtra)
library(plyr)
library(hydroTSM) # for 'fdc'

source('0_utility_functions.R')

stdErr <- function(x) {sd(x, na.rm=TRUE)/ sqrt(length(x[!is.na(x)]))}

load('discharge_daily.Rdata')

###############################################################################
# Percentiles
valid_rows <- which(discharge$discharge > 0 & !is.na(discharge$discharge > 0))
percentiles <- quantile(discharge$discharge[valid_rows], prob=c(c(25, 50, 90, 95, 99)/100), na.rm=T)

gt_90 <- ddply(discharge, .(Year), summarize,
               num_days=sum(discharge > percentiles['90%'], na.rm=TRUE))
labeldata <- eqnfunc_slope(gt_90, 'num_days ~ order(Year)')
discharge_gt_90_plot <- ggplot(gt_90, aes(Year, num_days)) +
    geom_line() + xlab('Year') + facet_grid(~ . Station) +
    ylab('Days discharge above 90th percentile') +
    geom_smooth(method="lm", se=TRUE, color="black") +
    geom_text(aes(x=1965, y=60, label=labeldata), parse=TRUE, clour='black', 
              hjust=0, size=8)
png('discharge_gt_90.png ', width=PLOT_WIDTH*PLOT_DPI, height=PLOT_HEIGHT*PLOT_DPI)
print(discharge_gt_90_plot )
dev.off()

gt_95 <- ddply(discharge, .(Year), summarize,
               num_days=sum(discharge > percentiles['95%'], na.rm=TRUE))
labeldata <- eqnfunc_slope(gt_95, 'num_days ~ order(Year)')
discharge_gt_95_plot <-ggplot(gt_95, aes(Year, num_days)) +
    geom_line() + xlab('Year') +
    ylab('Days discharge above 95th percentile') +
    geom_smooth(method="lm", se=TRUE, color="black") +
    geom_text(aes(x=1965, y=40, label=labeldata), parse=TRUE, clour='black', 
              hjust=0, size=8)
png('discharge_gt_95.png ', width=PLOT_WIDTH*PLOT_DPI, height=PLOT_HEIGHT*PLOT_DPI)
print(discharge_gt_95_plot )
dev.off()

gt_99 <- ddply(discharge, .(Year), summarize,
               num_days=sum(discharge > percentiles['99%'], na.rm=TRUE))
labeldata <- eqnfunc_slope(gt_99, 'num_days ~ order(Year)')
discharge_gt_99_plot <- ggplot(gt_99, aes(Year, num_days)) +
    geom_line() + xlab('Year') +
    ylab('Days discharge above 99th percentile') +
    geom_smooth(method="lm", se=TRUE, color="black") +
    geom_text(aes(x=1965, y=14, label=labeldata), parse=TRUE, clour='black', 
              hjust=0, size=8)
png('discharge_gt_99.png ', width=PLOT_WIDTH*PLOT_DPI, height=PLOT_HEIGHT*PLOT_DPI)
print(discharge_gt_99_plot )
dev.off()

###############################################################################
# Annual discharge
ann <- ddply(discharge, .(Year), summarize, total=sum(discharge, na.rm=TRUE))
labeldata <- eqnfunc_slope(ann, 'total ~ order(Year)')
discharge_annual_plot <- ggplot(ann, aes(Year, total)) +
    geom_line() + xlab('Year') +
    ylab(expression(paste('Total annual discharge (m'^3, ')', sep=''))) +
    geom_smooth(method="lm", se=TRUE, color="black") +
    geom_text(aes(x=1968, y=695000, label=labeldata), parse=TRUE, clour='black', 
              hjust=0, size=8)
png('discharge_annual.png', width=PLOT_WIDTH*PLOT_DPI, height=PLOT_HEIGHT*PLOT_DPI)
print(discharge_annual_plot)
dev.off()

###############################################################################
# Flow discharge curve
png('discharge_fdc_curve.png', width=PLOT_WIDTH*PLOT_DPI, 
    height=PLOT_HEIGHT*PLOT_DPI)
# For par: bottom, left, top, right
par(mar=c(3, 3, .75, .5), cex=3)
res <- fdc(discharge$discharge, cex=.5, cex.lab=.75, cex.axis=.75, main='', mgp=c(2, 0, 0))
dev.off()

###############################################################################
# Mmax,i and Mmin,i
#
# From:
#   Diadovski, Ivan K. “Risk Assessment of Extreme Events Along a River Flow.” 
#   Journal of Water Resource and Protection 02, no. 05 (2010): 455–461.  
#   doi:10.4236/jwarp.2010.25052.
qextrema <- ddply(discharge, .(Year), summarize,
                 qmin=min(discharge, na.rm=TRUE),
                 qmax=max(discharge, na.rm=TRUE))
qmin_mean <- mean(qextrema$qmin)
qextrema$qmin_anom <- qextrema$qmin - qmin_mean
qextrema$mmin <- qextrema$qmin / qmin_mean
qmax_mean <- mean(qextrema$qmax)
qextrema$qmax_anom <- qextrema$qmax - qmax_mean
qextrema$mmax <- qextrema$qmax / qmax_mean

labeldata <- eqnfunc_slope(qextrema, 'mmin ~ order(Year)')
discharge_mmin_plot <- ggplot(qextrema, aes(Year, mmin)) +
    geom_line() + xlab('Year') +
    ylab(expression(paste('M'[min], sep=''))) +
    geom_smooth(method="lm", se=TRUE, color="black") + ylim(c(.5, 1.75)) +
    geom_text(aes(x=1965, y=1.5, label=labeldata), parse=TRUE, clour='black', 
              hjust=0, size=8)
png('discharge_mmin.png', width=PLOT_WIDTH*PLOT_DPI, height=PLOT_HEIGHT*PLOT_DPI)
print(discharge_mmin_plot)
dev.off()

labeldata <- eqnfunc_slope(qextrema, 'mmax ~ order(Year)')
discharge_mmax_plot <- ggplot(qextrema, aes(Year, mmax)) +
    geom_line() + xlab('Year') +
    ylab(expression(paste('M'[max], sep=''))) +
    geom_smooth(method="lm", se=TRUE, color="black") + ylim(c(.5, 1.75)) +
    geom_text(aes(x=1980, y=1.5, label=labeldata), parse=TRUE, clour='black', 
              hjust=0, size=8)
png('discharge_mmax.png', width=PLOT_WIDTH*PLOT_DPI, height=PLOT_HEIGHT*PLOT_DPI)
print(discharge_mmax_plot)
dev.off()

###############################################################################
# TODO: Also look at proportion of flow by season

###############################################################################
# Seasonal discharge plot
seasonal_q <- ddply(discharge, .(Year, Season), summarize,
                    mean_q=mean(discharge, na.rm=TRUE),
                    max_q=max(discharge, na.rm=TRUE),
                    min_q=min(discharge, na.rm=TRUE))

ddply(seasonal_q, .(Season), summarize,
      series_mean_mean=mean(mean_q, na.rm=T),
      series_mean_min=mean(min_q, na.rm=T),
      series_mean_max=mean(max_q, na.rm=T))


labeldata <- ddply(seasonal_q, .(Season), eqnfunc_slope, 'mean_q ~ order(Year)')
labeldata
seasonal_mean_q_plot <- ggplot(seasonal_q, aes(Year, mean_q)) +
    geom_line() + xlab('Year') + 
    facet_grid(Season ~ . , scales='free_y') +
    ylab(expression(paste('Seasonal mean Q (m'^3, '/s)', sep=''))) +
    geom_smooth(method="lm", se=TRUE, color="black") +
    theme(strip.text.y=theme_text(size=27))
png('discharge_seasonal_mean_q.png', width=PLOT_WIDTH*PLOT_DPI, height=PLOT_HEIGHT*PLOT_DPI)
print(seasonal_mean_q_plot)
dev.off()

labeldata <- ddply(seasonal_q, .(Season), eqnfunc_slope, 'max_q ~ order(Year)')
labeldata
seasonal_max_q_plot <- ggplot(seasonal_q, aes(Year, max_q)) +
    geom_line() + xlab('Year') + 
    facet_grid(Season ~ . , scales='free_y') +
    ylab(expression(paste('Seasonal max. Q (m'^3, '/s)', sep=''))) +
    geom_smooth(method="lm", se=TRUE, color="black") +
    theme(strip.text.y=theme_text(size=27))
png('discharge_seasonal_max_q.png', width=PLOT_WIDTH*PLOT_DPI, height=PLOT_HEIGHT*PLOT_DPI)
print(seasonal_max_q_plot)
dev.off()

labeldata <- ddply(seasonal_q, .(Season), eqnfunc_slope, 'min_q ~ order(Year)')
labeldata
seasonal_min_q_plot <- ggplot(seasonal_q, aes(Year, min_q)) +
    geom_line() + xlab('Year') + 
    facet_grid(Season ~ . , scales='free_y') +
    ylab(expression(paste('Seasonal min. Q (m'^3, '/s)', sep=''))) +
    geom_smooth(method="lm", se=TRUE, color="black") +
    theme(strip.text.y=theme_text(size=27))
png('discharge_seasonal_min_q.png', width=PLOT_WIDTH*PLOT_DPI, height=PLOT_HEIGHT*PLOT_DPI)
print(seasonal_min_q_plot)
dev.off()

###############################################################################
# Save multiplot
grid_cols <- 2
grid_rows <- 2
png('hydro_multiplot.png', width=PLOT_WIDTH*PLOT_DPI*grid_cols, 
    height=PLOT_HEIGHT*PLOT_DPI*grid_rows)
grid.arrange(ann_plot, pct_95_plot, rainy_days_plot, EP_frac_plot, max_5_day_plot, 
             monthly_precip_plot, ncol=2)
dev.off()
