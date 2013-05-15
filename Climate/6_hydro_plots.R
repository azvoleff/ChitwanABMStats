library(lubridate)
library(ggplot2)
library(gridExtra)
library(plyr)
library(hydroTSM) # for 'fdc'

source('0_utility_functions.R')

stdErr <- function(x) {sd(x, na.rm=TRUE)/ sqrt(length(x[!is.na(x)]))}

load('discharge_daily.Rdata')

# Summarize how much data is available for each station
avail_data <- ddply(discharge, .(Station), summarize, 
                    start_year=min(Year),
                    end_year=max(Year),
                    num_days=sum(!is.na(discharge)),
                    num_years=length(unique(Year)))
avail_data

###############################################################################
# Percentiles
gt_90 <- ddply(discharge, .(Station, Year), summarize, 
               num_days=sum(discharge_gt_90, na.rm=TRUE))
labeldata <- ddply(gt_90, .(Station), eqnfunc_slope, 'num_days ~ order(Year)')
discharge_gt_90_plot <- ggplot(gt_90, aes(Year, num_days)) +
    geom_line() + xlab('Year') + facet_grid(Station ~ .) +
    ylab('Days discharge above 90th percentile') +
    geom_smooth(method="lm", se=TRUE, color="black") +
    geom_text(data=labeldata, aes(x=1965, y=85, label=eqn), parse=TRUE, 
              colour='black', hjust=0, size=8)
png('discharge_gt_90.png ', width=PLOT_WIDTH*PLOT_DPI, height=PLOT_HEIGHT*PLOT_DPI)
print(discharge_gt_90_plot )
dev.off()

gt_95 <- ddply(discharge, .(Station, Year), summarize, 
               num_days=sum(discharge_gt_95, na.rm=TRUE))
labeldata <- ddply(gt_95, .(Station), eqnfunc_slope, 'num_days ~ order(Year)')
discharge_gt_95_plot <-ggplot(gt_95, aes(Year, num_days)) +
    geom_line() + xlab('Year') + facet_grid(Station ~ .) +
    ylab('Days discharge above 95th percentile') +
    geom_smooth(method="lm", se=TRUE, color="black") +
    geom_text(data=labeldata, aes(x=1965, y=85, label=eqn), parse=TRUE, 
              colour='black', hjust=0, size=8)
png('discharge_gt_95.png ', width=PLOT_WIDTH*PLOT_DPI, height=PLOT_HEIGHT*PLOT_DPI)
print(discharge_gt_95_plot )
dev.off()

gt_99 <- ddply(discharge, .(Station, Year), summarize, 
               num_days=sum(discharge_gt_99, na.rm=TRUE))
labeldata <- ddply(gt_99, .(Station), eqnfunc_slope, 'num_days ~ order(Year)')
discharge_gt_99_plot <- ggplot(gt_99, aes(Year, num_days)) +
    geom_line() + xlab('Year') + facet_grid(Station ~ .) +
    ylab('Days discharge above 99th percentile') +
    geom_smooth(method="lm", se=TRUE, color="black") +
    geom_text(data=labeldata, aes(x=1965, y=60, label=eqn), parse=TRUE, 
              colour='black', hjust=0, size=8)
png('discharge_gt_99.png ', width=PLOT_WIDTH*PLOT_DPI, height=PLOT_HEIGHT*PLOT_DPI)
print(discharge_gt_99_plot )
dev.off()

###############################################################################
# Annual discharge
ann <- ddply(discharge, .(Station, Year), summarize, total=sum(discharge, na.rm=TRUE))
labeldata <- ddply(ann, .(Station), eqnfunc_slope, 'total ~ order(Year)')
discharge_annual_plot <- ggplot(ann, aes(Year, total)) +
    geom_line() + xlab('Year') + facet_grid(Station ~ ., scales='free_y') +
    ylab(expression(paste('Total annual discharge (m'^3, ')', sep=''))) +
    geom_smooth(method="lm", se=TRUE, color="black")
    #geom_text(data=labeldata, aes(x=1968, y=700000, label=eqn), parse=TRUE, 
    #          colour='black', hjust=0, size=8)
png('discharge_annual.png', width=PLOT_WIDTH*PLOT_DPI, height=PLOT_HEIGHT*PLOT_DPI)
print(discharge_annual_plot)
dev.off()

###############################################################################
# Flow discharge curve
discharge_wide <- data.frame(Date=seq(as.Date('1965/01/01'),
                                      as.Date('2004/12/31'), by='day'))
for (Station in levels(discharge$Station)) {
    this_station <- discharge[discharge$Station == Station, ]
    station_data <- 
    discharge_wide <- cbind(discharge_wide, 
                            new_col=this_station$discharge[match(discharge_wide$Date, 
                                                                 this_station$Date)])
    names(discharge_wide)[names(discharge_wide) == 'new_col'] <- Station
}
discharge_wide <- discharge_wide[-(names(discharge_wide) == 'Date')]

png('discharge_fdc_curve.png', width=PLOT_WIDTH*PLOT_DPI, 
    height=PLOT_HEIGHT*PLOT_DPI)
# For par: bottom, left, top, right
par(mar=c(3, 3, .75, .5), cex=3)
res <- fdc(discharge_wide, cex=.25, cex.lab=.75, cex.axis=.75, main='', mgp=c(2, 0, 0))
dev.off()

###############################################################################
# Mmax,i and Mmin,i
#
# From:
#   Diadovski, Ivan K. “Risk Assessment of Extreme Events Along a River Flow.” 
#   Journal of Water Resource and Protection 02, no. 05 (2010): 455–461.  
#   doi:10.4236/jwarp.2010.25052.
qextrema <- ddply(discharge, .(Station, Year), summarize,
                  qmin=min(discharge, na.rm=TRUE),
                  qmax=max(discharge, na.rm=TRUE))
qextrema <- ddply(qextrema, .(Station), transform,
                  qmin_anom=(qmin - mean(qmin)),
                  qmax_anom=(qmax - mean(qmax)),
                  mmin=(qmin / mean(qmin)),
                  mmax=(qmax / mean(qmax)))

labeldata <- ddply(qextrema, .(Station), eqnfunc_slope, 'mmin ~ order(Year)')
discharge_mmin_plot <- ggplot(qextrema, aes(Year, mmin)) +
    geom_line() + xlab('Year') + facet_grid(Station ~ .) +
    ylab(expression(paste('M'[min], sep=''))) +
    geom_smooth(method="lm", se=TRUE, color="black") + 
    geom_text(data=labeldata, aes(x=1965, y=1.5, label=eqn), parse=TRUE, 
              colour='black', hjust=0, size=8)
png('discharge_mmin.png', width=PLOT_WIDTH*PLOT_DPI, height=PLOT_HEIGHT*PLOT_DPI)
print(discharge_mmin_plot)
dev.off()

labeldata <- ddply(qextrema, .(Station), eqnfunc_slope, 'mmax ~ order(Year)')
discharge_mmax_plot <- ggplot(qextrema, aes(Year, mmax)) +
    geom_line() + xlab('Year') + facet_grid(Station ~ .) +
    ylab(expression(paste('M'[max], sep=''))) +
    geom_smooth(method="lm", se=TRUE, color="black") + 
    geom_text(data=labeldata, aes(x=1965, y=2.5, label=eqn), parse=TRUE, 
              colour='black', hjust=0, size=8)
png('discharge_mmax.png', width=PLOT_WIDTH*PLOT_DPI, height=PLOT_HEIGHT*PLOT_DPI)
print(discharge_mmax_plot)
dev.off()

###############################################################################
# TODO: Also look at proportion of flow by season

###############################################################################
# Seasonal discharge plot
seasonal_q <- ddply(discharge, .(Station, Year, Season), summarize,
                    mean_q=mean(discharge, na.rm=TRUE),
                    max_q=max(discharge, na.rm=TRUE),
                    min_q=min(discharge, na.rm=TRUE))
seasonal_q$min_q[seasonal_q$min_q == Inf] <- NA
seasonal_q$max_q[seasonal_q$max_q == -Inf] <- NA

ddply(seasonal_q, .(Station, Season), summarize,
      series_mean_mean=mean(mean_q, na.rm=TRUE),
      series_mean_min=mean(min_q, na.rm=TRUE),
      series_mean_max=mean(max_q, na.rm=TRUE))

labeldata <- ddply(seasonal_q, .(Station, Season), eqnfunc_slope, 'mean_q ~ order(Year)')
labeldata
seasonal_mean_q_plot <- ggplot(seasonal_q, aes(Year, mean_q)) +
    geom_line() + xlab('Year') + facet_grid(Station ~ Season, scales='free') +
    ylab(expression(paste('Seasonal mean Q (m'^3, '/s)', sep=''))) +
    geom_smooth(method="lm", se=TRUE, color="black") +
    theme(strip.text.y=element_text(size=27))
png('discharge_seasonal_mean_q.png', width=PLOT_WIDTH*PLOT_DPI*2, height=PLOT_HEIGHT*PLOT_DPI*2)
print(seasonal_mean_q_plot)
dev.off()

labeldata <- ddply(seasonal_q, .(Station, Season), eqnfunc_slope, 'max_q ~ order(Year)')
labeldata
seasonal_max_q_plot <- ggplot(seasonal_q, aes(Year, max_q)) +
    geom_line() + xlab('Year') + facet_grid(Station ~ Season, scales='free') +
    ylab(expression(paste('Seasonal max. Q (m'^3, '/s)', sep=''))) +
    geom_smooth(method="lm", se=TRUE, color="black") +
    theme(strip.text.y=element_text(size=27))
png('discharge_seasonal_max_q.png', width=PLOT_WIDTH*PLOT_DPI*2, height=PLOT_HEIGHT*PLOT_DPI*2)
print(seasonal_max_q_plot)
dev.off()

labeldata <- ddply(seasonal_q, .(Station, Season), eqnfunc_slope, 'min_q ~ order(Year)')
labeldata
seasonal_min_q_plot <- ggplot(seasonal_q, aes(Year, min_q)) +
    geom_line() + xlab('Year') + facet_grid(Station ~ Season, scales='free') +
    ylab(expression(paste('Seasonal min. Q (m'^3, '/s)', sep=''))) +
    geom_smooth(method="lm", se=TRUE, color="black") +
    theme(strip.text.y=element_text(size=27))
png('discharge_seasonal_min_q.png', width=PLOT_WIDTH*PLOT_DPI*2, height=PLOT_HEIGHT*PLOT_DPI*2)
print(seasonal_min_q_plot)
dev.off()
