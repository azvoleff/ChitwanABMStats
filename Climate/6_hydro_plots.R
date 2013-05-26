library(lubridate)
library(ggplot2)
library(gridExtra)
library(plyr)
library(scales)
library(boot)
library(hydroTSM) # for 'fdc'

source('0_utility_functions.R')

stdErrMean <- function(x) {sd(x, na.rm=TRUE)/ sqrt(length(x[!is.na(x)]))}
stdErrDiffMean <- function(x, y) {
    nx <- length(x[!is.na(x)])
    ny <- length(y[!is.na(y)])
    sqrt(var(x, na.rm=TRUE) / nx + var(y, na.rm=TRUE) / ny)
}

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

###############################################################################
###############################################################################
# Now focus on station 450
###############################################################################
###############################################################################

discharge <- discharge[discharge$Station == 'AQ450', ]

###############################################################################
# Percentiles
gt_95_450 <- ddply(discharge, .(Year), summarize, 
                   num_days=sum(discharge_gt_95, na.rm=TRUE))
labeldata <- eqnfunc_slope(gt_95_450, 'num_days ~ order(Year)')
discharge_gt_95_450_plot <- ggplot(gt_95_450, aes(Year, num_days)) +
geom_line() + xlab('Year') +
ylab('Days discharge above 95th percentile') +
geom_smooth(method="lm", se=TRUE, color="black") +
geom_text(aes(x=1965, y=40, label=labeldata), parse=TRUE, 
          colour='black', hjust=0, size=8)
png('discharge_AQ450_gt_95.png', width=PLOT_WIDTH*PLOT_DPI, height=PLOT_HEIGHT*PLOT_DPI)
print(discharge_gt_95_450_plot)
dev.off()

# Now plot with separate 95th percentiles calculated for each season to see if 
# the number of seasonally extreme events is increasing.
discharge <- ddply(discharge, .(Season), transform,
                   discharge_gt_95_seasonal=is_extreme(discharge, 95))
gt_95_450_seasonal <- ddply(discharge, .(Year, Season), summarize, 
                            num_days=sum(discharge_gt_95_seasonal, na.rm=TRUE))
labeldata <- ddply(gt_95_450_seasonal, .(Season), eqnfunc_slope, 'num_days ~ order(Year)')
labeldata
discharge_gt_95_450_seasonal_plot <- ggplot(gt_95_450_seasonal, aes(Year, num_days)) +
geom_line() + xlab('Year') + facet_grid(Season ~ ., scales='free') +
ylab('Days discharge above 95th percentile') +
geom_smooth(method="lm", se=TRUE, color="black") +
theme(strip.text.y=element_text(size=27))
png('discharge_AQ450_gt_95_seasonal.png', width=PLOT_WIDTH*PLOT_DPI, height=PLOT_HEIGHT*PLOT_DPI)
print(discharge_gt_95_450_seasonal_plot)
dev.off()

###############################################################################
# Annual discharge
ann_450 <- ddply(discharge, .(Year), summarize, total=sum(discharge, na.rm=TRUE))
labeldata <- eqnfunc_slope(ann_450, 'total ~ order(Year)')
discharge_annual_plot_450 <- ggplot(ann_450, aes(Year, total)) +
geom_line() + xlab('Year') +
ylab(expression(paste('Total annual discharge (m'^3, ')', sep=''))) +
geom_smooth(method="lm", se=TRUE, color="black") +
geom_text(aes(x=1968, y=700000, label=labeldata), parse=TRUE, 
          colour='black', hjust=0, size=8)
png('discharge_AQ450_annual.png', width=PLOT_WIDTH*PLOT_DPI, height=PLOT_HEIGHT*PLOT_DPI)
print(discharge_annual_plot_450)
dev.off()

###############################################################################
# Annual discharge curves
# Note that discharge is in m3 per second, so must be multiplied by 86400 to 
# get daily
monthly_q <- ddply(discharge, .(Year, Month), summarize, 
                   q=sum(discharge*86400, na.rm=TRUE))
monthly_q$q[monthly_q$q == 0] <- NA
monthly_q$Period <- cut(monthly_q$Year, c(1963, 1978, 1993, 2008), dig.lab=4)
monthly_q <- monthly_q[!is.na(monthly_q$Period), ]
#monthly_q <- monthly_q[!monthly_q$Period == '(1978,1993]', ]
q_monthly_curves <- ddply(monthly_q, .(Period, Month), summarize, 
                          mean_q=mean(q, na.rm=TRUE),
                          stdErrMean=stdErrMean(q))
q_monthly_curves$mean_q_lcl <- q_monthly_curves$mean_q - 1.96*q_monthly_curves$stdErrMean
q_monthly_curves$mean_q_ucl <- q_monthly_curves$mean_q + 1.96*q_monthly_curves$stdErrMean
q_monthly_curves$Month_Date <- as.Date(paste(9999, q_monthly_curves$Month, '1'), format='%Y %m %d')
monthly_discharge_plot <- ggplot(q_monthly_curves, aes(Month_Date, mean_q, colour=Period)) +
geom_line() + xlab('Month') + 
ylab(expression(paste('Mean discharge (m'^3, '/month)', sep=''))) +
geom_ribbon(aes(x=Month_Date, ymin=mean_q_lcl, 
                ymax=mean_q_ucl, fill=Period),
            alpha=.2, data=q_monthly_curves) +
scale_y_continuous(labels=scientific_format()) +
scale_x_date(labels=date_format("%b")) +
theme(legend.position='bottom')
png('discharge_AQ450_monthly.png', width=PLOT_WIDTH*PLOT_DPI, height=PLOT_HEIGHT*PLOT_DPI)
print(monthly_discharge_plot)
dev.off()

# Try displaying it another way - as percentage change with errorbars
base_period <- '(1963,1978]'
base_curve <- q_monthly_curves[q_monthly_curves$Period == base_period, ]
# Calculate standard error the difference of the means
get_std_err_diff_from_base <- function(Period, Month) {
    base <- monthly_q$q[monthly_q$Period == base_period & monthly_q$Month == Month]
    comp <- monthly_q$q[monthly_q$Period == Period & monthly_q$Month == Month]
    stdErrDiffMean(base, comp)
}
q_monthly_curves <- ddply(q_monthly_curves, .(Period, Month), transform,
                          stdErrDiff=get_std_err_diff_from_base(Period, Month),
                          mean_diff=(mean_q - base_curve$mean_q[match(Month, base_curve$Month)]))

q_monthly_curves$mean_diff_lcl <- q_monthly_curves$mean_diff - 1.96*q_monthly_curves$stdErrDiff
q_monthly_curves$mean_diff_ucl <- q_monthly_curves$mean_diff + 1.96*q_monthly_curves$stdErrDiff
# Now convert mean differences to fractions of the base
q_monthly_curves <- ddply(q_monthly_curves, .(Period), transform,
                          mean_diff_frac=mean_diff/base_curve$mean_q[match(Month, base_curve$Month)],
                          mean_diff_frac_lcl=mean_diff_lcl/base_curve$mean_q[match(Month, base_curve$Month)],
                          mean_diff_frac_ucl=mean_diff_ucl/base_curve$mean_q[match(Month, base_curve$Month)])
monthly_discharge_plot <- ggplot(q_monthly_curves[q_monthly_curves$Period != base_period, ]) +
    geom_bar(aes(x=Month_Date, y=mean_diff_frac, fill=Period), stat='identity', position="dodge") +
    xlab('Month') + 
    ylab('Deviation from 1963-1978 mean') +
    geom_errorbar(aes(x=Month_Date, ymin=mean_diff_frac_lcl, 
                      ymax=mean_diff_frac_ucl, fill=Period), alpha=.2, position="dodge") +
    scale_x_date(labels=date_format("%b")) +
    scale_y_continuous(labels=percent) +
png('discharge_AQ450_monthly_pct_anom.png', width=PLOT_WIDTH*PLOT_DPI, height=PLOT_HEIGHT*PLOT_DPI)
print(monthly_discharge_plot)
dev.off()

#  The bootstrapping code below takes about 10 minutes to run (if iterations is 
#  set to 10000), so it is commented out.
#
# Bootstrap confidence intervals for difference of means between periods
# boot_mean_diff_conf <- function(comp_data, base_data, iterations=2000) {
#     base_period <- unique(base_data$Period)
#     if (length(base_period) > 1) {stop('base data contains more than one period')}
#     comp_period <- unique(comp_data$Period)
#     if (length(comp_period) > 1) {stop('comparison data contains more than one period')}
#     comp_month <- unique(comp_data$Month)
#     if (length(comp_month) > 1) {stop('comparison data contains more than one month')}
#     q_data <- rbind(base_data[base_data$Month == comp_month, ], comp_data)
#     meanDiffPct <- function(dataFrame, indexVector, base_period, comp_period) { 
#         m1 <- mean(subset(dataFrame[indexVector, ]$q, dataFrame[indexVector, ]$Period == base_period), na.rm=TRUE)
#         m2 <- mean(subset(dataFrame[indexVector, ]$q, dataFrame[indexVector, ]$Period == comp_period), na.rm=TRUE)
#         m <- (m2 - m1) / m1
#         return(m)
#     }
#     q_data_boot <- boot(q_data, meanDiffPct, R=iterations, strata=q_data$Period, 
#                         base_period=base_period, comp_period=comp_period)
#     q_data_boot_CI <- boot.ci(q_data_boot, type='perc')
#     return(data.frame(diff_pct=q_data_boot_CI$t0, lcl=q_data_boot_CI$percent[4], ucl=q_data_boot_CI$percent[5]))
# }
# monthly_q_base <- monthly_q[monthly_q$Period == base_period, ]
# boot_mean_diff_conf(monthly_q[monthly_q$Period == '(1978,1993]' & monthly_q$Month == 1, ], monthly_q_base)
# bootstrap_ints <- ddply(monthly_q, .(Period, Month), boot_mean_diff_conf, monthly_q_base, iterations=10000)
# bootstrap_ints$Month_Date <- as.Date(paste(9999, bootstrap_ints$Month, '1'), format='%Y %m %d')
# 
# monthly_discharge_plot_bootstrap <- ggplot(bootstrap_ints[bootstrap_ints$Period != base_period, ]) +
#     geom_bar(aes(x=Month_Date, y=diff_pct, fill=Period), stat='identity', position="dodge") +
#     xlab('Month') + 
#     ylab('Deviation from 1963-1978 mean') +
#     geom_errorbar(aes(x=Month_Date, ymin=lcl, 
#                       ymax=ucl, fill=Period), alpha=.2, position="dodge") +
#     scale_x_date(labels=date_format("%b")) +
#     scale_y_continuous(labels=percent) +
#     scale_fill_discrete(guide=guide_legend(breaks=c('(1977-1993]', '(1993-2008]')),
#                                            labels=c('1978-1993', '1994-2008'))
# png('discharge_AQ450_monthly_pct_anom_bootstrap.png', width=PLOT_WIDTH*PLOT_DPI, height=PLOT_HEIGHT*PLOT_DPI)
# print(monthly_discharge_plot_bootstrap)
# dev.off()

###############################################################################
# Monthly discharge anomaly
q_monthly <- ddply(discharge, .(Year, Month), summarize,
                   total=sum(discharge*86400, na.rm=TRUE))
q_monthly_mean <- ddply(q_monthly, .(Month), summarize,
                        mean_q=mean(total))
q_monthly$anom <- q_monthly$total - q_monthly_mean$mean_q[match(q_monthly$Month, q_monthly_mean$Month)]
q_monthly$Date <- as.Date(paste(q_monthly$Year,
                                q_monthly$Month, '15'), 
                          format='%Y %m %d')
q_monthly_anom_plot <- ggplot(q_monthly, aes(Date, anom)) +
geom_line() + xlab('Year') +
ylab(expression(paste('Discharge anomaly (m'^3, '/month)', sep=''))) +
geom_smooth(method="lm", se=TRUE, color="black") +
scale_y_continuous(labels=scientific_format())
png('discharge_AQ450_monthly_anom.png', width=PLOT_WIDTH*PLOT_DPI,
    height=PLOT_HEIGHT*PLOT_DPI)
print(q_monthly_anom_plot)
dev.off()

###############################################################################
# Flow discharge curve
png('discharge_AQ450_fdc_curve.png', width=PLOT_WIDTH*PLOT_DPI, 
    height=PLOT_HEIGHT*PLOT_DPI)
# For par: bottom, left, top, right
par(mar=c(3, 3, .75, .5), cex=3)
res <- fdc(discharge$discharge, cex=.25, cex.lab=.75, cex.axis=.75, main='', mgp=c(2, 0, 0))
dev.off()

###############################################################################
# Mmax,i and Mmin,i
#
# From:
#   Diadovski, Ivan K. “Risk Assessment of Extreme Events Along a River Flow.” 
#   Journal of Water Resource and Protection 02, no. 05 (2010): 455–461.  
#   doi:10.4236/jwarp.2010.25052.
qextrema_450 <- ddply(discharge, .(Year), summarize,
                      qmin=min(discharge, na.rm=TRUE),
                      qmax=max(discharge, na.rm=TRUE))
qextrema_450$qmin_anom <- with(qextrema_450, qmin - mean(qmin))
qextrema_450$qmax_anom<- with(qextrema_450, qmax - mean(qmax))
qextrema_450$mmin <- with(qextrema_450, qmin / mean(qmin))
qextrema_450$mmax <- with(qextrema_450, qmax / mean(qmax))

labeldata <- eqnfunc_slope(qextrema_450, 'mmin ~ order(Year)')
discharge_mmin_plot_450 <- ggplot(qextrema_450, aes(Year, mmin)) +
geom_line() + xlab('Year') +
ylab(expression(paste('M'[min], sep=''))) +
geom_smooth(method="lm", se=TRUE, color="black") + 
geom_text(aes(x=1965, y=1.5, label=labeldata), parse=TRUE, 
          colour='black', hjust=0, size=8)
png('discharge_AQ450_mmin.png', width=PLOT_WIDTH*PLOT_DPI, height=PLOT_HEIGHT*PLOT_DPI)
print(discharge_mmin_plot_450)
dev.off()

labeldata <- eqnfunc_slope(qextrema_450, 'mmax ~ order(Year)')
discharge_mmax_plot_450 <- ggplot(qextrema_450, aes(Year, mmax)) +
geom_line() + xlab('Year') + 
ylab(expression(paste('M'[max], sep=''))) +
geom_smooth(method="lm", se=TRUE, color="black") + 
geom_text(aes(x=1985, y=1.5, label=labeldata), parse=TRUE, 
          colour='black', hjust=0, size=8)
png('discharge_AQ450_mmax.png', width=PLOT_WIDTH*PLOT_DPI, height=PLOT_HEIGHT*PLOT_DPI)
print(discharge_mmax_plot_450)
dev.off()

###############################################################################
# TODO: Also look at proportion of flow by season

###############################################################################
# Seasonal discharge plot
seasonal_q_450 <- ddply(discharge, .(Year, Season), summarize,
                        total_q=sum(discharge*86400, na.rm=TRUE),
                        mean_q_per_sec=mean(discharge, na.rm=TRUE),
                        max_q_per_sec=max(discharge, na.rm=TRUE),
                        min_q_per_sec=min(discharge, na.rm=TRUE))
seasonal_q_450$min_q[seasonal_q_450$min_q == Inf] <- NA
seasonal_q_450$max_q[seasonal_q_450$max_q == -Inf] <- NA

ddply(seasonal_q_450, .(Season), summarize,
      mean_total=mean(total_q, na.rm=TRUE),
      mean_mean_q_per_sec=mean(mean_q_per_sec, na.rm=TRUE),
      mean_min_q_per_sec=mean(min_q_per_sec, na.rm=TRUE),
      mean_max_q_per_sec=mean(max_q_per_sec, na.rm=TRUE))

labeldata <- ddply(seasonal_q_450, .(Season), eqnfunc_slope, 'mean_q_per_sec ~ order(Year)')
labeldata
seasonal_mean_q_per_sec_plot_450 <- ggplot(seasonal_q_450, aes(Year, mean_q_per_sec)) +
geom_line() + xlab('Year') + facet_grid(Season ~ ., scales='free') +
ylab(expression(paste('Seasonal mean Q (m'^3, '/s)', sep=''))) +
geom_smooth(method="lm", se=TRUE, color="black") +
theme(strip.text.y=element_text(size=27))
png('discharge_AQ450_seasonal_mean_q_per_sec.png', width=PLOT_WIDTH*PLOT_DPI, height=PLOT_HEIGHT*PLOT_DPI)
print(seasonal_mean_q_per_sec_plot_450)
dev.off()

labeldata <- ddply(seasonal_q_450, .(Season), eqnfunc_slope, 'max_q_per_sec ~ order(Year)')
labeldata
seasonal_max_q_per_sec_plot_450 <- ggplot(seasonal_q_450, aes(Year, max_q_per_sec)) +
geom_line() + xlab('Year') + facet_grid(Season ~ ., scales='free') +
ylab(expression(paste('Seasonal max. Q (m'^3, '/s)', sep=''))) +
geom_smooth(method="lm", se=TRUE, color="black") +
theme(strip.text.y=element_text(size=27))
png('discharge_AQ450_seasonal_max_q_per_sec.png', width=PLOT_WIDTH*PLOT_DPI, height=PLOT_HEIGHT*PLOT_DPI)
print(seasonal_max_q_per_sec_plot_450)
dev.off()

labeldata <- ddply(seasonal_q_450, .(Season), eqnfunc_slope, 'min_q_per_sec ~ order(Year)')
labeldata
seasonal_min_q_per_sec_plot_450 <- ggplot(seasonal_q_450, aes(Year, min_q_per_sec)) +
geom_line() + xlab('Year') + facet_grid(Season ~ ., scales='free') +
ylab(expression(paste('Seasonal min. Q (m'^3, '/s)', sep=''))) +
geom_smooth(method="lm", se=TRUE, color="black") +
theme(strip.text.y=element_text(size=27))
png('discharge_AQ450_seasonal_min_q_per_sec.png', width=PLOT_WIDTH*PLOT_DPI, height=PLOT_HEIGHT*PLOT_DPI)
print(seasonal_min_q_per_sec_plot_450)
dev.off()

labeldata <- ddply(seasonal_q_450, .(Season), eqnfunc_slope, 'total_q ~ order(Year)')
labeldata
seasonal_total_q_plot_450 <- ggplot(seasonal_q_450, aes(Year, total_q)) +
geom_line() + xlab('Year') + facet_grid(Season ~ ., scales='free') +
ylab(expression(paste('Seasonal discharge (m'^3, ')', sep=''))) +
geom_smooth(method="lm", se=TRUE, color="black") +
theme(strip.text.y=element_text(size=27)) +
scale_y_continuous(labels=scientific_format())
png('discharge_AQ450_seasonal_total_q.png', width=PLOT_WIDTH*PLOT_DPI, height=PLOT_HEIGHT*PLOT_DPI)
print(seasonal_total_q_plot_450)
dev.off()
