library(lubridate)
library(ggplot2)
library(gridExtra)
library(plyr)

source('0_utility_functions.R')

theme_set(theme_grey(base_size=30))
update_geom_defaults("smooth", aes(size=1))
update_geom_defaults("line", aes(size=1))
# The four below margins are the page margins in inches - by default figures 
# will be sized up to half the column width and 1/3 the page height )minus the 
# caption_space) at 300 DPI.
right_mar = 1.25
left_mar = 1.5
up_mar = 1.25
low_mar = 1.25
# How much space to leave for a caption (in inches) on a full page 2 col x 3 
# row multiplot?
caption_space <- 1
PLOT_WIDTH = (8.5 - left_mar - right_mar) / 2
PLOT_HEIGHT = (11 - up_mar - low_mar - caption_space) / 3
PLOT_DPI = 300

stdErr <- function(x) {sd(x, na.rm=TRUE)/ sqrt(length(x[!is.na(x)]))}

load('precip_daily_Chitwan_cleaned.Rdata')

precip$Period <- cut(precip$Year, c(1980, 1995, 2010), dig.lab=4)

#precip <- precip[precip$Year> 1980, ]

###############################################################################
# Note that many of these metrics are from:
# Bodini, A., and Q. A. Cossu. “Vulnerability Assessment of Central-East 
# Sardinia (Italy) to Extreme Rainfall Events.” Nat. Hazards Earth Syst. Sci 10 
# (2010): 61–72.

###############################################################################
# Annual precip
ann_missings <- aggregate(is.na(precip$precip),
                          by=list(Year=precip$Year, Station=precip$Station), sum, 
                          na.rm=TRUE)
names(ann_missings)[names(ann_missings) == 'x'] <- 'missings'
ann <- aggregate(precip$precip,
                 by=list(Year=precip$Year, Station=precip$Station), sum, 
                 na.rm=TRUE)
names(ann)[names(ann) == 'x'] <- 'annual_total'
ann$annual_total[ann_missings$missing > 10] <- NA
labeldata <- ddply(ann, .(Station), eqnfunc_slope, 'annual_total ~ order(Year)')
labeldata$eqn <- gsub('order[(]Year[)]', 'Year', labeldata$eqn)
labeldata$eqn <- gsub('annual_total', 'Total', labeldata$eqn)
ann_plot <- ggplot(ann, aes(Year, annual_total)) + facet_grid(Station ~ .) +
    geom_line() + xlab('Year') + ylab('Precip. (mm/year)') +
    geom_smooth(method="lm", se=TRUE) + 
    geom_text(data=labeldata, aes(x=1970, y=3000, label=eqn), parse=TRUE, 
              colour='black', hjust=0, size=8)
png('precip_annual.png', width=PLOT_WIDTH*PLOT_DPI, height=PLOT_HEIGHT*PLOT_DPI)
print(ann_plot)
dev.off()

###############################################################################
# 3 year rolling mean annual precip
filter_years <- 5
filter_size <- 365*filter_years
ann_roll_mean <- ddply(precip, .(Station), summarize,
                      Date=Date, Year=Year, Month=Month, Day=Day,
                      mean=filter(as.matrix(precip), rep(1/filter_size, filter_size))*365)
ann_roll_mean_plot <- ggplot(ann_roll_mean, aes(Date, mean)) +
    facet_grid(Station ~ .) + geom_line() + xlab('Year') +
    ylab(paste(filter_years, '-year average precip. (mm/year)', sep='')) +
    geom_rect(aes(xmin=as.Date('1997/02/01'), xmax=as.Date('2006/01/01'), 
                  ymin=1000, ymax=3000), alpha=.005, color='black') +
    geom_text(aes(x=as.Date("2001/08/15"), y=1100, label="CVFS"))
png(paste('precip_annual_', filter_years, 'yr_mean.png', sep=''), width=PLOT_WIDTH*PLOT_DPI, height=PLOT_HEIGHT*PLOT_DPI)
print(ann_roll_mean_plot)
dev.off()

###############################################################################
# 95th percentiles
gt_95 <- aggregate(precip$gt_pct_95,
                   by=list(Year=precip$Year, Station=precip$Station),
                   sum)
names(gt_95)[names(gt_95) == 'x'] <- 'num_days'
labeldata <- ddply(gt_95, .(Station), eqnfunc_slope, 'num_days ~ order(Year)')
labeldata$eqn <- gsub('order[(]Year[)]', 'Year', labeldata$eqn)
labeldata$eqn <- gsub('num_days', 'Num. Days', labeldata$eqn)
pct_95_plot <- ggplot(gt_95, aes(Year, num_days)) +
    geom_line() + xlab('Year') + facet_grid(Station ~ .) +
    ylab('Days above 95th pct.') + 
    geom_smooth(method="lm", se=TRUE) +
    geom_text(data=labeldata, aes(x=1970, y=13, label=eqn), parse=TRUE, 
              colour='black', hjust=0, size=8) + ylim(c(0,14))
png('precip_95th_above.png', width=PLOT_WIDTH*PLOT_DPI, height=PLOT_HEIGHT*PLOT_DPI)
print(pct_95_plot)
dev.off()

###############################################################################
# Pentad max total precip
pentad_sum <- ddply(precip, .(Station, Year, Pentad), summarize,
                    pentad_sum=sum(precip, na.rm=TRUE))
pentad_max <- ddply(pentad_sum, .(Station, Year), summarize,
                    pentad_max=max(pentad_sum, na.rm=TRUE))
# Set years where there was no data at all  to NA
pentad_max$pentad_max[pentad_max$pentad_max == 0] <- NA
labeldata <- ddply(pentad_max, .(Station), eqnfunc_slope, 'pentad_max ~ order(Year)')
labeldata$eqn <- gsub('order[(]Year[)]', 'Year', labeldata$eqn)
labeldata$eqn <- gsub('pentad_max', 'Max', labeldata$eqn)
max_5_day_plot <- ggplot(pentad_max, aes(Year, pentad_max)) +
    geom_line() + xlab('Year') + facet_grid(Station ~ .) +
    ylab('Max. 5 day precip. (mm/day)') + 
    geom_smooth(method="lm", se=TRUE) +
    geom_text(data=labeldata, aes(x=1970, y=500, label=eqn), parse=TRUE, 
              colour='black', hjust=0, size=8)
png('precip_pentad_max.png', width=PLOT_WIDTH*PLOT_DPI, height=PLOT_HEIGHT*PLOT_DPI)
print(max_5_day_plot)
dev.off()

###############################################################################
# Climatological pentad mean
CPM <- ddply(precip, .(Station, Pentad), summarize,
             CPM=mean(precip, na.rm=TRUE),
             stdErr=stdErr(precip))
CPM$lower_conf <- CPM$CPM - 1.96*CPM$stdErr
CPM$upper_conf <- CPM$CPM + 1.96*CPM$stdErr
CPM_plot <- ggplot(CPM, aes(Pentad, CPM, colour=Station, linetype=Station)) +
    geom_line() + xlab('Pentad') +
    ylab('Mean precip. (mm/day)')
    #geom_ribbon(aes(x=Pentad, ymin=lower_conf, ymax=upper_conf, fill=Period),
    #            alpha=.2, data=CPM)
png('precip_CPM.png', width=PLOT_WIDTH*PLOT_DPI, height=PLOT_HEIGHT*PLOT_DPI)
print(CPM_plot)
dev.off()

###############################################################################
# Climatological pentad mean - mean across all stations
CPM <- ddply(precip, .(Pentad), summarize,
             CPM=mean(precip, na.rm=TRUE),
             stdErr=stdErr(precip))
CPM$lower_conf <- CPM$CPM - 1.96*CPM$stdErr
CPM$upper_conf <- CPM$CPM + 1.96*CPM$stdErr
CPM_plot <- ggplot(CPM, aes(Pentad, CPM)) +
    geom_line() + xlab('Pentad') +
    ylab('Mean precip. (mm/day)')
    #geom_ribbon(aes(x=Pentad, ymin=lower_conf, ymax=upper_conf, fill=Period),
    #            alpha=.2, data=CPM)
png('precip_CPM_all_stations.png', width=PLOT_WIDTH*PLOT_DPI, height=PLOT_HEIGHT*PLOT_DPI)
print(CPM_plot)
dev.off()

###############################################################################
# Proportion of annual cumulate due to extreme events (q0.95)
precip_annual_EP_frac <- ddply(precip, .(Station, Year), summarize, 
                               EP_frac=(sum(precip[gt_pct_95], na.rm=TRUE) / 
                                        sum(precip)))
labeldata <- ddply(precip_annual_EP_frac, .(Station), eqnfunc_slope, 'EP_frac ~ order(Year)')
labeldata$eqn <- gsub('order[(]Year[)]', 'Year', labeldata$eqn)
labeldata$eqn <- gsub('EP_frac', 'Fraction', labeldata$eqn)
EP_frac_plot <- ggplot(precip_annual_EP_frac, aes(Year, EP_frac)) +
    geom_line() + xlab('Year') + facet_grid(Station ~ .) +
    ylab('Fraction precip. from extreme events') + 
    geom_smooth(method="lm", se=TRUE) +
    geom_text(data=labeldata, aes(x=1970, y=.5, label=eqn), parse=TRUE, 
              colour='black', hjust=0, size=8) + ylim(c(0,.65))
png('precip_EP_frac.png', width=PLOT_WIDTH*PLOT_DPI, height=PLOT_HEIGHT*PLOT_DPI)
print(EP_frac_plot)
dev.off()

###############################################################################
# Number of rainy days
num_rainy_days <- ddply(precip, .(Station, Year), summarize, 
                               rainy_days=sum(precip > 0, na.rm=TRUE))
num_rainy_days$rainy_days[num_rainy_days$rainy_days == 0] <- NA
labeldata <- ddply(num_rainy_days, .(Station), eqnfunc_slope, 'rainy_days ~ 
                   order(Year)')
labeldata$eqn <- gsub('order[(]Year[)]', 'Year', labeldata$eqn)
labeldata$eqn <- gsub('rainy_days', 'Num. Days', labeldata$eqn)
labeldata$x_pos <- 1970
labeldata$y_pos <- c(90, 140, 140)
rainy_days_plot <- ggplot(num_rainy_days, aes(Year, rainy_days)) +
    geom_line() + xlab('Year') + facet_grid(Station ~ .) +
    ylab('Rainy days') + 
    geom_smooth(method="lm", se=TRUE) +
    geom_text(data=labeldata, aes(x=x_pos, y=y_pos, label=eqn), parse=TRUE, 
              colour='black', hjust=0, size=8)
png('precip_rainy_days.png', width=PLOT_WIDTH*PLOT_DPI, height=PLOT_HEIGHT*PLOT_DPI)
print(rainy_days_plot)
dev.off()

###############################################################################
# Annual precip curves
total_precip_monthly <- ddply(precip, .(Station, Year, Month), summarize, 
                               total_precip=sum(precip, na.rm=TRUE))
total_precip_monthly$total_precip[total_precip_monthly$total_precip == 0] <- NA
total_precip_monthly$Period <- cut(total_precip_monthly$Year, c(1980, 1995, 2010), dig.lab=4)
total_precip_monthly <- total_precip_monthly[!is.na(total_precip_monthly$Period), ]
mean_total_precip_monthly <- ddply(total_precip_monthly, .(Station, Period, Month), summarize, 
                               mean_total=mean(total_precip, na.rm=TRUE),
                               stdErr=stdErr(total_precip))
mean_total_precip_monthly$mean_total_lower_conf <- mean_total_precip_monthly$mean_total - 1.96*mean_total_precip_monthly$stdErr
mean_total_precip_monthly$mean_total_upper_conf <- mean_total_precip_monthly$mean_total + 1.96*mean_total_precip_monthly$stdErr
monthly_precip_plot <- ggplot(mean_total_precip_monthly, aes(Month, mean_total, colour=Period)) +
    geom_line() + xlab('Month') + facet_grid(Station ~ .) +
    ylab('Mean precip. (mm/month)') + 
    geom_ribbon(aes(x=Month, ymin=mean_total_lower_conf, 
                    ymax=mean_total_upper_conf, fill=Period),
                alpha=.2, data=mean_total_precip_monthly)
png('precip_monthly.png', width=PLOT_WIDTH*PLOT_DPI, height=PLOT_HEIGHT*PLOT_DPI)
print(monthly_precip_plot)
dev.off()

###############################################################################
# Precip anomalies by month
monthly_total <- ddply(precip, .(Station, Year, Month), summarize, 
                           total=sum(precip))
monthly_mean_clim <- ddply(monthly_total, .(Station, Month), summarize, 
                           mean=mean(total, na.rm=TRUE))
monthly_anom <- ddply(monthly_total, .(Station, Year, Month), summarize, 
                      anom=(total - 
                      monthly_mean_clim$mean[monthly_mean_clim$Station == Station & monthly_mean_clim$Month == Month]))
monthly_anom$Date <- as.Date(paste(monthly_anom$Year, monthly_anom$Month, '15'), format='%Y %m %d')
monthly_anom_plot <- ggplot(monthly_anom, aes(Date, anom)) +
    geom_line() + xlab('Year') + facet_grid(Station ~ .) +
    ylab('Precip. anomaly (mm/month)') +
    geom_rect(aes(xmin=as.Date('1997/02/01'), xmax=as.Date('2006/01/01'), 
                  ymin=-500, ymax=750), alpha=.005, color='black') +
    geom_text(aes(x=as.Date("2001/08/15"), y=-425, label="CVFS"))
png('precip_monthly_anom.png', width=PLOT_WIDTH*PLOT_DPI, height=PLOT_HEIGHT*PLOT_DPI)
print(monthly_anom_plot)
dev.off()

###############################################################################
# Monsoon onset and end date. Procedure follows the reference below:
#   Marengo, Jose A., Brant Liebmann, Vernon E. Kousky, Naziano P. Filizola, and 
#   Ilana C. Wainer. “Onset and End of the Rainy Season in the Brazilian 
#   Amazon Basin.” Journal of Climate 14, no. 5 (March 2001): 833–852.  
# Onset (end) date defined as date on which precip is greater (less) than 2.5 mm,
# provided that 6 of the 8 preceding (subsequent) pentads had precipitation of 
# less than 2 mm per day, and that 6 of the 8 subsequent (preceding) pentads 
# had precipitation of greater (less) than 3 mm per day.
check_neighbors <- function(x, num_neighbors, preceding=FALSE) {
    x <- matrix(x)
    # First build a matrix of neighbors, either subsequent or preceding
    lags <- seq(1, num_neighbors, 1)
    lagged <- matrix(NA, nrow=nrow(x), ncol=length(lags))
    for (n in 1:length(lags)) {
        this_lag <- lags[n]
        if (preceding) {
            lagged[, n] <- c(rep(NA, this_lag), x[1:(length(x) - this_lag)])
        } else {
            lagged[, n] <- c(x[(1 + this_lag):length(x)], rep(NA, this_lag))
        }
    }
    return(lagged)
}

pentad_sum <- ddply(precip, .(Station, Year, Pentad), summarize,
                    sum=sum(precip))

thresh <- 3
low_thresh <- thresh - .5
up_thresh <- thresh + .5
num_neigh <- 6
num_meet <- 4
onset <- ddply(pentad_sum, .(Station), summarize,
               Year=Year, Pentad=Pentad, sum=sum, sum_gt_thresh=sum > thresh,
               preced_lt_low_thresh=(rowSums(check_neighbors(sum, num_neighbors=num_neigh, preceding=TRUE) < low_thresh) >= num_meet),
               subseq_gt_up_thresh=(rowSums(check_neighbors(sum, num_neighbors=num_neigh) > up_thresh) >= num_meet))
onset$onset <- onset$sum_gt_thresh & onset$preced_lt_low_thresh & onset$subseq_gt_up_thresh
onset_date <- ddply(onset, .(Station, Year), summarize, onset_pentad=match(TRUE, onset))
(no_onset_years <- onset_date[is.na(onset_date$onset_pentad),])
table(is.na(onset_date$onset_pentad))
labeldata <- ddply(onset_date, .(Station), eqnfunc_slope, 'onset_pentad ~ order(Year)')
onset_date_plot <- ggplot(onset_date, aes(Year, onset_pentad)) +
    geom_line() + xlab('Year') + facet_grid(Station ~ .) +
    ylab('Monsoon onset pentad') + 
    geom_smooth(method="lm", se=TRUE) +
    geom_text(data=labeldata, aes(x=1970, y=35, label=eqn), parse=TRUE, 
              colour='black', hjust=0, size=8) +
    geom_segment(data=no_onset_years, aes(x=Year, y=0, xend=Year, yend=40), 
                 alpha=.3, color='black', size=1)
png('precip_monsoon_onset_date.png', width=PLOT_WIDTH*PLOT_DPI, height=PLOT_HEIGHT*PLOT_DPI)
print(onset_date_plot)
dev.off()

end <- ddply(pentad_sum, .(Station), summarize,
             Year=Year, Pentad=Pentad, sum=sum, sum_lt_thresh=sum < thresh,
             preced_gt_up_thresh=(rowSums(check_neighbors(sum, num_neighbors=num_neigh, preceding=TRUE) > up_thresh) >= num_meet),
             subseq_lt_low_thresh=(rowSums(check_neighbors(sum, num_neighbors=num_neigh) < low_thresh) >= num_meet))
end$end <- end$sum_lt_thresh & end$preced_gt_up_thresh & end$subseq_lt_low_thresh
# Set end dates before the 40th pentad to NA
end$end[end$end & end$Pentad < 40] <- NA
(no_end_years <- end_date[is.na(end_date$end_pentad),])
end_date <- ddply(end, .(Station, Year), summarize, end_pentad=match(TRUE, end))
table(is.na(end_date$end_pentad))
labeldata <- ddply(end_date, .(Station), eqnfunc_slope, 'end_pentad ~ order(Year)')
end_date_plot <- ggplot(end_date, aes(Year, end_pentad)) +
    geom_line() + xlab('Year') + facet_grid(Station ~ .) +
    ylab('Monsoon end pentad') + 
    geom_smooth(method="lm", se=TRUE) +
    geom_text(data=labeldata, aes(x=1970, y=35, label=eqn), parse=TRUE, 
              colour='black', hjust=0, size=8)
png('precip_monsoon_end_date.png', width=PLOT_WIDTH*PLOT_DPI, height=PLOT_HEIGHT*PLOT_DPI)
print(end_date_plot)
dev.off()

# Try start and end dates on the mean series from the three stations
stn_mean_onset <- ddply(precip, .(Year, Pentad), summarize,
                         mean_sum=mean(precip, na.rm=TRUE))
stn_mean_onset$sum_gt_thresh <- stn_mean_onset$mean_sum > thresh
stn_mean_onset$preced_lt_low_thresh <- rowSums(check_neighbors(stn_mean_onset$mean_sum, num_neighbors=num_neigh, preceding=TRUE) < low_thresh) >= num_meet
stn_mean_onset$subseq_gt_up_thresh <- rowSums(check_neighbors(stn_mean_onset$mean_sum, num_neighbors=num_neigh) > up_thresh) >= num_meet
stn_mean_onset$onset <- stn_mean_onset$sum_gt_thresh & stn_mean_onset$preced_lt_low_thresh & stn_mean_onset$subseq_gt_up_thresh
stn_mean_onset_date <- ddply(stn_mean_onset, .(Year), summarize, onset_pentad=match(TRUE, onset))
table(is.na(stn_mean_onset_date$onset_pentad))
labeldata <- eqnfunc_slope(stn_mean_onset_date, 'onset_pentad ~ order(Year)')
stn_mean_onset_date_plot <- ggplot(stn_mean_onset_date, aes(Year, onset_pentad)) +
    geom_line() + xlab('Year') +
    ylab('Monsoon onset pentad') + 
    geom_smooth(method="lm", se=TRUE) +
    geom_text(aes(x=1970, y=35, label=labeldata), parse=TRUE, 
              colour='black', hjust=0, size=8)
png('precip_monsoon_onset_date_station_mean.png', width=PLOT_WIDTH*PLOT_DPI, height=PLOT_HEIGHT*PLOT_DPI)
print(stn_mean_onset_date_plot)
dev.off()

stn_mean_end <- ddply(precip, .(Year, Pentad), summarize,
                         mean_sum=mean(precip, na.rm=TRUE))
stn_mean_end$sum_lt_thresh <- stn_mean_end$mean_sum < thresh
stn_mean_end$preced_gt_up_thresh <- rowSums(check_neighbors(stn_mean_end$mean_sum, num_neighbors=num_neigh, preceding=TRUE) > up_thresh) >= num_meet
stn_mean_end$subseq_lt_low_thresh <- rowSums(check_neighbors(stn_mean_end$mean_sum, num_neighbors=num_neigh) < low_thresh) >= num_meet
stn_mean_end$end <- stn_mean_end$sum_lt_thresh & stn_mean_end$preced_gt_up_thresh & stn_mean_end$subseq_lt_low_thresh
stn_mean_end_date <- ddply(stn_mean_end, .(Year), summarize, end_pentad=match(TRUE, end))
table(is.na(stn_mean_end_date$end_pentad))
labeldata <- eqnfunc_slope(stn_mean_end_date, 'end_pentad ~ order(Year)')
stn_mean_end_date_plot <- ggplot(stn_mean_end_date, aes(Year, end_pentad)) +
    geom_line() + xlab('Year') +
    ylab('Monsoon end pentad') + 
    geom_smooth(method="lm", se=TRUE) +
    geom_text(aes(x=1992, y=59, label=labeldata), parse=TRUE, 
              colour='black', hjust=0, size=8)
png('precip_monsoon_end_date_station_mean.png', width=PLOT_WIDTH*PLOT_DPI, height=PLOT_HEIGHT*PLOT_DPI)
print(stn_mean_end_date_plot)
dev.off()

###############################################################################
# Save multi_plot
grid_cols <- 2
grid_rows <- 3
png('precip_multiplot.png', width=PLOT_WIDTH*PLOT_DPI*grid_cols, 
    height=PLOT_HEIGHT*PLOT_DPI*grid_rows)
grid.arrange(ann_plot, pct_95_plot, monthly_anom_plot, EP_frac_plot, 
             rainy_days_plot, max_5_day_plot, ncol=2)
dev.off()
