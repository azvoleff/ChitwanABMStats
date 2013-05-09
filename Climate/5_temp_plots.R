###############################################################################
###############################################################################
###############################################################################
###############################################################################
###############################################################################
# Add growing degree days
#   Bhusal and Timsina (2011) uses 8C as the base for GDD calculation
#   Amgain (2011) uses base of 10C, for formula: GDD = (Tmax - Tmin)/2 - 10
###############################################################################
###############################################################################
###############################################################################
###############################################################################
###############################################################################
library(lubridate)
library(ggplot2)
library(gridExtra)
library(plyr)

source('0_shared_code.R')

stdErr <- function(x) {sd(x, na.rm=TRUE)/ sqrt(length(x[!is.na(x)]))}

load('temp_daily_Chitwan_cleaned.Rdata')

###############################################################################
# Max temp 95th percentiles
gt_95 <- ddply(temp, .(Year), summarize, num_days=sum(maxt_gt_95, na.rm=TRUE))
labeldata <- eqnfunc_slope(gt_95, 'num_days ~ order(Year)')
pct_95_plot <- ggplot(gt_95, aes(Year, num_days)) +
    geom_line() + xlab('Year') +
    ylab('Days max. temp. above 95th pct.') + 
    geom_smooth(method="lm", se=TRUE) +
    geom_text(aes(x=1980, y=35, label=labeldata), parse=TRUE, 
              colour='black', hjust=0, size=8)
lm(num_days ~ order(Year), data=gt_95)
pct_95_plot

###############################################################################
# Min temp 5th percentiles
lt_5 <- ddply(temp, .(Year), summarize, num_days=sum(mint_lt_5, na.rm=TRUE))
labeldata <- eqnfunc_slope(lt_5, 'num_days ~ order(Year)')
pct_5_plot <- ggplot(lt_5, aes(Year, num_days)) +
    geom_line() + xlab('Year') +
    ylab('Days min. temp. below 5th pct.') + 
    geom_smooth(method="lm", se=TRUE) +
    geom_text(aes(x=1985, y=35, label=labeldata), parse=TRUE, 
              colour='black', hjust=0, size=8)
lm(num_days ~ order(Year), data=lt_5)
pct_5_plot

###############################################################################
# Diurnal temperature range

# Save plots
grid_cols <- 2
grid_rows <- 2
png('temp_multiplot.png', width=PLOT_WIDTH*PLOT_DPI*grid_cols, 
    height=PLOT_HEIGHT*PLOT_DPI*grid_rows)
grid.arrange(ann_plot, pct_95_plot, rainy_days_plot, EP_frac_plot, max_5_day_plot, 
             monthly_temp_plot, ncol=2)
dev.off()
