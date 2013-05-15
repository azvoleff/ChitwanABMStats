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
library(reshape2)

source('0_utility_functions.R')

stdErr <- function(x) {sd(x, na.rm=TRUE)/ sqrt(length(x[!is.na(x)]))}

load('temp_daily_Chitwan_cleaned.Rdata')

# Add indicators for monsoon, winter, and spring
temp$Season <- NA
temp$Season[temp$Month %in% c(6, 7, 8, 9)] <- 'Monsoon (JJAS)'
temp$Season[temp$Month %in% c(10, 11, 12, 1)] <- 'Winter (ONDJ)'
temp$Season[temp$Month %in% c(2, 3, 4, 5)] <- 'Spring (FMAM)'
temp$Season <- factor(temp$Season, levels=c('Spring (FMAM)', 'Monsoon (JJAS)', 'Winter (ONDJ)'))
# Add a variable for the starting year of each season (winter starts the year 
# prior for Jan and Feb months)
temp$season_start_year <- temp$Year
temp$season_start_year[temp$Month == 1] <- temp$Year[temp$Month == 1] - 1

table(temp$Year, is.na(temp$min_temp))

table(temp$Year, is.na(temp$max_temp))

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
png('temp_maxt_gt95.png', width=PLOT_WIDTH*PLOT_DPI, height=PLOT_HEIGHT*PLOT_DPI)
print(pct_95_plot)
dev.off()

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
png('temp_mint_lt5.png', width=PLOT_WIDTH*PLOT_DPI, height=PLOT_HEIGHT*PLOT_DPI)
print(pct_5_plot)
dev.off()

###############################################################################
# Seasonal temperatures
seasonal_stats <- ddply(temp, .(Season, season_start_year), summarize,
                       mean_mint=mean(min_temp, na.rm=TRUE),
                       mean_maxt=mean(max_temp, na.rm=TRUE),
                       mean_diurnal=mean(max_temp-min_temp, na.rm=TRUE))

# Eliminate data from years missing minimum temperature data. Also eliminate 
# data from 1979 since we only have one month for that winter season (January 
# 1980)
seasonal_stats[seasonal_stats$season_start_year %in%
               c(1979, 1984, 1988, 1989, 1998), ]$mean_mint <- NA
seasonal_stats[seasonal_stats$season_start_year %in%
               c(1979, 1984, 1988, 1989, 1998), ]$mean_diurnal <- NA
# Eliminate data from 1979 since we only have one month for that winter season 
# (January 1980), and from winter 2011, since we only have one month (D
seasonal_stats[seasonal_stats$season_start_year %in%
               c(1979), ]$mean_maxt<- NA

labeldata <- ddply(seasonal_stats, .(Season), eqnfunc_slope,
                    'mean_mint ~ order(season_start_year)')
seasonal_mint_plot <- ggplot(seasonal_stats, aes(season_start_year, mean_mint, 
                                                 colour=Season, 
                                                 linetype=Season)) +
    geom_line() + xlab('Time') +
    ylab(expression(paste("Mean daily min. temp. (", degree, C, ')', sep=''))) +
    geom_smooth(method="lm", se=TRUE) + theme(legend.position='bottom')
png('temp_seasonal_mint.png', width=PLOT_WIDTH*PLOT_DPI, height=PLOT_HEIGHT*PLOT_DPI)
print(seasonal_mint_plot)
dev.off()

labeldata <- ddply(seasonal_stats, .(Season), eqnfunc_slope,
                    'mean_maxt ~ order(season_start_year)')
seasonal_maxt_plot <- ggplot(seasonal_stats, aes(season_start_year, mean_maxt, 
                                                 colour=Season, 
                                                 linetype=Season)) +
    geom_line() + xlab('Time') +
    ylab(expression(paste("Mean daily max. temp. (", degree, C, ')', sep=''))) +
    geom_smooth(method="lm", se=TRUE) + theme(legend.position='bottom', legend.key.size=unit(1, "cm"))
png('temp_seasonal_maxt.png', width=PLOT_WIDTH*PLOT_DPI, height=PLOT_HEIGHT*PLOT_DPI)
print(seasonal_maxt_plot)
dev.off()

labeldata <- ddply(seasonal_stats, .(Season), eqnfunc_slope,
                    'mean_diurnal ~ order(season_start_year)')
seasonal_diurnal_plot <- ggplot(seasonal_stats, aes(season_start_year, 
                                                    mean_diurnal, 
                                                    colour=Season, 
                                                    linetype=Season)) +
    geom_line() + xlab('Time') +
    ylab(expression(paste('Mean daily diurnal temperature range (', degree, C, ')', sep=''))) +
    geom_smooth(method="lm", se=TRUE) + theme(legend.position='bottom', legend.key.size=unit(1, "cm"))
png('temp_seasonal_maxt.png', width=PLOT_WIDTH*PLOT_DPI, height=PLOT_HEIGHT*PLOT_DPI)
print(seasonal_maxt_plot)
dev.off()

labeldata <- ddply(seasonal_stats, .(Season), eqnfunc_slope,
                    'mean_diurnal ~ order(season_start_year)')
seasonal_diurnal_plot <- ggplot(seasonal_stats, aes(season_start_year, 
                                                    mean_diurnal, 
                                                    colour=Season, 
                                                    linetype=Season)) +
    geom_line() + xlab('Time') +
    
    geom_smooth(method="lm", se=TRUE)
png('temp_seasonal_diurnal.png', width=PLOT_WIDTH*PLOT_DPI, height=PLOT_HEIGHT*PLOT_DPI)
print(seasonal_diurnal_plot)
dev.off()

###############################################################################
# Faceted seasonal min/max t plots

# Below is from: http://bit.ly/17qgN4u - It allows setting a y scale with 
# integer breaks rather than decimals
library("scales")
integer_breaks <- function(n = 3, ...) {
  breaker <- pretty_breaks(n, ...)
  function(x) {
     breaks <- breaker(x)
     breaks[breaks == floor(breaks)]
  }
}

seasonal_melt <- melt(seasonal_stats, id.vars=c('Season', 'season_start_year'), 
                      measure.vars=c('mean_mint', 'mean_maxt'))
seasonal_melt$variable <- as.character(seasonal_melt$variable)
seasonal_melt$variable[seasonal_melt$variable == 'mean_mint'] <- 'Mean minimum'
seasonal_melt$variable[seasonal_melt$variable == 'mean_maxt'] <- 'Mean maximum'
x_breaks <- seq(1980, 2010, 15)
seasonal_melt_plot <- ggplot(seasonal_melt, aes(season_start_year, value, 
                                                colour=variable)) +
    geom_line() + xlab('Time') +
    facet_grid(variable ~ Season, scales='free_y', space='free_y') +
    ylab(expression(paste("Temperature (", degree, C, ')', sep=''))) +
    theme(legend.position='none') +
    scale_x_continuous(breaks=x_breaks) + 
    scale_y_continuous(breaks = integer_breaks()) +
    theme(axis.title.x=element_text(vjust=-0.5)) +
    theme(strip.text.x=element_text(lineheight=2.5)) +
    geom_smooth(method="lm", se=TRUE)
png('temp_seasonal_meltplot.png', width=PLOT_WIDTH*PLOT_DPI*2, height=PLOT_HEIGHT*PLOT_DPI)
print(seasonal_melt_plot)
dev.off()


###############################################################################
# Multiplots

g_legend<-function(a.gplot){
    tmp <- ggplot_gtable(ggplot_build(a.gplot))
    leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
    legend <- tmp$grobs[[leg]]
    legend
}

season_legend <- g_legend(seasonal_maxt_plot)
grid_cols <- 2
grid_rows <- 1
png('temp_seasonal_multiplot.png', width=PLOT_WIDTH*PLOT_DPI*grid_cols, 
    height=PLOT_HEIGHT*PLOT_DPI*grid_rows)
grid.arrange(arrangeGrob(seasonal_mint_plot + theme(legend.position='none'),
                         seasonal_maxt_plot + theme(legend.position='none'),
                         ncol=2),
             season_legend, ncol=1, heights=c(7/8,1/8))
dev.off()
