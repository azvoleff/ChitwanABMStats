library(raster)
library(lubridate)
library(ggplot2)
library(gridExtra)
library(plyr)
library(reshape2)

source('0_utility_functions.R')

source('C:/Users/azvoleff/Code/IDL/Chitwan_MODIS_SVIs/tts2df.R')
source('C:/Users/azvoleff/Code/IDL/Chitwan_MODIS_SVIs/ttsdf2raster.R')

stdErr <- function(x) {sd(x, na.rm=TRUE)/ sqrt(length(x[!is.na(x)]))}

lag_vector <- function(data_vec, k=1) {
    if (k > 0) {
        return(c(rep(NA, k), data_vec[1:(length(data_vec) - k)]))
    } else {
        return(c(data_vec[(1 + abs(k)):(length(data_vec))], rep(NA, abs(k))))
    }
}

load('discharge_daily.Rdata')
load('precip_daily_Chitwan_cleaned.Rdata')
load('temp_daily_Chitwan_cleaned.Rdata')
load('C:/Users/azvoleff/Code/IDL/Chitwan_MODIS_SVIs/EVI_monthly_mean_over_valley.Rdata')
load('precip_monthly_SPI_all_stations.Rdata')

unique(discharge$Station)
unique(precip$Station)
unique(temp$Station)
unique(SPI$Station)

discharge <- discharge[discharge$Station == 'AQ450', ]
precip <- precip[precip$Station == 'Rampur', ]
temp <- temp[temp$Station == 'Rampur', ]
SPI <- SPI[SPI$Station == 'Rampur', ]

filter_years <- 2
filter_size <- 365*filter_years

precip$mean_precip_24mth <- filter(precip$precip, rep(1/filter_size, filter_size), sides=1)*365
discharge$mean_discharge_24mth <- filter(discharge$discharge*60*60*24, rep(1/filter_size, filter_size), sides=1)*365

precip_monthly <- ddply(precip, .(Year, Month), summarize,
                        mean_precip=mean(precip, na.rm=TRUE),
                        mean_precip_24mth=mean(mean_precip_24mth, na.rm=TRUE))
discharge_monthly <- ddply(discharge, .(Year, Month), summarize,
                        mean_discharge=mean(discharge, na.rm=TRUE),
                        mean_discharge_24mth=mean(mean_discharge_24mth, na.rm=TRUE))
temp_monthly <- ddply(temp, .(Year, Month), summarize,
                        mean_maxt=mean(max_temp, na.rm=TRUE),
                        mean_mint=mean(min_temp, na.rm=TRUE))
precip_monthly$Date <- as.Date(paste(precip_monthly$Year,
                                     precip_monthly$Month, '15'), '%Y %m %d')
discharge_monthly$Date <- as.Date(paste(discharge_monthly$Year,
                                     discharge_monthly$Month, '15'), '%Y %m %d')
temp_monthly$Date <- as.Date(paste(temp_monthly$Year,
                                     temp_monthly$Month, '15'), '%Y %m %d')

# Limit series to 2001-2008 as discharge data is not available past 2008, and 
# EVI data is only available back to 2000
EVI_monthly <- EVI_monthly[EVI_monthly$Year >= 2000, ]

precip_monthly <- precip_monthly[precip_monthly$Date %in% EVI_monthly$Date, ]
discharge_monthly <- discharge_monthly[discharge_monthly$Date %in% EVI_monthly$Date, ]
temp_monthly <- temp_monthly[temp_monthly$Date %in% EVI_monthly$Date, ]
SPI <- SPI[SPI$Date %in% EVI_monthly$Date, ]

EVI_monthly <- merge(EVI_monthly, precip_monthly, all.x=TRUE)
EVI_monthly <- merge(EVI_monthly, discharge_monthly, all.x=TRUE)
EVI_monthly <- merge(EVI_monthly, temp_monthly, all.x=TRUE)
EVI_monthly <- merge(EVI_monthly, SPI, all.x=TRUE)
EVI_monthly <- EVI_monthly[order(EVI_monthly$Date), ]

EVI_monthly$Year <- year(EVI_monthly$Date)
EVI_monthly$Month <- month(EVI_monthly$Date)
EVI_monthly$lagged_precip <- lag_vector(EVI_monthly$mean_precip, )
EVI_monthly$lagged_discharge <- lag_vector(EVI_monthly$mean_discharge, 1)
EVI_monthly$lagged_mean_mint <- lag_vector(EVI_monthly$mean_mint, 1)
EVI_monthly$lagged_mean_maxt <- lag_vector(EVI_monthly$mean_maxt, 1)

lm_discharge <- lm(mean_EVI ~ mean_discharge, data=EVI_monthly)
summary(lm_discharge)
lm_discharge_lag <- lm(mean_EVI ~ lagged_discharge, data=EVI_monthly)
summary(lm_discharge_lag)

lm_precip <- lm(mean_EVI ~ mean_precip, data=EVI_monthly)
summary(lm_precip)
lm_precip_lag <- lm(mean_EVI ~ lagged_precip, data=EVI_monthly)
summary(lm_precip_lag)

lm_mean_mint <- lm(mean_EVI ~ mean_mint, data=EVI_monthly)
summary(lm_mean_mint)
lm_mean_mint_lag <- lm(mean_EVI ~ lagged_mean_mint, data=EVI_monthly)
summary(lm_mean_mint_lag)

lm_mean_maxt <- lm(mean_EVI ~ mean_maxt, data=EVI_monthly)
summary(lm_mean_maxt)
lm_mean_maxt_lag <- lm(mean_EVI ~ lagged_mean_maxt, data=EVI_monthly)
summary(lm_mean_maxt_lag)

lm_SPI_6 <- lm(mean_EVI_6mth ~ SPI_6, data=EVI_monthly)
summary(lm_SPI_6)

lm_SPI_12 <- lm(mean_EVI_12mth ~ SPI_12, data=EVI_monthly)
summary(lm_SPI_12)

lm_SPI_24 <- lm(mean_EVI_24mth ~ SPI_24, data=EVI_monthly)
summary(lm_SPI_24)

EVI_monthly_6mth_vs_SPI_plot <- ggplot(data=EVI_monthly) +
    geom_line(aes(Date, scale(mean_EVI_6mth), colour='6 month EVI', linetype='6 month EVI'), size=.75) +
    geom_line(aes(Date, scale(SPI_6), colour='6 month SPI', linetype='6 month SPI'), size=.75) +
    guides(colour=guide_legend(title='Legend'),
           linetype=guide_legend(title='Legend'))
png('EVI_monthly_6mth_vs_SPI.png', width=PLOT_WIDTH*PLOT_DPI, height=PLOT_HEIGHT*PLOT_DPI)
print(EVI_monthly_6mth_vs_SPI_plot)
dev.off()

EVI_monthly_12mth_vs_SPI_plot <- ggplot(data=EVI_monthly) +
    geom_line(aes(Date, scale(mean_EVI_12mth), colour='12 month EVI', linetype='12 month EVI'), size=.75) +
    geom_line(aes(Date, scale(SPI_12), colour='12 month SPI', linetype='12 month SPI'), size=.75) +
    guides(colour=guide_legend(title='Legend'),
           linetype=guide_legend(title='Legend'))
png('EVI_monthly_12mth_vs_SPI.png', width=PLOT_WIDTH*PLOT_DPI, height=PLOT_HEIGHT*PLOT_DPI)
print(EVI_monthly_12mth_vs_SPI_plot)
dev.off()

EVI_monthly_24mth_vs_SPI_plot <- ggplot(data=EVI_monthly) +
    geom_line(aes(Date, scale(mean_EVI_24mth), colour='24 month EVI', linetype='24 month EVI'), size=.75) +
    geom_line(aes(Date, scale(SPI_24), colour='24 month SPI', linetype='24 month SPI'), size=.75) +
    guides(colour=guide_legend(title='Legend'),
           linetype=guide_legend(title='Legend'))
png('EVI_monthly_24mth_vs_SPI.png', width=PLOT_WIDTH*PLOT_DPI, height=PLOT_HEIGHT*PLOT_DPI)
print(EVI_monthly_24mth_vs_SPI_plot)
dev.off()

###############################################################################
# Plot correlation with annual yields
###############################################################################
# Load the Chitwan annual yield data and merge it with the MODIS indicators
chitwan_yields <- read.csv('R:/Data/Nepal/Nepal_Census/Chitwan_Annual_AgProduction_From_Yearbooks.csv', skip=1)
EVI_annual <- ddply(EVI_monthly, .(Year), summarize,
                    SPI_12_Oct=SPI_6[10],
                    SPI_6_Oct=SPI_6[10],
                    EVI_6_Oct=mean_EVI_6mth[10],
                    SPI_6=mean(SPI_6),
                    EVI_6=mean(mean_EVI_6mth),
                    SPI_12=mean(SPI_12),
                    EVI_12=mean(mean_EVI_12mth))
merged_yields <- merge(EVI_annual, chitwan_yields, by.x='Year', by.y='Year_End')
# Load the seasonal indicators from the TIMESAT TTS files
load('C:/Users/azvoleff/Code/IDL/Chitwan_MODIS_SVIs/EVI_seasonal_indicators_mean_over_valley.Rdata')
valley_means$Year <- year(valley_means$Year)
merged_yields <- merge(merged_yields, valley_means)

#std_indic <- with(merged_yields, data.frame(EVI_12, Paddy, Maize))
std_indic <- with(merged_yields, data.frame(amp, Sinteg_mean, Sinteg_sum, 
                                            Linteg_mean, Linteg_sum, EVI_6_Oct, 
                                            EVI_12, SPI_6, SPI_12, Paddy, 
                                            Maize))
std_indic <- data.frame(scale(std_indic))
std_indic <- cbind(Year=merged_yields$Year, std_indic)
std_indic <- melt(std_indic, id.vars='Year')
std_indic$variable <- as.character(std_indic$variable)
std_indic$variable[std_indic$variable == 'Sinteg_sum'] <- 'Seasonal growth'
std_indic$variable[std_indic$variable == 'Linteg_sum'] <- 'Total growth'
std_indic$variable[std_indic$variable == 'amp'] <- 'Amplitude of season'
std_indic$variable[std_indic$variable == 'EVI_6_Oct'] <- 'May-Oct EVI'
std_indic$variable[std_indic$variable == 'EVI_12'] <- '12 month EVI'
std_indic$variable[std_indic$variable == 'SPI_6'] <- '6 month SPI'
std_indic$variable[std_indic$variable == 'SPI_12'] <- '12 month SPI'
std_indic$variable[std_indic$variable == 'Paddy'] <- 'Paddy yield'
std_indic$variable[std_indic$variable == 'Maize'] <- 'Maize yield'
std_indic$variable <- factor(std_indic$variable,
                             levels=c('Seasonal growth',
                                      'Total growth',
                                      'Amplitude of season',
                                      'May-Oct EVI',
                                      '12 month EVI',
                                      '6 month SPI',
                                      '12 month SPI',
                                      'Paddy yield',
                                      'Maize yield'))
std_indic$Date <- as.Date(paste(std_indic$Year, 1, 1, sep='/'))

EVI_annual_yields_plot <- ggplot(data=std_indic[std_indic$variable %in% c('Amplitude of season',
                                                                          'Paddy yield',
                                                                          'Maize yield'), ],
                                 aes(Date, value, colour=variable, linetype=variable)) +
    geom_line() + geom_point(size=3) +
    guides(colour=guide_legend(title='Legend'),
           linetype=guide_legend(title='Legend')) +
    ylab('Standardized value') +  theme(legend.position='bottom') + xlab('Year')
png('annual_yields_plot_EVI_amp.png', width=PLOT_WIDTH*PLOT_DPI, height=PLOT_HEIGHT*PLOT_DPI)
print(EVI_annual_yields_plot)
dev.off()

SPI_annual_yields_plot <- ggplot(data=std_indic[std_indic$variable %in% c('12 month SPI',
                                                                          'Paddy yield',
                                                                          'Maize yield'), ],
                                 aes(Date, value, colour=variable, linetype=variable)) +
    geom_line() + geom_point(size=3) +
    guides(colour=guide_legend(title='Legend'),
           linetype=guide_legend(title='Legend')) +
    ylab('Standardized value') +  theme(legend.position='bottom') + xlab('Year')
png('annual_yields_plot_SPI.png', width=PLOT_WIDTH*PLOT_DPI, height=PLOT_HEIGHT*PLOT_DPI)
print(SPI_annual_yields_plot)
dev.off()

Sinteg_sum_annual_yields_plot <- ggplot(data=std_indic[std_indic$variable %in% c('Seasonal growth',
                                                                          'Paddy yield',
                                                                          'Maize yield'), ],
                                 aes(Date, value, colour=variable, linetype=variable)) +
    geom_line() + geom_point(size=3) +
    guides(colour=guide_legend(title='Legend'),
           linetype=guide_legend(title='Legend')) +
    ylab('Standardized value') +  theme(legend.position='bottom') + xlab('Year')
png('annual_yields_plot_Sinteg_sum.png', width=PLOT_WIDTH*PLOT_DPI, height=PLOT_HEIGHT*PLOT_DPI)
print(Sinteg_sum_annual_yields_plot)
dev.off()

Linteg_sum_annual_yields_plot <- ggplot(data=std_indic[std_indic$variable %in% c('Total growth',
                                                                          'Paddy yield',
                                                                          'Maize yield'), ],
                                 aes(Date, value, colour=variable, linetype=variable)) +
    geom_line() + geom_point(size=3) +
    guides(colour=guide_legend(title='Legend'),
           linetype=guide_legend(title='Legend')) +
    ylab('Standardized value') +  theme(legend.position='bottom') + xlab('Year')
png('annual_yields_plot_Linteg_sum.png', width=PLOT_WIDTH*PLOT_DPI, height=PLOT_HEIGHT*PLOT_DPI)
print(Linteg_sum_annual_yields_plot)
dev.off()

with(merged_yields, cor.test(Sinteg_sum, Maize, use='complete.obs'))
with(merged_yields, cor.test(Sinteg_sum, Paddy, use='complete.obs'))

with(merged_yields[!(merged_yields$Year %in% c(2003, 2007)), ], cor.test(Sinteg_sum, Paddy, use='complete.obs'))

cor.test(merged_yields$Linteg_sum, merged_yields$Maize, use='complete.obs')
cor.test(merged_yields$Linteg_sum, merged_yields$Paddy, use='complete.obs')

cor.test(merged_yields$amp, merged_yields$Maize, use='complete.obs')
cor.test(merged_yields$amp, merged_yields$Paddy, use='complete.obs')

cor.test(merged_yields$SPI_12_Oct, merged_yields$Maize, use='complete.obs')
cor.test(merged_yields$SPI_12_Oct, merged_yields$Paddy, use='complete.obs')

cor.test(merged_yields$EVI_6_Oct, merged_yields$Maize, use='complete.obs')
cor.test(merged_yields$EVI_6_Oct, merged_yields$Paddy, use='complete.obs')

library(boot)
boot_calc_cor <- function(data, k) cor(data[k,], use='complete.obs')[1,2]
boot_cor_paddy <- boot(data=with(merged_yields, cbind(Sinteg_sum, Paddy)), 
                       statistic=boot_calc_cor, R=10000)
boot_cor_paddy

boot_cor_maize <- boot(data=with(merged_yields, cbind(Sinteg_sum, Maize)), 
                       statistic=boot_calc_cor, R=10000)
boot_cor_maize
boot.ci(boot_cor_maize)

ccf(EVI_monthly$mean_EVI, EVI_monthly$mean_discharge)

ccf(EVI_monthly$mean_EVI, EVI_monthly$mean_precip)

ccf(EVI_monthly$mean_EVI, EVI_monthly$SPI_24)

with(EVI_monthly[!is.na(EVI_monthly$mean_EVI_24mth), ], ccf(mean_EVI_24mth, mean_precip_24mth))

with(EVI_monthly[!is.na(EVI_monthly$mean_EVI_24mth), ], ccf(mean_EVI_24mth, mean_discharge_24mth))

################################################################################
# Make correlation maps
################################################################################

# Setup vector of MODIS dates
days <- seq(1, 365, 16)
years <- rep(seq(2000, 2011), each=length(days))
EVI_dates <- as.Date(paste(years, days), '%Y %j')
EVI_dates <- EVI_dates[EVI_dates >= as.Date('2000 01', '%Y %j')]
EVI_dates <- EVI_dates[EVI_dates <= as.Date('2011 353', '%Y %j')]

precip$Julian_Day <- as.numeric(format(precip$Date, '%j'))
precip$Period <- cut(precip$Julian_Day, seq(1, 370, 16), right=FALSE)
period_precip <- ddply(precip, .(Year, Period), summarize,
                       start_Julian_Day=Julian_Day[1], Season=Season[1],
                       total=sum(precip, na.rm=TRUE),
                       mean_precip_24mth=mean(mean_precip_24mth, na.rm=TRUE))
period_precip$Date <- as.Date(paste(period_precip$Year, period_precip$start_Julian_Day), '%Y %j')
period_precip <- period_precip[period_precip$Date %in% EVI_dates, ]

discharge$Julian_Day <- as.numeric(format(discharge$Date, '%j'))
discharge$Period <- cut(discharge$Julian_Day, seq(1, 370, 16), right=FALSE)
period_discharge <- ddply(discharge, .(Year, Period), summarize,
                       start_Julian_Day=Julian_Day[1], Season=Season[1],
                       total=sum(discharge, na.rm=TRUE),
                       mean_discharge_24mth=mean(mean_discharge_24mth, na.rm=TRUE))
period_discharge$Date <- as.Date(paste(period_discharge$Year, period_discharge$start_Julian_Day), '%Y %j')
period_discharge <- period_discharge[period_discharge$Date %in% EVI_dates, ]

# Load EVI data
tts_file_name <- 'R:/Data/Nepal/Imagery/MODIS/MOD13Q1_Chitwan_Cropped/Chitwan_MOD13Q1_EVI_Full_Series_Cropped_Expanded_fit.tts'
tts_df <- tts2df(tts_file_name)
base_image_file <- 'R:/Data/Nepal/Imagery/MODIS/MOD13Q1_Chitwan_Cropped/2000001_MOD13Q1_EVI_scaled_flt16.envi'
CVFS_area_mask <- raster('R:/Data/Nepal/Imagery/MODIS/AOIs/CVFS_Study_Area_mask_float.img')
ttsraster <- ttsdf2raster(tts_df, base_image_file)
#ttsraster <- setValues(ttsraster, getValues(ttsraster) * 
#                       getValues(CVFS_area_mask))

# Make a ttsraster of two year rolling mean EVI
filter_years <- 2
filter_size <- 23*filter_years
# Remember that cols 1 and 2 are the row and col numbers
tts_df_24mth_rollmean <- data.frame(t(apply(tts_df[, 3:ncol(tts_df)], 1, function(x) {filter(x, rep(1/filter_size, filter_size), sides=1)})))
tts_df_24mth_rollmean <- cbind(row=tts_df$row, col=tts_df$col, tts_df_24mth_rollmean)
names(tts_df_24mth_rollmean) <- gsub('X', 't', names(tts_df_24mth_rollmean))
ttsraster_rollmean <- ttsdf2raster(tts_df_24mth_rollmean, base_image_file)

################################################################################
# First make simple correlation maps

# Load CVFS area polygon so it can be shown on the image
cvfs_area <- readOGR(dsn="R:/Data/Nepal/GIS/ChitwanDistrict", layer="CVFS_Study_Area")
# Need to transform CVFS polygon to WGS84 since it is in UTM
cvfs_area  <- spTransform(cvfs_area, CRS("+init=epsg:4326"))
cvfs_area@data$id <- rownames(cvfs_area@data)
cvfs_area.points <- fortify(cvfs_area, region="id")
cvfs_area.df <- join(cvfs_area.points, cvfs_area@data, by="id")

plot_cor_df <- function(cor_df, png_file) {
    theme_set(theme_bw(base_size=8))
    ggplot(cor_df) +
        geom_raster(aes(x, y, fill=cor)) + coord_fixed() + 
        theme(axis.text.x=element_blank(), axis.text.y=element_blank(),
              axis.title.x=element_blank(), axis.title.y=element_blank(),
              panel.background=element_blank(), panel.border=element_blank(),
              panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
              plot.background=element_blank(), axis.ticks=element_blank(),
              plot.margin=unit(c(.1, .1, .1, .1), 'cm')) +
        scale_fill_gradient2(midpoint=0, low='blue', mid='yellow', high='red',
                             limits=c(-1, 1), name='Correlation') +
        guides(fill=guide_colorbar(barwidth=.5, barheight=4, ticks=FALSE)) +
        geom_path(data=cvfs_area.df, aes(long, lat), color='black', size=.5, alpha=.7)
    ggsave(png_file, width=PLOT_WIDTH*2, height=PLOT_HEIGHT*.5*2, dpi=PLOT_DPI)
}

# Correlation of EVI with precip - have to end at period 253 (last period in 
# 2010) as there is no good precip data for 2011
EVI_cor_precip <- calc(subset(ttsraster, seq(1, 253)),
                       fun=function(x){cor(x, period_precip$total)})
writeRaster(EVI_cor_precip, 'EVI_cor_precip.envi', format='ENVI', overwrite=TRUE)
EVI_cor_precip_df <- data.frame(x=coordinates(EVI_cor_precip)[, 1],
                                   y=coordinates(EVI_cor_precip)[, 2],
                                   cor=getValues(EVI_cor_precip))
EVI_cor_precip_df <- EVI_cor_precip_df[!is.na(EVI_cor_precip_df$cor), ]
plot_cor_df(EVI_cor_precip_df, 'EVI_cor_precip_plot.png')

# Have to start at period 46 since the 2 year filter on the EVI leads to NAs 
# for the first 45 periods.
EVI_cor_precip_24mth <- calc(subset(ttsraster_rollmean, c(46:253)),
                           fun=function(x){cor(x, period_precip$mean_precip_24mth[46:nrow(period_precip)])})
writeRaster(EVI_cor_precip_24mth, 'EVI_cor_precip_24mth.envi', format='ENVI', overwrite=TRUE)
EVI_cor_precip_24mth_df <- data.frame(x=coordinates(EVI_cor_precip_24mth)[, 1],
                                   y=coordinates(EVI_cor_precip_24mth)[, 2],
                                   cor=getValues(EVI_cor_precip_24mth))
EVI_cor_precip_24mth_df <- EVI_cor_precip_24mth_df[!is.na(EVI_cor_precip_24mth_df$cor), ]
plot_cor_df(EVI_cor_precip_24mth_df, 'EVI_cor_precip_24mth_plot.png')

# Correlation of EVI with discharge mean
EVI_cor_discharge <- calc(subset(ttsraster, seq(1, 207)), 
                                         fun=function(x){cor(x, period_discharge$total)})
writeRaster(EVI_cor_discharge, 'EVI_cor_discharge.envi', format='ENVI', overwrite=TRUE)
EVI_cor_discharge_df <- data.frame(x=coordinates(EVI_cor_discharge)[, 1],
                                   y=coordinates(EVI_cor_discharge)[, 2],
                                   cor=getValues(EVI_cor_discharge))
EVI_cor_discharge_df <- EVI_cor_discharge_df[!is.na(EVI_cor_discharge_df$cor), ]
plot_cor_df(EVI_cor_discharge_df, 'EVI_cor_discharge_plot.png')

EVI_cor_discharge_24mth <- calc(subset(ttsraster_rollmean, c(46:207)),
                           fun=function(x){cor(x, period_discharge$mean_discharge_24mth[46:207])})
writeRaster(EVI_cor_discharge_24mth, 'EVI_cor_discharge_24mth.envi', format='ENVI', overwrite=TRUE)
EVI_cor_discharge_24mth_df <- data.frame(x=coordinates(EVI_cor_discharge_24mth)[, 1],
                                   y=coordinates(EVI_cor_discharge_24mth)[, 2],
                                   cor=getValues(EVI_cor_discharge_24mth))
EVI_cor_discharge_24mth_df <- EVI_cor_discharge_24mth_df[!is.na(EVI_cor_discharge_24mth_df$cor), ]
plot_cor_df(EVI_cor_discharge_24mth_df, 'EVI_cor_discharge_24mth_plot.png')

###############################################################################
# TODO: Idea - plot mean anomaly

################################################################################
# Now make correlation maps for the winter, spring, and summer seasons for 
# discharge
winter_season <- which(period_discharge$Season == 'Winter (ONDJ)')
spring_season <- which(period_discharge$Season == 'Spring (FMAM)')
monsoon_season <- which(period_discharge$Season == 'Monsoon (JJAS)')

# Winter
EVI_cor_discharge_winter <- calc(subset(ttsraster, winter_season), 
                                        fun=function(x){cor(x, period_discharge$total[winter_season])})
writeRaster(EVI_cor_discharge_winter, 'EVI_cor_discharge_winter.envi', format='ENVI', overwrite=TRUE)
EVI_cor_discharge_winter_df <- data.frame(x=coordinates(EVI_cor_discharge_winter)[, 1],
                                   y=coordinates(EVI_cor_discharge_winter)[, 2],
                                   cor=getValues(EVI_cor_discharge_winter))
EVI_cor_discharge_winter_df <- EVI_cor_discharge_winter_df[!is.na(EVI_cor_discharge_winter_df$cor), ]
plot_cor_df(EVI_cor_discharge_winter_df, 'EVI_cor_discharge_winter_plot.png')

# Spring
EVI_cor_discharge_spring <- calc(subset(ttsraster, spring_season), 
                                        fun=function(x){cor(x, period_discharge$total[spring_season])})
writeRaster(EVI_cor_discharge_spring, 'EVI_cor_discharge_spring.envi', format='ENVI', overwrite=TRUE)
EVI_cor_discharge_spring_df <- data.frame(x=coordinates(EVI_cor_discharge_spring)[, 1],
                                   y=coordinates(EVI_cor_discharge_spring)[, 2],
                                   cor=getValues(EVI_cor_discharge_spring))
EVI_cor_discharge_spring_df <- EVI_cor_discharge_spring_df[!is.na(EVI_cor_discharge_spring_df$cor), ]
plot_cor_df(EVI_cor_discharge_spring_df, 'EVI_cor_discharge_spring_plot.png')

# Monsoon
EVI_cor_discharge_monsoon <- calc(subset(ttsraster, monsoon_season), 
                                        fun=function(x){cor(x, period_discharge$total[monsoon_season])})
writeRaster(EVI_cor_discharge_monsoon, 'EVI_cor_discharge_monsoon.envi', format='ENVI', overwrite=TRUE)
EVI_cor_discharge_monsoon_df <- data.frame(x=coordinates(EVI_cor_discharge_monsoon)[, 1],
                                   y=coordinates(EVI_cor_discharge_monsoon)[, 2],
                                   cor=getValues(EVI_cor_discharge_monsoon))
EVI_cor_discharge_monsoon_df <- EVI_cor_discharge_monsoon_df[!is.na(EVI_cor_discharge_monsoon_df$cor), ]
plot_cor_df(EVI_cor_discharge_monsoon_df, 'EVI_cor_discharge_monsoon_plot.png')

################################################################################
# Now make correlation maps for the winter, spring, and summer seasons for 
# precip
# Winter
winter_season <- which(period_precip$Season == 'Winter (ONDJ)')
spring_season <- which(period_precip$Season == 'Spring (FMAM)')
monsoon_season <- which(period_precip$Season == 'Monsoon (JJAS)')

EVI_cor_precip_winter <- calc(subset(ttsraster, winter_season), 
                                        fun=function(x){cor(x, period_precip$total[winter_season])})
writeRaster(EVI_cor_precip_winter, 'EVI_cor_precip_winter.envi', format='ENVI', overwrite=TRUE)
EVI_cor_precip_winter_df <- data.frame(x=coordinates(EVI_cor_precip_winter)[, 1],
                                   y=coordinates(EVI_cor_precip_winter)[, 2],
                                   cor=getValues(EVI_cor_precip_winter))
EVI_cor_precip_winter_df <- EVI_cor_precip_winter_df[!is.na(EVI_cor_precip_winter_df$cor), ]
plot_cor_df(EVI_cor_precip_winter_df, 'EVI_cor_precip_winter_plot.png')

# Spring
EVI_cor_precip_spring <- calc(subset(ttsraster, spring_season), 
                                        fun=function(x){cor(x, period_precip$total[spring_season])})
writeRaster(EVI_cor_precip_spring, 'EVI_cor_precip_spring.envi', format='ENVI', overwrite=TRUE)
EVI_cor_precip_spring_df <- data.frame(x=coordinates(EVI_cor_precip_spring)[, 1],
                                   y=coordinates(EVI_cor_precip_spring)[, 2],
                                   cor=getValues(EVI_cor_precip_spring))
EVI_cor_precip_spring_df <- EVI_cor_precip_spring_df[!is.na(EVI_cor_precip_spring_df$cor), ]
plot_cor_df(EVI_cor_precip_spring_df, 'EVI_cor_precip_spring_plot.png')

# Monsoon
EVI_cor_precip_monsoon <- calc(subset(ttsraster, monsoon_season), 
                                        fun=function(x){cor(x, period_precip$total[monsoon_season])})
writeRaster(EVI_cor_precip_monsoon, 'EVI_cor_precip_monsoon.envi', format='ENVI', overwrite=TRUE)
EVI_cor_precip_monsoon_df <- data.frame(x=coordinates(EVI_cor_precip_monsoon)[, 1],
                                   y=coordinates(EVI_cor_precip_monsoon)[, 2],
                                   cor=getValues(EVI_cor_precip_monsoon))
EVI_cor_precip_monsoon_df <- EVI_cor_precip_monsoon_df[!is.na(EVI_cor_precip_monsoon_df$cor), ]
plot_cor_df(EVI_cor_precip_monsoon_df, 'EVI_cor_precip_monsoon_plot.png')
