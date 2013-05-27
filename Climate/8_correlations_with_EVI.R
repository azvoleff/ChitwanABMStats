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

precip$mean_precip_2yr <- filter(precip$precip, rep(1/filter_size, filter_size), sides=1)*365
discharge$mean_discharge_2yr <- filter(discharge$discharge*60*60*24, rep(1/filter_size, filter_size), sides=1)*365

precip_monthly <- ddply(precip, .(Year, Month), summarize,
                        mean_precip=mean(precip, na.rm=TRUE),
                        mean_precip_2yr=mean(mean_precip_2yr, na.rm=TRUE))
discharge_monthly <- ddply(discharge, .(Year, Month), summarize,
                        mean_discharge=mean(discharge, na.rm=TRUE),
                        mean_discharge_2yr=mean(mean_discharge_2yr, na.rm=TRUE))
temp_monthly <- ddply(temp, .(Year, Month), summarize,
                        mean_maxt=mean(max_temp, na.rm=TRUE),
                        mean_mint=mean(min_temp, na.rm=TRUE))
precip_monthly$Date <- as.Date(paste(precip_monthly$Year,
                                     precip_monthly$Month, '15'), '%Y %m %d')
discharge_monthly$Date <- as.Date(paste(discharge_monthly$Year,
                                     discharge_monthly$Month, '15'), '%Y %m %d')
temp_monthly$Date <- as.Date(paste(temp_monthly$Year,
                                     temp_monthly$Month, '15'), '%Y %m %d')

# Standardize the 2 year EVI
EVI_monthly$mean_EVI_2yr_std <- with(EVI_monthly,
                                     (mean_EVI_2yr - mean(mean_EVI_2yr, na.rm=TRUE)) /
                                      sd(mean_EVI_2yr, na.rm=TRUE))

# Limit series to 2001-2008 as discharge data is not available past 2008, and 
# EVI data is only available back to 2000
EVI_monthly <- EVI_monthly[EVI_monthly$Year >= 2000 & EVI_monthly$Year <= 2008, ]

precip_monthly <- precip_monthly[precip_monthly$Date %in% EVI_monthly$Date, ]
discharge_monthly <- discharge_monthly[discharge_monthly$Date %in% EVI_monthly$Date, ]
temp_monthly <- temp_monthly[temp_monthly$Date %in% EVI_monthly$Date, ]
SPI <- SPI[SPI$Date %in% EVI_monthly$Date, ]

model_vars <- data.frame(Date=EVI_monthly$Date,
                         mean_EVI=EVI_monthly$mean_EVI,
                         mean_EVI_2yr=EVI_monthly$mean_EVI_2yr,
                         mean_EVI_2yr_std=EVI_monthly$mean_EVI_2yr_std,
                         mean_precip=precip_monthly$mean_precip,
                         mean_precip_2yr=precip_monthly$mean_precip_2yr,
                         mean_discharge=discharge_monthly$mean_discharge,
                         mean_discharge_2yr=discharge_monthly$mean_discharge_2yr,
                         mean_mint=temp_monthly$mean_mint,
                         mean_maxt=temp_monthly$mean_maxt,
                         SPI_6=SPI$SPI_6,
                         SPI_12=SPI$SPI_12,
                         SPI_24=SPI$SPI_24)
model_vars$Year <- year(model_vars$Date)
model_vars$Month <- month(model_vars$Date)
model_vars$lagged_precip <- lag_vector(model_vars$mean_precip, )
model_vars$lagged_discharge <- lag_vector(model_vars$mean_discharge, 1)
model_vars$lagged_mean_mint <- lag_vector(model_vars$mean_mint, 1)
model_vars$lagged_mean_maxt <- lag_vector(model_vars$mean_maxt, 1)
model_vars_melt <- melt(model_vars, id.vars='Date')

lm_discharge <- lm(mean_EVI ~ mean_discharge, data=model_vars)
summary(lm_discharge)
lm_discharge_lag <- lm(mean_EVI ~ lagged_discharge, data=model_vars)
summary(lm_discharge_lag)

lm_precip <- lm(mean_EVI ~ mean_precip, data=model_vars)
summary(lm_precip)
lm_precip_lag <- lm(mean_EVI ~ lagged_precip, data=model_vars)
summary(lm_precip_lag)

lm_mean_mint <- lm(mean_EVI ~ mean_mint, data=model_vars)
summary(lm_mean_mint)
lm_mean_mint_lag <- lm(mean_EVI ~ lagged_mean_mint, data=model_vars)
summary(lm_mean_mint_lag)

lm_mean_maxt <- lm(mean_EVI ~ mean_maxt, data=model_vars)
summary(lm_mean_maxt)
lm_mean_maxt_lag <- lm(mean_EVI ~ lagged_mean_maxt, data=model_vars)
summary(lm_mean_maxt_lag)

lm_SPI_12 <- lm(mean_EVI_2yr ~ SPI_24, data=model_vars)
summary(lm_SPI_24)

lm_SPI_24 <- lm(mean_EVI_2yr ~ SPI_24, data=model_vars)
summary(lm_SPI_24)

qplot(Date, SPI_24, colour=as.factor(Year), data=model_vars)

qplot(SPI_24, mean_EVI_2yr, colour=as.factor(Year), data=model_vars)

ggplot(model_vars_melt[model_vars_melt$variable %in% c('mean_EVI_2yr', 'SPI_24'), ]) +
    geom_line(aes(Date, value)) + facet_grid(variable ~ ., scales='free')

ggplot(model_vars_melt[model_vars_melt$variable %in% c('mean_EVI_2yr_std', 'SPI_24'), ]) +
    geom_line(aes(Date, value)) + facet_grid(variable ~ ., scales='free')

ccf(model_vars$mean_EVI, model_vars$mean_discharge)

ccf(model_vars$mean_EVI, model_vars$mean_precip)

ccf(model_vars$mean_EVI, model_vars$SPI_24)

with(model_vars[!is.na(model_vars$mean_EVI_2yr), ], ccf(mean_EVI_2yr, mean_precip_2yr))

with(model_vars[!is.na(model_vars$mean_EVI_2yr), ], ccf(mean_EVI_2yr, mean_discharge_2yr))

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
                       mean_precip_2yr=mean(mean_precip_2yr, na.rm=TRUE))
period_precip$Date <- as.Date(paste(period_precip$Year, period_precip$start_Julian_Day), '%Y %j')
period_precip <- period_precip[period_precip$Date %in% EVI_dates, ]

discharge$Julian_Day <- as.numeric(format(discharge$Date, '%j'))
discharge$Period <- cut(discharge$Julian_Day, seq(1, 370, 16), right=FALSE)
period_discharge <- ddply(discharge, .(Year, Period), summarize,
                       start_Julian_Day=Julian_Day[1], Season=Season[1],
                       total=sum(discharge, na.rm=TRUE),
                       mean_discharge_2yr=mean(mean_discharge_2yr, na.rm=TRUE))
period_discharge$Date <- as.Date(paste(period_discharge$Year, period_discharge$start_Julian_Day), '%Y %j')
period_discharge <- period_discharge[period_discharge$Date %in% EVI_dates, ]

# Load EVI data
tts_file_name <- 'G:/Data/Nepal/Imagery/MODIS/MOD13Q1_Chitwan_Cropped/Chitwan_MOD13Q1_EVI_Full_Series_Cropped_Expanded_fit.tts'
tts_df <- tts2df(tts_file_name)
base_image_file <- 'G:/Data/Nepal/Imagery/MODIS/MOD13Q1_Chitwan_Cropped/2000001_MOD13Q1_EVI_scaled_flt16.envi'
CVFS_area_mask <- raster('G:/Data/Nepal/Imagery/MODIS/AOIs/CVFS_Study_Area_mask_float.img')
ttsraster <- ttsdf2raster(tts_df, base_image_file)
#ttsraster <- setValues(ttsraster, getValues(ttsraster) * 
#                       getValues(CVFS_area_mask))

# Make a ttsraster of two year rolling mean EVI
filter_years <- 2
filter_size <- 23*filter_years
# Remember that cols 1 and 2 are the row and col numbers
tts_df_2yr_rollmean <- data.frame(t(apply(tts_df[, 3:ncol(tts_df)], 1, function(x) {filter(x, rep(1/filter_size, filter_size), sides=1)})))
tts_df_2yr_rollmean <- cbind(row=tts_df$row, col=tts_df$col, tts_df_2yr_rollmean)
names(tts_df_2yr_rollmean) <- gsub('X', 't', names(tts_df_2yr_rollmean))
ttsraster_rollmean <- ttsdf2raster(tts_df_2yr_rollmean, base_image_file)

################################################################################
# First make simple correlation maps

# Load CVFS area polygon so it can be shown on the image
cvfs_area <- readOGR(dsn="G:/Data/Nepal/GIS/ChitwanDistrict", layer="CVFS_Study_Area")
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
        geom_path(data=cvfs_area.df, aes(long, lat), color='black', size=.5, alpha=.5)
    ggsave(png_file, width=PLOT_WIDTH*2, height=PLOT_HEIGHT*.5*2, dpi=PLOT_DPI)
}

# Correlation of EVI with precip - have to end at period 253 (last period in 
# 2010) as there is no good precip data for 2011
EVI_cor_precip <- calc(ttsraster, seq(1, 253), fun=function(x){cor(x, period_precip$total)})
writeRaster(EVI_cor_precip, 'EVI_cor_precip.envi', format='ENVI', overwrite=TRUE)
EVI_cor_precip_df <- data.frame(x=coordinates(EVI_cor_precip)[, 1],
                                   y=coordinates(EVI_cor_precip)[, 2],
                                   cor=getValues(EVI_cor_precip))
EVI_cor_precip_df <- EVI_cor_precip_df[!is.na(EVI_cor_precip_df$cor), ]
plot_cor_df(EVI_cor_precip_df, 'EVI_cor_precip_plot.png')

# Have to start at period 46 since the 2 year filter on the EVI leads to NAs 
# for the first 45 periods.
EVI_cor_precip_2yr <- calc(subset(ttsraster_rollmean, c(46:nlayers(ttsraster_rollmean))),
                           fun=function(x){cor(x, period_precip$mean_precip_2yr[46:nrow(period_precip)])})
writeRaster(EVI_cor_precip_2yr, 'EVI_cor_precip_2yr.envi', format='ENVI', overwrite=TRUE)
EVI_cor_precip_2yr_df <- data.frame(x=coordinates(EVI_cor_precip_2yr)[, 1],
                                   y=coordinates(EVI_cor_precip_2yr)[, 2],
                                   cor=getValues(EVI_cor_precip_2yr))
EVI_cor_precip_2yr_df <- EVI_cor_precip_2yr_df[!is.na(EVI_cor_precip_2yr_df$cor), ]
plot_cor_df(EVI_cor_precip_2yr_df, 'EVI_cor_precip_2yr_plot.png')

# Correlation of EVI with discharge mean
EVI_cor_discharge <- calc(subset(ttsraster, seq(1, 207)), 
                                         fun=function(x){cor(x, period_discharge$total)})
writeRaster(EVI_cor_discharge, 'EVI_cor_discharge.envi', format='ENVI', overwrite=TRUE)
EVI_cor_discharge_df <- data.frame(x=coordinates(EVI_cor_discharge)[, 1],
                                   y=coordinates(EVI_cor_discharge)[, 2],
                                   cor=getValues(EVI_cor_discharge))
EVI_cor_discharge_df <- EVI_cor_discharge_df[!is.na(EVI_cor_discharge_df$cor), ]
plot_cor_df(EVI_cor_discharge_df, 'EVI_cor_discharge_plot.png')

EVI_cor_discharge_2yr <- calc(subset(ttsraster_rollmean, c(46:207)),
                           fun=function(x){cor(x, period_discharge$mean_discharge_2yr[46:207])})
writeRaster(EVI_cor_discharge_2yr, 'EVI_cor_discharge_2yr.envi', format='ENVI', overwrite=TRUE)
EVI_cor_discharge_2yr_df <- data.frame(x=coordinates(EVI_cor_discharge_2yr)[, 1],
                                   y=coordinates(EVI_cor_discharge_2yr)[, 2],
                                   cor=getValues(EVI_cor_discharge_2yr))
EVI_cor_discharge_2yr_df <- EVI_cor_discharge_2yr_df[!is.na(EVI_cor_discharge_2yr_df$cor), ]
plot_cor_df(EVI_cor_discharge_2yr_df, 'EVI_cor_discharge_2yr_plot.png')

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
