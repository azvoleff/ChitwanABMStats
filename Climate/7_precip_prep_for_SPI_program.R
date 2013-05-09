library(ggplot2)
library(plyr)
library(spi)

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

load('precip_daily_Chitwan_cleaned.Rdata')

precip_monthly <- ddply(precip, .(Station, Year, Month), summarize,
                        total=round(sum(precip, na.rm=TRUE)))

precip_monthly_rampur <- precip_monthly[precip_monthly$Station == 'Rampur' &
                                        precip_monthly$Year >=1970 &
                                        precip_monthly$Year <=2010, ]
# Remove Station column
precip_monthly_rampur <- precip_monthly_rampur[-1]

# create and open the file connection
datafile <- file('precip_monthly_rampur.cor', open='wt')
writeLines('Rampur station monthly precip (mm)', con=datafile)
write.table(precip_monthly_rampur, file=datafile, row.names=FALSE, col.names=FALSE)
close(datafile)
