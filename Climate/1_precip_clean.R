###############################################################################
# Reads in and cleans the precip data for several Western Chitwan Valley 
# stations.
###############################################################################

library(lubridate)
library(ggplot2)
library(plyr)

source('0_utility_functions.R')

base_data_folder <-  'G:/Data/Nepal/Climate/Nepal_DHM/Precip/'
data_subfolders <- c('0704', '0706', '0902', '0903', '0920', '0925', '0927')

raw_precip <- c()
station_names <- c()
dates <- c()
for (data_subfolder in data_subfolders) {
    station_name <- paste('AS', data_subfolder, sep='')
    # Setup two vectors to code the months
    precip_files <- list.files(file.path(base_data_folder, data_subfolder))
    for (precip_file in precip_files) {
        raw_data <- read.table(file.path(base_data_folder, data_subfolder, 
                                         precip_file), colClasses=c("integer", 
                                         "character"))
        # The filename includes the two digit year as the last two characters of 
        # the name - convert it to a 4 digit year depending on whether it starts 
        # with a zero or 1 (meaning in 21st century) or some other number (meaning 
        # in 20th century).
        year_2digit <- regmatches(precip_file, regexpr('[0-9]{2}$', precip_file))
        if (grepl('^(0|1)', year_2digit)) {
            file_year <- paste('20', year_2digit, sep='')
        } else {
            file_year <- paste('19', year_2digit, sep='')
        }
        # Column 1 of raw_data is the Julian day
        file_dates <- paste(file_year, raw_data[, 1])
        dates <- c(dates, file_dates)
        # Column 2 of raw_data is mm of precip
        raw_precip <- c(raw_precip, raw_data[, 2])
        station_names <- c(station_names, rep(station_name, length(file_dates)))
    }
}
dates <- as.Date(dates, '%Y %j')
precip <- data.frame(Date=dates, raw_precip, Station=station_names, stringsAsFactors=FALSE)

# Clean the data
precip$precip <- as.numeric(precip$raw_precip)
table(precip$raw_precip[is.na(precip$precip)])
# Recode 'trace' as zero
precip$precip[precip$raw_precip == 'T'] <- 0
# Recode data not available ('DNA') as NA
precip$precip[precip$raw_precip == 'DNA'] <- NA
precip$Year <- year(precip$Date)
precip$Month <- month(precip$Date)
precip$Day <- day(precip$Date)

precip$Julian_Day <- as.numeric(format(precip$Date, "%j"))
# Assign pentads for consistency with CMAP: "The pentad dataset consists of 73 
# pentads per year with the 12th pentad covering Feb 25-Mar 1 whether or not 
# there is a leap year." See: http://1.usa.gov/18IObQd
precip$Pentad <- NA
leaps <- leap_year(precip$Year)
non_leaps <- !leap_year(precip$Year)
precip$Pentad[non_leaps] <- cut(precip$Julian_Day[non_leaps], 73)
# In leap years, the 12th pentad needs to cover 6 days, from day February 26th 
# to March 1s (days 56 - 61, inclusive)
precip$Pentad[leaps] <- cut(precip$Julian_Day[leaps],
                            c(seq(0, 56, by=5), seq(61, 366, by=5)))
precip$Pentad_Start_Day <- seq(1, 365, by=5)[precip$Pentad]

save(precip, file='precip_daily_ALL.Rdata')
write.csv(precip, file='precip_daily_ALL.csv', row.names=FALSE)

# And remove years with more than 10 days missing from the record
ann_missings <- aggregate(is.na(precip$precip),
                          by=list(Year=precip$Year, Station=precip$Station), sum, 
                          na.rm=TRUE)
names(ann_missings)[names(ann_missings) == 'x'] <- 'missings'
#ggplot(ann_missings, aes(Year, missings, colour=Station)) +
#geom_line() + xlab('Year') + ylab('Number of missing days')

ann_missings_gt_10 <- ann_missings[ann_missings$missing > 10, ]
for (n in 1:nrow(ann_missings_gt_10)) {
    precip$precip[precip$Station == ann_missings_gt_10$Station[n] & precip$Year 
                  == ann_missings_gt_10$Year[n]] <- NA
}

# Add separate percentiles indicators for each station
precip <- ddply(precip, .(Station), transform, 
              precip_gt_90=is_extreme(precip, 90, data_subset=(precip > 0)),
              precip_gt_95=is_extreme(precip, 95, data_subset=(precip > 0)),
              precip_gt_99=is_extreme(precip, 99, data_subset=(precip > 0)))

save(precip, file='precip_daily_ALL_cleaned.Rdata')
write.csv(precip, file='precip_daily_ALL_cleaned.csv', row.names=FALSE)

###############################################################################
# Limit further analysis to stations:
#   AS0902 - Rampur, 256m
#   AS0903 - Jhawani, 270m
#   AS0706 - Dumkauli, 154m
# Note that there is some data from Bharatpur:
#   AS0927 - Bharatpur, 205m
# But there is only about 5 years of data, so not too useful.
precip <- precip[precip$Station %in% c('AS0902', 'AS0903', 'AS0706'), ]
precip$Station[precip$Station == 'AS0902'] <- 'Rampur'
precip$Station[precip$Station == 'AS0903'] <- 'Jhawani'
precip$Station[precip$Station == 'AS0706'] <- 'Dumkauli'
precip$Station <- as.factor(precip$Station)
precip$Station <- relevel(precip$Station, 'Rampur')

save(precip, file='precip_daily_Chitwan_cleaned.Rdata')
write.csv(precip, file='precip_daily_Chitwan_cleaned.csv', row.names=FALSE)
