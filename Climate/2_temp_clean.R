# for monsoon, winter, and 
# spring#############################################################################c
# Reads in and cleans the temperature data for three stations near Western 
# Chitwan.
###############################################################################

library(lubridate)
library(plyr)

source('0_utility_functions.R')

base_data_folder <-  'G:/Data/Nepal/Climate/Nepal_DHM/Temp/'
data_subfolders <- c('0706', '0902', '0927')

raw_min_temp <- c()
raw_max_temp <- c()
station_names <- c()
dates <- c()
for (data_subfolder in data_subfolders) {
    station_name <- paste('TA', data_subfolder, sep='')
    # Setup two vectors to code the months
    temp_files <- list.files(file.path(base_data_folder, data_subfolder))
    for (temp_file in temp_files) {
        raw_data <- read.table(file.path(base_data_folder, data_subfolder, 
                                         temp_file), skip=2, colClasses=c('integer', 
                                         'character', 'character'))
        # The filename includes the two digit year as the last two characters of 
        # the name - convert it to a 4 digit year depending on whether it starts 
        # with a zero or 1 (meaning in 21st century) or some other number (meaning 
        # in 20th century).
        year_2digit <- regmatches(temp_file, regexpr('[0-9]{2}$', temp_file))
        if (grepl('^(0|1)', year_2digit)) {
            file_year <- paste('20', year_2digit, sep='')
        } else {
            file_year <- paste('19', year_2digit, sep='')
        }
        # Column 1 of raw_data is the Julian day
        file_dates <- paste(file_year, raw_data[, 1])
        dates <- c(dates, file_dates)
        # Column 2 of raw_data is mm of temp
        raw_max_temp <- c(raw_max_temp, raw_data[, 2])
        raw_min_temp <- c(raw_min_temp, raw_data[, 3])
        station_names <- c(station_names, rep(station_name, length(file_dates)))
    }
}
dates <- as.Date(dates, '%Y %j')
temp <- data.frame(Date=dates, raw_min_temp, raw_max_temp, Station=station_names, stringsAsFactors=FALSE)

# Clean the data
temp$max_temp <- as.numeric(temp$raw_max_temp)
table(temp$raw_max_temp[is.na(temp$max_temp)])
# Recode data not available ('DNA') as NA
temp$max_temp[temp$raw_temp == 'DNA'] <- NA

temp$min_temp <- as.numeric(temp$raw_min_temp)
table(temp$raw_min_temp[is.na(temp$min_temp)])
# Recode data not available ('DNA') as NA
temp$min_temp[temp$raw_temp == 'DNA'] <- NA

temp$Year <- year(temp$Date)
temp$Month <- month(temp$Date)
temp$day <- day(temp$Date)

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

# There are 0s in the dataset for max temp and min_temp - these look like they 
# should actually be NAs
temp$min_temp[temp$min_temp == 0] <- NA
temp$max_temp[temp$max_temp == 0] <- NA

temp$Julian_Day <- as.numeric(format(temp$Date, "%j"))
# Assign pentads for consistency with CMAP: "The pentad dataset consists of 73 
# pentads per year with the 12th pentad covering Feb 25-Mar 1 whether or not 
# there is a leap year." See: http://1.usa.gov/18IObQd
temp$Pentad <- NA
leaps <- leap_year(temp$Year)
non_leaps <- !leap_year(temp$Year)
temp$Pentad[non_leaps] <- cut(temp$Julian_Day[non_leaps], 73)
# In leap years, the 12th pentad needs to cover 6 days, from day February 26th 
# to March 1s (days 56 - 61, inclusive)
temp$Pentad[leaps] <- cut(temp$Julian_Day[leaps],
                            c(seq(0, 56, by=5), seq(61, 366, by=5)))
temp$Pentad_Start_Day <- seq(1, 365, by=5)[temp$Pentad]

save(temp, file='temp_daily_All.Rdata')
write.csv(temp, file='temp_daily_All.csv', row.names=FALSE)

###############################################################################
# Perform more involved data cleaning

# Remove years with more than 10 days missing from the record
maxt_ann_missings <- aggregate(is.na(temp$max_temp),
                 by=list(Year=temp$Year, Station=temp$Station), sum, 
                 na.rm=TRUE)
names(maxt_ann_missings)[names(maxt_ann_missings) == 'x'] <- 'missings'
ggplot(maxt_ann_missings, aes(Year, missings, colour=Station)) +
geom_line() + xlab('Year') + ylab('Number of missing days (high temperature)')

mint_ann_missings <- aggregate(is.na(temp$min_temp),
                 by=list(Year=temp$Year, Station=temp$Station), sum, 
                 na.rm=TRUE)
names(mint_ann_missings)[names(mint_ann_missings) == 'x'] <- 'missings'
ggplot(mint_ann_missings, aes(Year, missings, colour=Station)) +
geom_line() + xlab('Year') + ylab('Number of missing days (low temperature)')

maxt_ann_missings_gt_15 <- maxt_ann_missings[maxt_ann_missings$missing > 15, ]
for (n in 1:nrow(maxt_ann_missings_gt_15)) {
    temp$max_temp[temp$Station == maxt_ann_missings_gt_15$Station[n] & temp$Year 
                  == maxt_ann_missings_gt_15$Year[n]] <- NA
}
mint_ann_missings_gt_15 <- mint_ann_missings[mint_ann_missings$missing > 15, ]
for (n in 1:nrow(mint_ann_missings_gt_15)) {
    temp$min_temp[temp$Station == mint_ann_missings_gt_15$Station[n] & temp$Year 
                  == mint_ann_missings_gt_15$Year[n]] <- NA
}

###############################################################################
# Calculate percentiles

# First calculate separate percentiles for each station for both max and min 
# temp
temp <- ddply(temp, .(Station), transform, 
              mint_lt_1=is_extreme(min_temp, 1, greater=FALSE),
              mint_lt_5=is_extreme(min_temp, 5, greater=FALSE),
              mint_lt_10=is_extreme(min_temp, 10, greater=FALSE),
              maxt_gt_90=is_extreme(max_temp, 5),
              maxt_gt_95=is_extreme(max_temp, 95),
              maxt_gt_99=is_extreme(max_temp, 99))

save(temp, file='temp_daily_ALL_cleaned.Rdata')
write.csv(temp, file='temp_daily_ALL_cleaned.csv', row.names=FALSE)

###############################################################################
# Limit further analysis to TA0902 - Rampur, 256m, as TA0706 - Dumkauli and 
# TA0927 - Bharatpur, 205m have almost no data.
# But there is only about 5 years of data, so not too useful.
temp <- temp[temp$Station %in% c('TA0902'), ]
temp$Station[temp$Station == 'TA0902'] <- 'Rampur'
#temp$Station[temp$Station == 'TA0927'] <- 'Bharatpur (205m)'
#temp$Station[temp$Station == 'TA0706'] <- 'Dumkauli (154m)'
temp$Station <- as.factor(temp$Station)
temp$Station <- relevel(temp$Station, 'Rampur')

# Also limit analysis to post 1980, since pre 1980 there is only data for 6 
# years between 1968 and 1980.
temp <- temp[temp$Year >= 1980, ]

save(temp, file='temp_daily_Chitwan_cleaned.Rdata')
write.csv(temp, file='temp_daily_Chitwan_cleaned.csv', row.names=FALSE)
