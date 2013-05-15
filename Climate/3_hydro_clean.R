###############################################################################
# Reads in the Chitwan area gauge station discharge data, and cleans it.
###############################################################################

library(lubridate)
library(plyr)

source('0_utility_functions.R')

base_data_folder <-  'G:/Data/Nepal/Climate/Nepal_DHM/Hydro'
data_subfolders <- c('420', '450', '460', '465', '470')

raw_discharge <- c()
station_names <- c()
dates <- c()
for (data_subfolder in data_subfolders) {
    # Setup two vectors to code the months
    data_folder <- file.path(base_data_folder, data_subfolder)
    discharge_files <- list.files(data_folder)
    discharge_files <- discharge_files[grepl('AQ[0-9]{3}_[0-9]{4}.(txt|TXT)', discharge_files)]
    for (discharge_file in discharge_files) {
        this_year <- regmatches(discharge_file, 
                                regexpr('(19[0-9]{2})|(2[0-1][0-9]{2})', 
                                        discharge_file))
        station_name <- regmatches(discharge_file, regexpr('(^AQ[0-9]{3})', 
                                        discharge_file))
        # Read in as a fixed width file since there are blanks in the table for 
        # months with less than 31 days
        raw_data <- read.fwf(file.path(data_folder, discharge_file),
                             widths=c(5, rep(7, 13)), skip=9, 
                             header=FALSE, n=32, fill=TRUE, colClasses='character')
        names(raw_data) <- gsub('( |[.])', '', raw_data[1, ])
        raw_data <- raw_data[-1, ]
        month_cols <- grep('(jan|feb|mar|apr|may|jun|jul|aug|sep|oct|nov|dec)', 
                           tolower(names(raw_data)))
        if (length(month_cols) != 12) {
            stop(paste('error processing months in ', discharge_file))
        }
        month_names <- gsub('( |[.])', '', names(raw_data)[month_cols])
        # Note that the below assumes that each month has 31 days (as is done in 
        # the input data file table) so there will be NA rows for months with less 
        # than 31 days.
        file_dates <- as.Date(paste(rep(month_names, each=31), raw_data$Day, 
                                    this_year), format='%b %d %Y')
        long_data <- data.frame(stack(raw_data[month_cols]), dates=file_dates)
        long_data <- long_data[!is.na(long_data$dates), ]
        if (!(nrow(long_data) %in% c(365, 366))) {
            stop(paste('error processing', discharge_file, '- number of days not equal to 365 or 366'))
        }
        dates <- c(dates, format(long_data$dates, '%Y %j'))
        raw_discharge <- c(raw_discharge, long_data$values)
        station_names <- c(station_names, rep(station_name, nrow(long_data)))
    }
}
dates <- as.Date(dates, '%Y %j')
discharge <- data.frame(Date=dates, raw_discharge, Station=station_names, stringsAsFactors=FALSE)

# Clean the data
discharge$discharge <- as.numeric(discharge$raw_discharge)
table(discharge$raw_discharge[is.na(discharge$discharge)])
discharge$Year <- year(discharge$Date)
discharge$Month <- month(discharge$Date)
discharge$Day <- day(discharge$Date)

# Add indicators for monsoon, winter, and spring
discharge$Season <- NA
discharge$Season[discharge$Month %in% c(6, 7, 8, 9)] <- 'Monsoon (JJAS)'
discharge$Season[discharge$Month %in% c(10, 11, 12, 1)] <- 'Winter (ONDJ)'
discharge$Season[discharge$Month %in% c(2, 3, 4, 5)] <- 'Spring (FMAM)'
discharge$Season <- factor(discharge$Season, levels=c('Spring (FMAM)', 'Monsoon (JJAS)', 'Winter (ONDJ)'))
# Add a variable for the starting year of each season (winter starts the year 
# prior for Jan and Feb months)
discharge$season_start_year <- discharge$Year
discharge$season_start_year[discharge$Month == 1] <- discharge$Year[discharge$Month == 1] - 1

# Add percentile indicators
discharge <- ddply(discharge, .(Station), transform, 
                   discharge_lt_1=is_extreme(discharge, 1, greater=FALSE),
                   discharge_lt_5=is_extreme(discharge, 5, greater=FALSE),
                   discharge_lt_10=is_extreme(discharge, 10, greater=FALSE),
                   discharge_gt_90=is_extreme(discharge, 90),
                   discharge_gt_95=is_extreme(discharge, 95),
                   discharge_gt_99=is_extreme(discharge, 99))

save(discharge, file='discharge_daily.Rdata')
write.csv(discharge, file='discharge_daily.csv', row.names=FALSE)
