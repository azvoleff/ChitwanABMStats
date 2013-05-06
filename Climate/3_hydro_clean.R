###############################################################################
# Reads in the Devghat gauge discharge data, and makes some preliminary plots.
###############################################################################

library(lubridate)
library(ggplot2)
library(plyr)

source('0_utility_functions.R')

theme_set(theme_grey(base_size=30))
update_geom_defaults("smooth", aes(size=1))
update_geom_defaults("line", aes(size=1))
PLOT_WIDTH = (8.5 - 1.25 - 1.5) / 2
PLOT_HEIGHT = 7 / 3
PLOT_DPI = 300

data_folder <-  'G:/Data/Nepal/Climate/Nepal_DHM/Hydro/450'
start_date <- as.Date('1963/01/01')
end_date <- as.Date('2008/12/31')

discharge <- data.frame(date=seq(start_date, end_date, by='days'))
discharge$Station <- '450'
discharge$raw_discharge <- rep(NA, nrow(discharge))

# Setup two vectors to code the months
discharge_files <- list.files(data_folder)
discharge_files <- discharge_files[grepl('MDF_[0-9]{4}.txt', discharge_files)]
for (discharge_file in discharge_files) {
    this_year <- regmatches(discharge_file, 
                            regexpr('(19[0-9]{2})|(2[0-1][0-9]{2})', 
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
    dates <- as.Date(paste(rep(month_names, each=31), raw_data$Day, this_year), 
                     format='%b %d %Y')
    long_data <- cbind(stack(raw_data[month_cols]), dates)
    long_data <- long_data[!is.na(long_data$dates), ]
    if (!(nrow(long_data) %in% c(365, 366))) {
        stop(paste('error processing', discharge_file, '- number of days not equal to 365 or 366'))
    }
    discharge[match(long_data$dates, discharge$date), ]$raw_discharge <- long_data$values
}

# Clean the data
discharge$discharge <- as.numeric(discharge$raw_discharge)
table(discharge$raw_discharge[is.na(discharge$discharge)])
discharge$Year <- year(discharge$date)
discharge$Month <- month(discharge$date)
discharge$Day <- day(discharge$date)

save(discharge, file='discharge_daily.Rdata')
write.csv(discharge, file='discharge_daily.csv', row.names=FALSE)
