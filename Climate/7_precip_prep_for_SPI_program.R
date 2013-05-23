library(plyr)
library(spi)

source('0_utility_functions.R')

load('precip_daily_Chitwan_cleaned.Rdata')

precip_monthly <- ddply(precip, .(Station, Year, Month), summarize,
                        total=round(sum(precip, na.rm=TRUE)),
                        num_NA=sum(is.na(precip)))
# Eliminate years with NAs to obtain continuous series with no missing values
precip_monthly <- precip_monthly[!(precip_monthly$Station == 'Rampur' &
                                  (precip_monthly$Year >= 2012 |
                                   precip_monthly$Year <= 1967)), ]
precip_monthly <- precip_monthly[!(precip_monthly$Station == 'Dumkauli' &
                                   precip_monthly$Year <= 1978), ]
# Check that there are no missing values
precip_monthly[precip_monthly$num_NA > 1, ]

# Drop the num_NA column
precip_monthly <- precip_monthly[!(names(precip_monthly) == 'num_NA')]

stations <- c('Rampur', 'Jhawani', 'Dumkauli')
for (station in stations) {
    precip_monthly_this_station <- precip_monthly[precip_monthly$Station == station, ]
    # Remove Station column
    precip_monthly_this_station <- precip_monthly_this_station[-1]
    # create and open the file connection
    datafile <- file(paste('precip_monthly_', station, '.cor', sep=''), 
                     open='wt')
    writeLines(paste(station, 'station monthly precip (mm)'), con=datafile)
    write.table(precip_monthly_this_station, file=datafile, row.names=FALSE, 
                col.names=FALSE)
    close(datafile)
}
