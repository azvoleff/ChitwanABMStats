library(plyr)
library(spi)

source('0_shared_code.R')

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
