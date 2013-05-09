library(ggplot2)
library(plyr)

source('0_utility_functions.R')

rampur_SPI <- read.table('precip_monthly_rampur_SPI.dat', header=FALSE, skip=1, 
                         na.strings='-99.00')
names(rampur_SPI) <- c('Year', 'Month', paste('SPI', c(6, 12, 24, 48), sep='_'))
rampur_SPI$Date <- as.Date(paste(rampur_SPI$Year, rampur_SPI$Month, '15'),
                           '%Y %m %d')

save(rampur_SPI, file='precip_monthly_rampur_SPI.Rdata')
write.csv(rampur_SPI, file='precip_monthly_rampur_SPI.csv', row.names=FALSE)

SPI_6_plot  <- ggplot(rampur_SPI, aes(Date, SPI_6)) + geom_line() +
    ylab('SPI') + ylim(c(-2.5, 2.5))
png('precip_rampur_SPI_6.png', width=PLOT_WIDTH*PLOT_DPI, height=PLOT_HEIGHT*PLOT_DPI)
print(SPI_6_plot)
dev.off()

SPI_12_plot <- ggplot(rampur_SPI, aes(Date, SPI_12)) + geom_line() +
    ylab('SPI') + ylim(c(-2.5, 2.5))
png('precip_rampur_SPI_12.png', width=PLOT_WIDTH*PLOT_DPI, height=PLOT_HEIGHT*PLOT_DPI)
print(SPI_12_plot)
dev.off()

SPI_24_plot <- ggplot(rampur_SPI, aes(Date, SPI_24)) + geom_line() +
    ylab('SPI') + ylim(c(-2.5, 2.5))
png('precip_rampur_SPI_24.png', width=PLOT_WIDTH*PLOT_DPI, height=PLOT_HEIGHT*PLOT_DPI)
print(SPI_24_plot)
dev.off()

SPI_48_plot <- ggplot(rampur_SPI, aes(Date, SPI_48)) + geom_line() +
    ylab('SPI') + ylim(c(-2.5, 2.5))
png('precip_rampur_SPI_48.png', width=PLOT_WIDTH*PLOT_DPI, height=PLOT_HEIGHT*PLOT_DPI)
print(SPI_48_plot )
dev.off()
