library(ggplot2)
library(reshape2)
library(plyr)

source('0_utility_functions.R')

SPI_lengths <- c(1, 3, 6, 9, 12, 24)

SPI <- data.frame()
stations <- c('Rampur', 'Jhawani', 'Dumkauli')
for (station in stations) {
    datafile <- paste('precip_monthly_', station, '_SPI.dat', sep='')
    this_SPI <- read.table(datafile, header=FALSE, 
                           skip=1, na.strings='-99.00')
    names(this_SPI) <- c('Year', 'Month', paste('SPI', SPI_lengths, sep='_'))
    this_SPI$Date <- as.Date(paste(this_SPI$Year, this_SPI$Month, '15'),
                               '%Y %m %d')
    this_SPI$Station <- station
    if (nrow(SPI) == 0) {SPI <- this_SPI} else {SPI <- rbind(SPI, this_SPI)}
    save(this_SPI, file=paste('precip_monthly_', station, '_SPI.Rdata', sep=''))
    write.csv(this_SPI, file=paste('precip_monthly_', station, '_SPI.csv', 
                                     sep=''), row.names=FALSE)
}

SPI$Station <- factor(SPI$Station, levels=c('Rampur', 'Dumkauli', 'Jhawani'))

for (SPI_length in SPI_lengths) {
    SPI_var_name <- paste('SPI_', SPI_length, sep='')

    SPI_plot  <- ggplot(SPI, aes_string(x='Date', y=SPI_var_name, colour='Station', 
                                          linetype='Station')) +
        geom_line() + ylab('SPI') +
        geom_hline(yintercept=1.5, linetype=2) +
        geom_hline(yintercept=-1.5, linetype=2)
    png(paste('precip_monthly_SPI_', SPI_length, '.png', sep=''), 
        width=PLOT_WIDTH*PLOT_DPI, height=PLOT_HEIGHT*PLOT_DPI)
    print(SPI_plot)
    dev.off()

    SPI_plot_facets <- ggplot(SPI, aes_string(x='Date', y=SPI_var_name)) +
        geom_line() + ylab('SPI') + facet_grid(Station ~ .) +
        geom_hline(yintercept=1.5, linetype=2) +
        geom_hline(yintercept=-1.5, linetype=2)
    png(paste('precip_monthly_SPI_', SPI_length, '_facets.png', sep=''), 
        width=PLOT_WIDTH*PLOT_DPI, height=PLOT_HEIGHT*PLOT_DPI)
    print(SPI_plot_facets)
    dev.off()
}


# Make a meltplot for publication
SPI_melt <- melt(SPI, id.vars=c('Station', 'Date'),
                 measure.vars=c('SPI_6', 'SPI_24'))
SPI_melt$variable <- factor(SPI_melt$variable,
                            levels=c('SPI_6', 'SPI_24'),
                            labels=c('6-month SPI', '24-month SPI'))
SPI_melt_plot <- ggplot(SPI_melt, aes(Date, value)) +
    geom_line() + xlab('Time') + ylab('SPI') +
    facet_grid(variable ~ Station) +
    geom_hline(yintercept=1.5, linetype=2) +
    geom_hline(yintercept=0, linetype=1) +
    geom_hline(yintercept=-1.5, linetype=2) +
    theme(legend.position='none') +
    theme(axis.title.x=element_text(vjust=-0.5)) +
    theme(strip.text.x=element_text(lineheight=2.5))
png('precip_monthly_SPI_meltplot.png', width=PLOT_WIDTH*PLOT_DPI*2, height=PLOT_HEIGHT*PLOT_DPI)
print(SPI_melt_plot)
dev.off()


save(SPI, file=paste('precip_monthly_SPI_all_stations.Rdata', sep=''))
