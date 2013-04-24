library(foreign)
library(Hmisc)
library(ggplot2)
library(lubridate)

PLOT_WIDTH <- 8.33
PLOT_HEIGHT <- 5.53
DPI <- 300
theme_set(theme_grey(base_size=18))
update_geom_defaults("line", aes(size=.5))
default_scale_colour <- function(...) {scale_colour_brewer(palette="Dark2", ...)}
default_scale_fill <- function(...) {scale_fill_brewer(palette="Dark2", ...)}
scale_colour_discrete <- default_scale_colour
scale_fill_discrete <- default_scale_fill

# And add percentage and log percentage columns for agricultural vegetation
nbhhist <- read.xport("W:/Nepal/ICPSR_0538_Restricted/da04538-0014_REST.xpt")

nbhhist <- nbhhist[nbhhist$NEIGHID <= 151, ]

ID_cols <- grep('^(NEIGHID|STRATA|NX|NY)$', names(nbhhist))
nfos <- nbhhist[ID_cols]
nfos$NEIGHID <- factor(sprintf("%03d", nfos$NEIGHID))
nfos$STRATA <- factor(nfos$STRATA)

# Calculate the distance of each neighborhood from Narayanghat (using the 
# coordinates of the center of the road in the middle of the downtown area of 
# Narayanghat), and convert from meters to kilometers
nfos$dist_nara <- (sqrt((nfos$NX - 245848)**2 + (nfos$NY - 3066013)**2)) / 1000
nfos$dist_nara_cut <- cut(nfos$dist_nara, breaks=c(0, 10, 20, Inf), include.lowest=TRUE)

# Load and recode the LULC data
lulc_orig <- read.xport("W:/Nepal/ICPSR_SupplementalData/Survey_converted/landuse.xpt")

agveg_t1 <- with(lulc_orig, rowSums(cbind(BARI1, IKHET1, RKHET1)))
nonagveg_t1 <- with(lulc_orig, rowSums(cbind(GRASSC1, GRASSP1, PLANTC1, PLANTP1)))
privbldg_t1 <- with(lulc_orig, rowSums(cbind(HHRESID1, MILL1, OTRBLD1)))
pubbldg_t1 <- with(lulc_orig, rowSums(cbind(ROAD1, SCHOOL1, TEMPLE1)))
other_t1 <- with(lulc_orig, rowSums(cbind(CANAL1, POND1, RIVER1, SILT1, UNDVP1)))

agveg_t2 <- with(lulc_orig, rowSums(cbind(BARI2, IKHET2, RKHET2)))
nonagveg_t2 <- with(lulc_orig, rowSums(cbind(GRASSC2, GRASSP2, PLANTC2, PLANTP2)))
privbldg_t2 <- with(lulc_orig, rowSums(cbind(HHRESID2, MILL2, OTRBLD2)))
pubbldg_t2 <- with(lulc_orig, rowSums(cbind(ROAD2, SCHOOL2, TEMPLE2)))
other_t2 <- with(lulc_orig, rowSums(cbind(CANAL2, POND2, RIVER2, SILT2, UNDVP2)))

agveg_t3 <- with(lulc_orig, rowSums(cbind(BARI3, IKHET3, RKHET3)))
nonagveg_t3 <- with(lulc_orig, rowSums(cbind(GRASSC3, GRASSP3, PLANTC3, PLANTP3)))
privbldg_t3 <- with(lulc_orig, rowSums(cbind(HHRESID3, MILL3, OTRBLD3)))
pubbldg_t3 <- with(lulc_orig, rowSums(cbind(ROAD3, SCHOOL3, TEMPLE3)))
other_t3 <- with(lulc_orig, rowSums(cbind(CANAL3, POND3, RIVER3, SILT3, UNDVP3)))

lulc <- data.frame(NEIGHID=lulc_orig$NEIGHID, agveg_t1, nonagveg_t1, privbldg_t1, 
                   pubbldg_t1, other_t1, agveg_t2, nonagveg_t2, privbldg_t2, 
                   pubbldg_t2, other_t2, agveg_t3, nonagveg_t3, privbldg_t3, 
                   pubbldg_t3, other_t3)
# Convert land areas expressed in square feet to square meters
lulc[2:ncol(lulc)]  <- lulc[2:ncol(lulc)] * .09290304

lulc$total_t1 <- apply(lulc[grep('_t1$', names(lulc))], 1, sum)
lulc$total_t2 <- apply(lulc[grep('_t2$', names(lulc))], 1, sum)
lulc$total_t3 <- apply(lulc[grep('_t3$', names(lulc))], 1, sum)

lulc$percagveg_t1 <- with(lulc, (agveg_t1/total_t1)*100)
lulc$percagveg_t2 <- with(lulc, (agveg_t2/total_t2)*100)
lulc$percagveg_t3 <- with(lulc, (agveg_t3/total_t3)*100)
lulc$logpercagveg_t1 <- log(lulc$percagveg_t1 + 1)
lulc$logpercagveg_t2 <- log(lulc$percagveg_t2 + 1)
lulc$logpercagveg_t3 <- log(lulc$percagveg_t3 + 1)

nfos <- merge(nfos, lulc)

# Make a variable giving the average number of years non-family services have 
# been available in the neighborhood (for consistency with established models).
nbh_cols_regex <- '^(SCHLFT|HLTHFT|BUSFT|MARFT|EMPFT)[1-5][0-9]$'
nbhhist_nbh_serv_cols <- grep(nbh_cols_regex, names(nbhhist))
nfos <- cbind(nfos, nbhhist[nbhhist_nbh_serv_cols])

nfos_nbh_serv_cols <- grep(nbh_cols_regex, names(nfos))

nfos_long <- reshape(nfos, direction='long', idvar='NEIGHID', 
                        timevar='NEP_YEAR', 
                        varying=names(nfos)[nfos_nbh_serv_cols], 
                        sep="")
# Note that Nepali year 2052 is 1996
Nepali_years <- seq(10, len=43)
western_years <- seq(as.Date("1955-06-15"), by='year', len=43)
nfos_long$YEAR <- western_years[match(nfos_long$NEP_YEAR, Nepali_years)]

services <- c('SCHLFT', 'HLTHFT', 'BUSFT', 'MARFT', 'EMPFT')
pb <- txtProgressBar(style=3)
for (n in 1:length(services)) {
    service <- services[n]
    setTxtProgressBar(pb, (n-1)/length(services))
    data_col <- which(names(nfos_long) == service)

    p <- qplot(YEAR, nfos_long[, data_col], geom='line', colour=NEIGHID, 
               data=nfos_long, ylab='Distance in minutes on foot', xlab='Year')
    p + scale_colour_discrete(guide=FALSE)
    ggsave(paste('1955-1996_', service, '.png', sep=''), width=PLOT_WIDTH, 
           height=PLOT_HEIGHT, dpi=DPI)

    p <- qplot(YEAR, nfos_long[, data_col], geom='line', colour=NEIGHID, 
               facets=dist_nara_cut~., data=nfos_long, ylab='Distance in minutes on foot', xlab='Year')
    p + scale_colour_discrete(guide=FALSE)
    ggsave(paste('1955-1996_', service, '_by_dist.png', sep=''), width=8.33, 
           height=5.53, dpi=300)

    recent_data <- nfos_long[nfos_long$YEAR > 1980, ]
    p <- qplot(YEAR, recent_data[, data_col], geom='line', colour=NEIGHID, 
               data=recent_data, ylab='Distance in minutes on foot', xlab='Year')
    p + scale_colour_discrete(guide=FALSE)
    ggsave(paste('1980-1996_', service, '.png', sep=''), width=8.33, 
           height=5.53, dpi=300)

    p <- qplot(YEAR, recent_data[, data_col], geom='line', colour=NEIGHID, 
               facets=dist_nara_cut~., data=recent_data, ylab='Distance in minutes on foot', xlab='Year')
    p + scale_colour_discrete(guide=FALSE)
    ggsave(paste('1980-1996_', service, '_by_dist.png', sep=''), width=8.33, 
           height=5.53, dpi=300)

    services_1996 <- cut(nfos_long[, data_col][year(nfos_long$YEAR) == 
                         1996], breaks=c(0, 2, 5, 10, 20, 30, 60, Inf), include.lowest=TRUE)
    p <- qplot(services_1996, geom='histogram', ylab='Count', xlab='Distance in minutes on foot')
    p + ylim(c(0, 55))
    ggsave(paste('1996_', service, '_histogram.png', sep=''), width=8.33, 
           height=5.53, dpi=300)
    setTxtProgressBar(pb, (n)/length(services))
}
close(pb)

save(nfos_long, file="nfos_long.Rdata")
write.csv(nfos_long, file="nfos_long.csv", row.names=FALSE)
save(nfos, file="nfos.Rdata")
write.csv(nfos, file="nfos.csv", row.names=FALSE)
