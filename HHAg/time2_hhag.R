library(ggplot2)
library(reshape2)
library(foreign)
library(plyr)
library(rgdal)
library(automap)
library(raster)

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

t2ag_all <- read.xport("W:/Nepal/ICPSR_SupplementalData/Survey_converted/t2ag.xpt")

ID_cols <- grep('(_HHID)|(DATE)|(_HOUSE)|(_NEIGH)', names(t2ag_all))
#t2_crop_cols <- grep('^BAA29[A-E]$', names(t2ag))

t2ag <- data.frame(t2ag_all[ID_cols])

t2ag$any_farming_2001 <- as.logical(t2ag_all$T2A1)

t2ag$crops_kept_2001 <- t2ag_all$T2A30
t2ag$crops_kept_2001[t2ag$crops_kept_2001 == -1] <- NA
t2ag$crops_kept_2001 <- ordered(t2ag$crops_kept_2001, levels=c(1, 2, 3, 4, 97),
                           labels=c('All', 'Kept most', 'Kept half',
                                    'Sold most', 'Other'))
table(t2ag$crops_kept_2001)

t2ag$farm_bari_any_2001 <- as.logical(t2ag_all$T2A3)
t2ag$farm_bari_any_2001[!t2ag$any_farming_2001] <- FALSE
table(t2ag$farm_bari_any_2001)
t2ag$farm_khet_any_2001 <- as.logical(t2ag_all$T2A11)
t2ag$farm_khet_any_2001[!t2ag$any_farming_2001] <- FALSE
table(t2ag$farm_khet_any_2001)

t2ag$irrig_bari_any_2001 <- as.logical(t2ag_all$T2A5)
table(t2ag$irrig_bari_any_2001)
t2ag$irrig_khet_any_2001 <- as.logical(t2ag_all$T2A13)
table(t2ag$irrig_khet_any_2001)

# Make table of type of land farmed for only those who DO farm
land_farmed <- table(bari=t2ag$farm_bari_any_2001, khet=t2ag$farm_khet_any_2001)/1586
land_farmed[1] <- 0
addmargins(land_farmed)*100

t2ag$irrig_bari_m_only_2001 <- t2ag_all$T2A9
t2ag$irrig_bari_m_only_2001[t2ag$irrig_bari_m_only_2001 == 2] <- 0
t2ag$irrig_bari_m_only_2001 <- as.logical(t2ag$irrig_bari_m_only_2001)
table(t2ag$irrig_bari_m_only_2001)

t2ag$irrig_khet_m_only_2001 <- t2ag_all$T2A17
t2ag$irrig_khet_m_only_2001[t2ag$irrig_khet_m_only_2001 == 2] <- 0
t2ag$irrig_khet_m_only_2001 <- as.logical(t2ag$irrig_khet_m_only_2001)
table(t2ag$irrig_khet_m_only_2001)

# Also make 3 level irrigation indicator variables:
t2ag$irrig_khet_type_2001 <- t2ag$farm_khet_any_2001
t2ag$irrig_khet_type_2001[!t2ag$farm_khet_any_2001] <- 'No khet'
t2ag$irrig_khet_type_2001[!t2ag$irrig_khet_any_2001] <- 'No irrigation'
t2ag$irrig_khet_type_2001[t2ag$irrig_khet_any_2001] <- 'Irrigate any time'
t2ag$irrig_khet_type_2001[t2ag$irrig_khet_m_only_2001] <- 'Irrigate monsoon only'
t2ag$irrig_khet_type_2001 <- factor(t2ag$irrig_khet_type_2001,
                               levels=c('No khet', 'No irrigation',
                                        'Irrigate any time',
                                        'Irrigate monsoon only'))
table(t2ag$irrig_khet_type_2001)

t2ag$irrig_bari_type_2001 <- t2ag$farm_bari_any_2001
t2ag$irrig_bari_type_2001[!t2ag$farm_bari_any_2001] <- 'No bari'
t2ag$irrig_bari_type_2001[!t2ag$irrig_bari_any_2001] <- 'No irrigation'
t2ag$irrig_bari_type_2001[t2ag$irrig_bari_any_2001] <- 'Irrigate any time'
t2ag$irrig_bari_type_2001[t2ag$irrig_bari_m_only_2001] <- 'Irrigate monsoon only'
t2ag$irrig_bari_type_2001 <- factor(t2ag$irrig_bari_type_2001,
                               levels=c('No bari', 'No irrigation',
                                        'Irrigate any time',
                                        'Irrigate monsoon only'))
table(t2ag$irrig_bari_type_2001)

t2ag$irrig_khet_2001 <- t2ag$irrig_khet_type_2001 == 'Irrigate any time'
t2ag$irrig_khet_2001[t2ag$irrig_khet_type_2001 == 'Irrigate monsoon only'] <- TRUE
t2ag$irrig_bari_2001 <- t2ag$irrig_bari_type_2001 == 'Irrigate any time'
t2ag$irrig_bari_2001[t2ag$irrig_bari_type_2001 == 'Irrigate monsoon only'] <- TRUE

t2ag$exchanged_labor_2001 <- as.logical(t2ag_all$T2A22)
table(t2ag$exchanged_labor_2001)

t2ag$used_fertilizer_2001 <- as.logical(t2ag_all$T2A26)
table(t2ag$used_fertilizer_2001)

t2ag$used_pesticide_2001 <- as.logical(t2ag_all$T2A29)
table(t2ag$used_pesticide_2001)

t2ag$any_chick_ducks_2001 <- as.logical(t2ag_all$T2B1)
table(t2ag$any_chick_ducks_2001)

# Convert to Tropical Livestock_2001 Units from http://bit.ly/17aukgB
#	Cattle = .50
#	Buffalo = .50
#	Sheep = .10
#	Goats = .10
#	Pigs = .20
#	Asses = .50
#	Horses = .65
#	Mules = .60
#	Chickens = .01

t2ag$TLU_birds_2001 <- t2ag_all$T2B2 * .01 # chickens and ducks_2001
t2ag$TLU_birds_2001 <- t2ag$TLU_birds_2001 + t2ag_all$T2B3 * .01 # pigeons
t2ag$TLU_livestock_2001 <- t2ag_all$T2B5 *.5 # bullock_2001s
t2ag$TLU_livestock_2001 <- t2ag$TLU_livestock_2001 + t2ag_all$T2B6 * .5 # cows
t2ag$TLU_livestock_2001 <- t2ag$TLU_livestock_2001 + t2ag_all$T2B7 * .5 # he buffaloes
t2ag$TLU_livestock_2001 <- t2ag$TLU_livestock_2001 + t2ag_all$T2B8 * .5 # she buffaloes
t2ag$TLU_livestock_2001 <- t2ag$TLU_livestock_2001 + t2ag_all$T2B9 * .1 # sheep and goats
t2ag$TLU_livestock_2001 <- t2ag$TLU_livestock_2001 + t2ag_all$T2B10 * .2 # pigs
t2ag$TLU_birds_2001[is.na(t2ag$TLU_birds_2001)] <- 0
t2ag$TLU_livestock_2001[is.na(t2ag$TLU_livestock_2001)] <- 0
#hist(t2ag$TLU_livestock_2001)
#hist(t2ag$TLU_birds_2001)
t2ag$TLU_all_2001 <- t2ag$TLU_birds_2001 + t2ag$TLU_livestock_2001
t2ag$TLU_all_restrict_2001 <- t2ag$TLU_all_2001
t2ag$TLU_all_restrict_2001[t2ag$TLU_all_restrict_2001 > 15] <- 15
#hist(t2ag$TLU_all_2001)
#hist(t2ag$TLU_all_restrict_2001)
table(t2ag$TLU_birds_2001 == 0)
table(t2ag$TLU_livestock_2001 == 0)
table(t2ag$TLU_all_2001 == 0)

table(t2ag$TLU_birds_2001 == 0) / 2037
table(t2ag$TLU_livestock_2001 == 0) / 2037

t2ag$own_house_plot_2001 <- as.logical(t2ag_all$T2A43)
t2ag$rent_out_land_2001 <- as.logical(t2ag_all$T2A44)
t2ag$rent_out_land_2001[is.na(t2ag$rent_out_land_2001)] <- FALSE

# Income
# Note that there are 3 'Don't know' answers to T2G10 - these are left coded as 
# NAs
t2ag$income_2001 <- NA
t2ag$income_2001[t2ag_all$T2G10 == 0] <- '0'
t2ag$income_2001[t2ag_all$T2G11 == 2]  <- '25 - 50'
t2ag$income_2001[t2ag_all$T2G12 == 1] <- '0 - 10'
t2ag$income_2001[t2ag_all$T2G12 == 2] <- '10 - 25'
t2ag$income_2001[t2ag_all$T2G13 == 1] <- '50 - 100'
t2ag$income_2001[t2ag_all$T2G14 == 1] <- '100 - 250'
t2ag$income_2001[t2ag_all$T2G15 == 1] <- '250 - 500'
t2ag$income_2001[t2ag_all$T2G15 == 2] <- '> 500'
t2ag$income_2001 <- factor(t2ag$income_2001, ordered=TRUE,
                      levels=c('0', '0 - 10', '10 - 25',
                               '25 - 50', '50 - 100',
                               '100 - 250', '250 - 500',
                               '> 500'))
t2ag$income_2001_USD <- factor(t2ag$income_2001, ordered=TRUE,
                          levels=c('0', '0 - 10', '10 - 25',
                                   '25 - 50', '50 - 100',
                                   '100 - 250', '250 - 500',
                                   '> 500'),
                          labels=c('0', '0 - 111', '111 - 277',
                                   '277 - 554', '554 - 1111',
                                   '1111 - 2771', '2771 - 5543',
                                   '> 5543'))
ggplot(data=t2ag, aes(income_2001)) + geom_histogram() +
    xlab('Total income in past year (NPR / 1000)') + ylab('Count') +
    theme(axis.text.x=element_text(angle=45, hjust=1))

ggplot(data=t2ag, aes(income_2001_USD)) + geom_histogram() +
    xlab('Total income in past year (USD)') + ylab('Count') +
    theme(axis.text.x=element_text(angle=45, hjust=1))

# Wealth measures
t2ag$own_radio_2001 <- t2ag_all$T2C1
t2ag$own_TV_2001 <- t2ag_all$T2C3
t2ag$own_bike_2001 <- t2ag_all$T2C5
t2ag$own_motorbike_2001 <- t2ag_all$T2C7
t2ag$own_cart_2001 <- t2ag_all$T2C9

t2ag$own_total_2001 <- rowSums(cbind(t2ag$own_radio_2001,
                                     t2ag$own_TV_2001,
                                     t2ag$own_bike_2001,
                                     t2ag$own_motorbike_2001,
                                     t2ag$own_cart_2001))

summary(t2ag)

save(t2ag, file='W:/Nepal/ICPSR_0538_Restricted/Recode/t2ag.Rdata')

# Use automap to make interpolated surfaces of where irrigation is
CVFS_mask <- readGDAL("W:/Nepal/chitwanabm_initialization/CVFS_Study_Area_Raster_90m.tif")
DEM <- readGDAL("W:/Nepal/chitwanabm_initialization/CVFS_Study_Area_DEM_Raster_90m.tif")
names(DEM) <- 'ELEV'
CVFS_mask$band1[CVFS_mask$band1 == 0] <- NA
DEM$ELEV <- DEM$ELEV * CVFS_mask$band1

cvfs_nbhs <- readOGR('R:/Data/Nepal/GIS/CVFS_Data', 'cvfsns_with_elevations')
cvfs_nbhs <- cvfs_nbhs[cvfs_nbhs$NID <= 151, ]
cvfs_nbhs$NEIGHID <- sprintf("%03i", cvfs_nbhs$NID)
cvfs_nbhs$ELEV <- cvfs_nbhs$RASTERVALU
cvfs_nbhs <- cvfs_nbhs[, names(cvfs_nbhs) %in% c('NEIGHID', 'ELEV')]
cvfs_nbhs <- spTransform(cvfs_nbhs, CRS(projection(DEM)))

irrig_data <- ddply(t2ag, .(T2_NEIGH), summarize,
      frac_bari_no=sum(irrig_bari_type_2001 == 'No bari')/length(irrig_bari_type_2001),
      frac_bari_irrig_none=sum(irrig_bari_type_2001 == 'No irrigation')/length(irrig_bari_type_2001),
      frac_bari_irrig_any_seas=sum(irrig_bari_type_2001 == 'Irrigate any time')/length(irrig_bari_type_2001),
      frac_bari_irrig_mon_only=sum(irrig_bari_type_2001 == 'Irrigate monsoon only')/length(irrig_bari_type_2001),
      frac_khet_no=sum(irrig_khet_type_2001 == 'No khet')/length(irrig_khet_type_2001),
      frac_khet_irrig_none=sum(irrig_khet_type_2001 == 'No irrigation')/length(irrig_khet_type_2001),
      frac_khet_irrig_any_seas=sum(irrig_khet_type_2001 == 'Irrigate any time')/length(irrig_khet_type_2001),
      frac_khet_irrig_mon_only=sum(irrig_khet_type_2001 == 'Irrigate monsoon only')/length(irrig_khet_type_2001),
      frac_khet_any_irrig=sum(irrig_khet_2001, na.rm=TRUE)/sum(farm_khet_any_2001),
      frac_bari_any_irrig=sum(irrig_bari_2001, na.rm=TRUE)/sum(farm_bari_any_2001))
# Set NaNs (in NBHs without any farming at all) to zero.
irrig_data$frac_khet_any_irrig[is.nan(irrig_data$frac_khet_any_irrig)] <- 0
irrig_data$frac_bari_any_irrig[is.nan(irrig_data$frac_bari_any_irrig)] <- 0

save(irrig_data, file='t2_irrigation.Rdata')

# Follow instructions at http://bit.ly/10a4cP7
merged <- merge(cvfs_nbhs@data, irrig_data, by.x='NEIGHID', by.y='T2_NEIGH', all.x=TRUE)
cvfs_nbhs@data <- merged[match(cvfs_nbhs@data$NEIGHID, merged$NEIGHID), ]
writeOGR(cvfs_nbhs, '.', 't2_irrigation_shp', driver='ESRI Shapefile', overwrite=TRUE)

plot_irrig_map <- function(irrig_df, png_file) {
    cvfs_area <- readOGR(dsn="R:/Data/Nepal/GIS/ChitwanDistrict", layer="CVFS_Study_Area")
    # Need to transform CVFS polygon to WGS84 since it is in UTM
    cvfs_area  <- spTransform(cvfs_area, CRS(projection(irrig_df)))
    cvfs_area@data$id <- rownames(cvfs_area@data)
    cvfs_area.points <- fortify(cvfs_area, region="id")
    cvfs_area.df <- join(cvfs_area.points, cvfs_area@data, by="id")
    theme_set(theme_bw(base_size=8))
    irrig_df <- data.frame(x=coordinates(irrig_df)[, 1],
                           y=coordinates(irrig_df)[, 2],
                           fraction=irrig_df$var1.pred)
    ggplot(irrig_df) +
        geom_raster(aes(x, y, fill=fraction)) + coord_fixed() + 
        theme(axis.text.x=element_blank(), axis.text.y=element_blank(),
              axis.title.x=element_blank(), axis.title.y=element_blank(),
              panel.background=element_blank(), panel.border=element_blank(),
              panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
              plot.background=element_blank(), axis.ticks=element_blank(),
              plot.margin=unit(c(.1, .1, .1, .1), 'cm')) +
        scale_fill_gradient2(midpoint=.5, low='blue', mid='yellow', high='red',
                             limits=c(0, 1), name='Fraction') +
        guides(fill=guide_colorbar(barwidth=.5, barheight=4, ticks=FALSE)) +
        geom_path(data=cvfs_area.df, aes(long, lat), color='black', size=.5, alpha=.5)
    ggsave(png_file, width=PLOT_WIDTH*2, height=PLOT_HEIGHT*.5*2, dpi=PLOT_DPI)
}

khet_irrig_any_at_all <- autoKrige(frac_khet_any_irrig ~ 1, cvfs_nbhs, DEM)
plot(khet_irrig_any_at_all)
writeGDAL(khet_irrig_any_at_all$krige_output, 'khet_irrig_any_at_all.tif')
plot_irrig_map(khet_irrig_any_at_all$krige_output, 'khet_irrig_any_at_all_plot.png')

khet_irrig_m_only <- autoKrige(frac_khet_irrig_mon_only ~ 1, cvfs_nbhs, DEM)
plot(khet_irrig_m_only)
writeGDAL(khet_irrig_m_only$krige_output, 'khet_irrig_m_only.tif')
plot_irrig_map(khet_irrig_m_only$krige_output, 'khet_irrig_m_only_plot.png')

khet_irrig_any_seas <- autoKrige(frac_khet_irrig_any_seas ~ 1, cvfs_nbhs, DEM)
plot(khet_irrig_any_seas)
writeGDAL(khet_irrig_any_seas$krige_output, 'khet_irrig_any_season.tif')
plot_irrig_map(khet_irrig_any_seas$krige_output, 'khet_irrig_any_season_plot.png')
