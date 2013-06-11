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

t1ag_all <- read.xport("W:/Nepal/ICPSR_0538_Restricted/da04538-0002_REST.xpt")

ID_cols <- grep('(HHID)|(NEIGHID)', names(t1ag_all))

t1ag <- data.frame(t1ag_all[ID_cols])

t1ag$any_farming_1996 <- as.logical(t1ag_all$BAA1)

t1ag$farm_bari_any_1996 <- as.logical(t1ag_all$BAA3)
t1ag$farm_bari_any_1996[!t1ag$any_farming_1996] <- FALSE
table(t1ag$farm_bari_any_1996)
t1ag$farm_khet_any_1996 <- as.logical(t1ag_all$BAA11)
t1ag$farm_khet_any_1996[!t1ag$any_farming_1996] <- FALSE
table(t1ag$farm_khet_any_1996)

t1ag$irrig_bari_any_1996 <- t1ag_all$BAA5
t1ag$irrig_bari_any_1996[t1ag$irrig_bari_any_1996 == -1] <- NA
table(t1ag$irrig_bari_any_1996)
t1ag$irrig_khet_any_1996 <- t1ag_all$BAA13
t1ag$irrig_khet_any_1996[t1ag$irrig_khet_any_1996 == -1] <- NA
table(t1ag$irrig_khet_any_1996)

# Make table of type of land farmed for only those who DO farm
land_farmed <- table(bari=t1ag$farm_bari_any_1996, khet=t1ag$farm_khet_any_1996)/1773
addmargins(land_farmed)*100

t1ag$irrig_bari_m_only_1996 <- t1ag_all$BAA9
t1ag$irrig_bari_m_only_1996[t1ag$irrig_bari_m_only_1996 == 2] <- 0
t1ag$irrig_bari_m_only_1996[t1ag$irrig_bari_m_only_1996 == -1] <- NA
table(t1ag$irrig_bari_m_only_1996)

t1ag$irrig_khet_m_only_1996 <- t1ag_all$BAA17
t1ag$irrig_khet_m_only_1996[t1ag$irrig_khet_m_only_1996 == 2] <- 0
t1ag$irrig_khet_m_only_1996[t1ag$irrig_khet_m_only_1996 == -1] <- NA
table(t1ag$irrig_khet_m_only_1996)

# Also make 3 level irrigation indicator variables:
t1ag$irrig_khet_type_1996 <- t1ag$farm_khet_any_1996
t1ag$irrig_khet_type_1996[!t1ag$farm_khet_any_1996] <- 'No khet'
t1ag$irrig_khet_type_1996[!(t1ag$irrig_khet_any_1996 == 1)] <- 'No irrigation'
t1ag$irrig_khet_type_1996[t1ag$irrig_khet_any_1996 == 1] <- 'Irrigate any time'
t1ag$irrig_khet_type_1996[t1ag$irrig_khet_m_only_1996 == 1] <- 'Irrigate monsoon only'
t1ag$irrig_khet_type_1996 <- factor(t1ag$irrig_khet_type_1996,
                               levels=c('No khet', 'No irrigation',
                                        'Irrigate any time',
                                        'Irrigate monsoon only'))
table(t1ag$irrig_khet_type_1996)

t1ag$irrig_bari_type_1996 <- t1ag$farm_bari_any_1996
t1ag$irrig_bari_type_1996[!t1ag$farm_bari_any_1996] <- 'No bari'
t1ag$irrig_bari_type_1996[!(t1ag$irrig_bari_any_1996 == 1)] <- 'No irrigation'
t1ag$irrig_bari_type_1996[t1ag$irrig_bari_any_1996 == 1] <- 'Irrigate any time'
t1ag$irrig_bari_type_1996[t1ag$irrig_bari_m_only_1996 == 1] <- 'Irrigate monsoon only'
t1ag$irrig_bari_type_1996 <- factor(t1ag$irrig_bari_type_1996,
                               levels=c('No bari', 'No irrigation',
                                        'Irrigate any time',
                                        'Irrigate monsoon only'))
table(t1ag$irrig_bari_type_1996)

t1ag$irrig_khet_1996 <- t1ag$irrig_khet_type_1996 == 'Irrigate any time'
t1ag$irrig_khet_1996[t1ag$irrig_khet_type_1996 == 'Irrigate monsoon only'] <- TRUE
t1ag$irrig_bari_1996 <- t1ag$irrig_bari_type_1996 == 'Irrigate any time'
t1ag$irrig_bari_1996[t1ag$irrig_bari_type_1996 == 'Irrigate monsoon only'] <- TRUE

t1ag$any_chick_ducks_1996 <- as.logical(t1ag_all$BAB1)
table(t1ag$any_chick_ducks_1996)
t1ag$any_livestock_1996 <- as.logical(t1ag_all$BAB4)
table(t1ag$any_livestock_1996)

# Convert to Tropical Livestock_1996 Units from http://bit.ly/17aukgB
#	Cattle = .50
#	Buffalo = .50
#	Sheep = .10
#	Goats = .10
#	Pigs = .20
#	Asses = .50
#	Horses = .65
#	Mules = .60
#	Chickens = .01

chickduck_1996<- t1ag_all$BAB2
pigeons_1996 <- t1ag_all$BAB3
bulls_1996 <- t1ag_all$BAB5
cows_1996 <- t1ag_all$BAB6
hebuff_1996<- t1ag_all$BAB7
shebuff_1996 <- t1ag_all$BAB8
sheep_goats_1996 <- t1ag_all$BAB9
pigs_1996 <- t1ag_all$BAB10

chickduck_1996[chickduck_1996 == -1] <- 0
pigeons_1996[pigeons_1996 == -1] <- 0
bulls_1996[bulls_1996 == -1] <- 0
cows_1996[cows_1996 == -1] <- 0
hebuff_1996[hebuff_1996 == -1] <- 0
shebuff_1996[shebuff_1996 == -1] <- 0
sheep_goats_1996[sheep_goats_1996 == -1] <- 0
pigs_1996[pigs_1996 == -1] <- 0

t1ag$TLU_birds_1996 <- chickduck_1996 * .01 # chickens and ducks_1996
t1ag$TLU_birds_1996 <- t1ag$TLU_birds_1996 + pigeons_1996 * .01 # pigeons
t1ag$TLU_livestock_1996 <- bulls_1996 *.5 # bullock_1996s
t1ag$TLU_livestock_1996 <- t1ag$TLU_livestock_1996 + cows_1996 * .5 # cows
t1ag$TLU_livestock_1996 <- t1ag$TLU_livestock_1996 + hebuff_1996 * .5 # he buffaloes
t1ag$TLU_livestock_1996 <- t1ag$TLU_livestock_1996 + shebuff_1996 * .5 # she buffaloes
t1ag$TLU_livestock_1996 <- t1ag$TLU_livestock_1996 + sheep_goats_1996 * .1 # sheep and goats
t1ag$TLU_livestock_1996 <- t1ag$TLU_livestock_1996 + pigs_1996 * .2 # pigs
t1ag$TLU_birds_1996[is.na(t1ag$TLU_birds_1996)] <- 0
t1ag$TLU_livestock_1996[is.na(t1ag$TLU_livestock_1996)] <- 0
#hist(t1ag$TLU_livestock_1996)
#hist(t1ag$TLU_birds_1996)
t1ag$TLU_all_1996 <- t1ag$TLU_birds_1996 + t1ag$TLU_livestock_1996
t1ag$TLU_all_restrict_1996 <- t1ag$TLU_all_1996
t1ag$TLU_all_restrict_1996[t1ag$TLU_all_restrict_1996 > 15] <- 15
#hist(t1ag$TLU_all_1996)
#hist(t1ag$TLU_all_restrict_1996)

summary(t1ag$TLU_birds_1996)
summary(t1ag$TLU_livestock_1996)
summary(t1ag$TLU_all_1996)

table(t1ag$TLU_birds_1996 == 0) / 1773
table(t1ag$TLU_livestock_1996 == 0) / 1773

t1ag$own_house_plot_1996 <- as.logical(t1ag_all$BAA43)
t1ag$rent_out_land_1996 <- as.logical(t1ag_all$BAA44)
t1ag$rent_out_land_1996[is.na(t1ag$rent_out_land_1996)] <- FALSE

# Wealth measures
t1ag$own_radio_1996 <- t1ag_all$BAC1
t1ag$own_TV_1996 <- t1ag_all$BAC3
t1ag$own_bike_1996 <- t1ag_all$BAC5
t1ag$own_motorbike_1996 <- t1ag_all$BAC7
t1ag$own_cart_1996 <- t1ag_all$BAC9
t1ag$own_total_1996 <- rowSums(cbind(t1ag$own_radio_1996,
                                     t1ag$own_TV_1996,
                                     t1ag$own_bike_1996,
                                     t1ag$own_motorbike_1996,
                                     t1ag$own_cart_1996))

summary(t1ag)

save(t1ag, file='W:/Nepal/ICPSR_0538_Restricted/Recode/t1ag.Rdata')

# Svae TLU, any farming, and total possessions probability distributions for 
# Chitwan ABM.
make_txt_prob_dist <- function(probs, binlims, param.name) {
    # param.name is the name used by the ChitwanABM model for this parameter.
    binlims <- paste(round(binlims, digits=6), collapse=", ")
    probs <- paste(round(probs, digits=6), collapse=", ")
    txtprob <- paste("'", param.name, "' : [((", binlims, "), (", probs,  "))] | validate_prob_dist]", sep="")
    return(txtprob)
}

# First calculate TLU probs for farmers
TLU_farmer_lims <- c(seq(-1, 6), 10)
TLU_farmer_bin <- cut(t1ag$TLU_livestock_1996, TLU_farmer_lims)
TLU_farmer_prob <- data.frame(prob=table(TLU_farmer_bin[t1ag$any_farming_1996 == 1]))
names(TLU_farmer_prob) <- c('bin', 'prob')
TLU_farmer_prob$prob <- TLU_farmer_prob$prob / sum(TLU_farmer_prob$prob)
qplot(bin, prob*100, geom="bar", xlab="Tropical Livestock Units",
        ylab="Probability (%)", stat='identity', data=TLU_farmer_prob)
ggsave("prob_TLU_farmers.png", width=8.33, height=5.53, dpi=300)
write(make_txt_prob_dist(TLU_farmer_prob$prob, TLU_farmer_lims, 'TLU_probs.farmer'), 
      file="TLU_probs_farmers.txt")

# Now calculate TLU probs for non-farmers
TLU_nonfarmer_lims <- c(seq(-1, 6), 10)
TLU_nonfarmer_bin <- cut(t1ag$TLU_livestock_1996, TLU_nonfarmer_lims)
TLU_nonfarmer_prob <- data.frame(prob=table(TLU_nonfarmer_bin[t1ag$any_farming_1996 == 0]))
names(TLU_nonfarmer_prob) <- c('bin', 'prob')
TLU_nonfarmer_prob$prob <- TLU_nonfarmer_prob$prob / sum(TLU_nonfarmer_prob$prob)
qplot(bin, prob*100, geom="bar", xlab="Tropical Livestock Units",
        ylab="Probability (%)", stat='identity', data=TLU_nonfarmer_prob)
ggsave("prob_TLU_nonfarmers.png", width=8.33, height=5.53, dpi=300)
write(make_txt_prob_dist(TLU_nonfarmer_prob$prob, TLU_nonfarmer_lims, 'TLU_probs.nonfarmer'), 
      file="TLU_probs_nonfarmers.txt")

# Now calculate TLU probs for non-farmers
totalown_lims <- seq(-1, 5)
totalown_bin <- cut(t1ag$own_total_1996, totalown_lims)
totalown_prob <- data.frame(prob=table(totalown_bin))
names(totalown_prob) <- c('bin', 'prob')
totalown_prob$prob <- totalown_prob$prob / sum(totalown_prob$prob)
qplot(bin, prob*100, geom="bar", xlab="Total Possessions",
        ylab="Probability (%)", stat='identity', data=totalown_prob)
ggsave("prob_totalpossessions.png", width=8.33, height=5.53, dpi=300)
write(make_txt_prob_dist(totalown_prob$prob, totalown_lims, 'total_possessions_probs'), 
      file="total_possessions_prob.txt")

# Calculate probability of farming
table(t1ag$any_farming_1996)/nrow(t1ag)

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

t1ag$NEIGHID <- sprintf("%03i", t1ag$NEIGHID)
irrig_data <- ddply(t1ag, .(NEIGHID), summarize,
      frac_bari_no=sum(irrig_bari_type_1996 == 'No bari')/length(irrig_bari_type_1996),
      frac_bari_irrig_none=sum(irrig_bari_type_1996 == 'No irrigation')/length(irrig_bari_type_1996),
      frac_bari_irrig_any_seas=sum(irrig_bari_type_1996 == 'Irrigate any time')/length(irrig_bari_type_1996),
      frac_bari_irrig_mon_only=sum(irrig_bari_type_1996 == 'Irrigate monsoon only')/length(irrig_bari_type_1996),
      frac_khet_no=sum(irrig_khet_type_1996 == 'No khet')/length(irrig_khet_type_1996),
      frac_khet_irrig_none=sum(irrig_khet_type_1996 == 'No irrigation')/length(irrig_khet_type_1996),
      frac_khet_irrig_any_seas=sum(irrig_khet_type_1996 == 'Irrigate any time')/length(irrig_khet_type_1996),
      frac_khet_irrig_mon_only=sum(irrig_khet_type_1996 == 'Irrigate monsoon only')/length(irrig_khet_type_1996),
      frac_khet_any_irrig=sum(irrig_khet_1996, na.rm=TRUE)/sum(farm_khet_any_1996),
      frac_bari_any_irrig=sum(irrig_bari_1996, na.rm=TRUE)/sum(farm_bari_any_1996))
# Set NaNs (in NBHs without any farming at all) to zero.
irrig_data$frac_khet_any_irrig[is.nan(irrig_data$frac_khet_any_irrig)] <- 0
irrig_data$frac_bari_any_irrig[is.nan(irrig_data$frac_bari_any_irrig)] <- 0

save(irrig_data, file='t1_irrigation.Rdata')

# Follow instructions at http://bit.ly/10a4cP7
merged <- merge(cvfs_nbhs@data, irrig_data, all.x=TRUE)
cvfs_nbhs@data <- merged[match(cvfs_nbhs@data$NEIGHID, merged$NEIGHID), ]
writeOGR(cvfs_nbhs, '.', 't1_irrigation_shp', driver='ESRI Shapefile', overwrite=TRUE)

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
writeGDAL(khet_irrig_any_at_all$krige_output, 't1_khet_irrig_any_at_all.tif')
plot_irrig_map(khet_irrig_any_at_all$krige_output, 't1_khet_irrig_any_at_all_plot.png')

khet_irrig_m_only <- autoKrige(frac_khet_irrig_mon_only ~ 1, cvfs_nbhs, DEM)
plot(khet_irrig_m_only)
writeGDAL(khet_irrig_m_only$krige_output, 't1_khet_irrig_m_only.tif')
plot_irrig_map(khet_irrig_m_only$krige_output, 't1_khet_irrig_m_only_plot.png')

khet_irrig_any_seas <- autoKrige(frac_khet_irrig_any_seas ~ 1, cvfs_nbhs, DEM)
plot(khet_irrig_any_seas)
writeGDAL(khet_irrig_any_seas$krige_output, 't1_khet_irrig_any_season.tif')
plot_irrig_map(khet_irrig_any_seas$krige_output, 't1_khet_irrig_any_season_plot.png')
