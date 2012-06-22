##############################################################################
# Reads in household wood usage data, cleans it, and sets it up for use in R.  
# The resulting data is saved in binary R format.
###############################################################################

require(lme4)
require(ggplot2)

update_geom_defaults("line", aes(size=1))
update_geom_defaults("smooth", aes(size=1))
theme_update(theme_grey(base_size=24))
update_geom_defaults("point", aes(size=3))
DPI <- 300
WIDTH <- 9
HEIGHT <- 5.67

load("hhwu1.Rdata")
load("hhwu2.Rdata")

# Select only the columns that I need out of the hhwu1 dataset.
hhwu <- hhwu1[c(2, 3, 4, 17, 85:91)]
# Also add an "average age" column, that is the average of the ages of all the 
# household members present for hhwu1.
age.cols <- grep("Psn.Age.", names(hhwu1))
age.mean <- apply(hhwu1[age.cols], 1, mean, na.rm=TRUE)
hhwu <- cbind(hhwu, Psn.Age.Mean=age.mean)
hhwu$Ethnicity <- factor(hhwu$Ethnicity)
# TODO: for now, remove the repeated 065008 line. Need to hear back from Binoj 
# on what the ID for this line actually is
hhwu <- hhwu[-which(hhwu$ID.Full=="065008")[1],]
# Make a "own stove other than wood" variable
Own.NW.Stove <- with(hhwu, apply(cbind(Stove.LPG, Stove.Kerosene,
        Stove.Biogas, Stove.Electric, Stove.Wooddust, Stove.Other), 1, sum))
Own.NW.Stove[Own.NW.Stove > 1] <- 1
Own.NW.Stove <- factor(Own.NW.Stove)
# Make an ethnicity indicator variable
# 1 - Upper caste hindu
# 2 - Hill tibetoburmese
# 3 - Lower caste hindu
# 4 - Newar
# 5 - Terai tibetoburmese
Ethnic.Indic <- as.numeric(hhwu$Ethnicity)
Ethnic.Indic[Ethnic.Indic != 2] <- 0
Ethnic.Indic[Ethnic.Indic == 2] <- 1
Ethnic.Indic <- factor(Ethnic.Indic)
Ethnic.Indic.New <- as.numeric(hhwu$Ethnicity)
Ethnic.Indic.New[Ethnic.Indic.New != 1] <- 0
Ethnic.Indic.New <- factor(Ethnic.Indic.New)
hhwu <- cbind(hhwu, Own.NW.Stove, Ethnic.Indic, Ethnic.Indic.New)

# Remove all the unnecessary stove cols, except for the first, which is 
# Stove.Wood, which is needed (hence the [-1] at the end of the line below)
stove.cols <- grep("Stove.", names(hhwu))[-1]
hhwu <- hhwu[,-stove.cols]

# Calculate mean of hhsize.avg and mean mass.used.dry for each household over 
# all 5 periods, to be used later in a simplistic linear model
hhsize.avg.cols <- grep("HHSize.Avg.", names(hhwu2))
HHSize.Avg.Mean <- apply(hhwu2[hhsize.avg.cols], 1, mean)
mass.used.cols <- grep("Mass.Used.", names(hhwu2))
Mass.Used.Mean <- apply(hhwu2[mass.used.cols], 1, mean)
newcols <- data.frame(ID.Full=hhwu2$ID.Full, Mass.Used.Mean,
        hhwu2[mass.used.cols], HHSize.Avg.Mean, hhwu2[hhsize.avg.cols])
hhwu <- merge(hhwu, newcols, by="ID.Full")

hhsize.cols <- grep("^HHSize.Avg.[1-5]$", names(hhwu))
mass.used.cols <- grep("^Mass.Used.[1-5]$", names(hhwu))
mass.used.pp.cols <- grep("^Mass.Used.pp.[1-5]$", names(hhwu))
hhwu.long <- reshape(hhwu, varying=list(hhsize.cols, mass.used.cols,
        mass.used.pp.cols), direction="long", timevar="Period",
        idvar="ID.Full")

# Sort data for later plotting
hhwu <- hhwu[order(hhwu$HHSize.Avg.Mean),]

# Drop outliers
hhwu <- hhwu[!hhwu$HHSize.Avg.Mean > 8,]
hhwu.long <- hhwu.long[!hhwu.long$HHSize.Avg.1 > 8,]

# Make mixed-effects models
lmer1 <- lmer(Mass.Used.1 / HHSize.Avg.1 ~ Ethnic.Indic + Own.NW.Stove +
        HHSize.Avg.1 + (1 | ID.Full), hhwu.long)
lmer2 <- lmer(Mass.Used.1 / HHSize.Avg.1 ~ Ethnic.Indic + Own.NW.Stove +
        HHSize.Avg.1 + I(HHSize.Avg.1^2) + (1 | ID.Full), hhwu.long)
lmer3 <- lmer(Mass.Used.1 / HHSize.Avg.1 ~ Ethnic.Indic.New + Own.NW.Stove +
        HHSize.Avg.1 + I(HHSize.Avg.1^2) + (1 | ID.Full), hhwu.long)
# USE LMER4 MODEL BELOW. The old ethnic indicator is not defensible.
lmer4 <- lmer(Mass.Used.1 / HHSize.Avg.1 ~ Ethnic.Indic.New + Own.NW.Stove +
        HHSize.Avg.1 + (1 | ID.Full), hhwu.long)

test_data <- data.frame(HHSize.Avg.1=c(1:max(hhwu.long$HHSize.Avg.1)))
test_data <- cbind(test_data,
        Own.NW.Stove=rep(0, nrow(test_data)),
        Ethnic.Indic.New=rep(0, nrow(test_data)),
        Mass.Used.1=rep(0, nrow(test_data)))
mm <-  model.matrix(terms(lmer3), test_data)
pred_mass_used <- mm %*% fixef(lmer3)
lmer3_cor <- cor(hhwu.long$Mass.Used.1, fitted(lmer3))^2
qplot(test_data$HHSize.Avg.1, pred_mass_used, geom="line",
      xlab="Mean Household Size (number of persons)",
      ylab="Wood Mass (dry Kg / (psn * day))") +
      geom_point(aes(hhwu.long$HHSize.Avg.1, hhwu.long$Mass.Used.1/hhwu.long$HHSize.Avg.1, data=hhwu.long))
      #geom_point(aes(hhwu$HHSize.Avg.Mean, hhwu$Mass.Used.Mean/hhwu$HHSize.Avg.Mean, data=hhwu))
ggsave("hhsize_vs_fwusage_lmer4.png", width=WIDTH, height=HEIGHT, dpi=DPI)

# Create a linear model where all 5 measurements are averaged for each 
# household (as the 5 measurements are not independent).
lm1 <- lm(Mass.Used.Mean ~ HHSize.Avg.Mean, hhwu)
lm1.sq <- lm(Mass.Used.Mean ~ HHSize.Avg.Mean + I(HHSize.Avg.Mean^2), hhwu)
lm1.pp <- lm(Mass.Used.Mean / HHSize.Avg.Mean ~ HHSize.Avg.Mean, hhwu)
lm1.pp.sq <- lm(Mass.Used.Mean / HHSize.Avg.Mean ~ HHSize.Avg.Mean + I(HHSize.Avg.Mean^2), hhwu)
lm2 <- lm(Mass.Used.Mean ~ Own.NW.Stove, hhwu)
lm3 <- lm(Mass.Used.Mean ~ HHSize.Avg.Mean + Ethnic.Indic, hhwu)
lm4 <- lm(Mass.Used.Mean ~ HHSize.Avg.Mean + Own.NW.Stove, hhwu)
lm5 <- lm(Mass.Used.Mean ~ HHSize.Avg.Mean + Ethnic.Indic + Own.NW.Stove, hhwu)
lm5.pp <- lm(Mass.Used.Mean / HHSize.Avg.Mean ~ HHSize.Avg.Mean + Ethnic.Indic + Own.NW.Stove, hhwu)
lm6 <- lm(Mass.Used.Mean ~ HHSize.Avg.Mean + Ethnic.Indic + Own.NW.Stove + Psn.Age.Mean, hhwu)
lm7 <- lm(Mass.Used.Mean / HHSize.Avg.Mean ~ HHSize.Avg.Mean + Ethnic.Indic + Own.NW.Stove, hhwu)
lm8 <- lm(Mass.Used.Mean / HHSize.Avg.Mean ~ HHSize.Avg.Mean + Ethnic.Indic + Own.NW.Stove + I(HHSize.Avg.Mean^2), hhwu)
lm9 <- lm(Mass.Used.Mean / HHSize.Avg.Mean ~ HHSize.Avg.Mean + Ethnicity + Own.NW.Stove, hhwu)
lm10 <- lm(Mass.Used.Mean / HHSize.Avg.Mean ~ HHSize.Avg.Mean + Ethnic.Indic.New + Own.NW.Stove + I(HHSize.Avg.Mean^2), hhwu)
lm11 <- lm(Mass.Used.Mean / HHSize.Avg.Mean ~ HHSize.Avg.Mean + Ethnicity + Own.NW.Stove + I(HHSize.Avg.Mean^2), hhwu)

###############################################################################
# Now, make plots for PowerPoint
#
# Make a histogram of household sizes
qplot(hhwu$HHSize.Avg.Mean, geom="histogram", binwidth=1,
        xlab="Household Size (number of persons)", ylab="Frequency",
        ylim=c(0,12), xlim=c(0,11))
ggsave("hhsizes_hist.png", width=WIDTH, height=HEIGHT, dpi=DPI)

# Make a histogram of wood usage
qplot(hhwu$Mass.Used.Mean/hhwu$HHSize.Avg.Mean, geom="histogram", binwidth=.75,
        xlab="Wood Usage (dry Kg / (psn * day))", ylab="Frequency", xlim=c(0,5))
ggsave("fwusage_hist.png", width=WIDTH, height=HEIGHT, dpi=DPI)

# Make a plot of household sizes vs wood usage with fit
#annotation.eqn <- paste("FW Mass = ", round(lm1$coefficients[1], digits=2), " + ",
#        round(lm1$coefficients[2], digits=2),"×HHSize\n", sep="")
#annotation.R2 <- expression(R[adj]^{2} == ".10")

## Plot an example prediction from model lm10
test_data <- data.frame(HHSize.Avg.Mean=c(1:10))
test_data <- cbind(test_data,
        Ethnic.Indic.New=as.factor(rep(1, nrow(test_data))),
        Own.NW.Stove=as.factor(rep(1, nrow(test_data))),
        Mass.Used.Mean=rep(0, nrow(test_data)))
test_data_end$HHSize.Avg.Mean <- 6
qplot(test_data$HHSize.Avg.Mean, predict(lm10, test_data), geom="line",
      xlab="Mean Household Size (number of persons)",
      ylab="Wood Mass (dry Kg / (psn * day))") +
      geom_point(aes(hhwu$HHSize.Avg.Mean, hhwu$Mass.Used.Mean/hhwu$HHSize.Avg.Mean, data=hhwu))
ggsave("hhsize_vs_fwusage_lm10_raw.png", width=WIDTH, height=HEIGHT, dpi=DPI)

 Plot an example prediction from model lm10
test_data <- data.frame(HHSize.Avg.Mean=c(1:6))
test_data <- cbind(test_data,
        Ethnic.Indic.New=as.factor(rep(1, nrow(test_data))),
        Own.NW.Stove=as.factor(rep(1, nrow(test_data))),
        Mass.Used.Mean=rep(0, nrow(test_data)))
test_data_end <- test_data[1:6,] # Used to plot flat line for fw_usage after 7 psn hhize
test_data_end$HHSize.Avg.Mean <- 6
qplot(test_data$HHSize.Avg.Mean, predict(lm10, test_data), geom="line",
      xlab="Mean Household Size (number of persons)",
      ylab="Wood Mass (dry Kg / (psn * day))") +
      geom_line(aes(c(6:11), predict(lm10, test_data_end))) + 
      geom_point(aes(hhwu$HHSize.Avg.Mean, hhwu$Mass.Used.Mean/hhwu$HHSize.Avg.Mean, data=hhwu))
ggsave("hhsize_vs_fwusage_lm10_held_constant.png", width=WIDTH, height=HEIGHT, dpi=DPI)

# Make a plot of household sizes vs wood usage with fit, on a per-person basis
#annotation.eqn <- paste("FW Mass = ", round(lm1.pp.sq$coefficients[1], digits=2),
#        round(lm1.pp.sq$coefficients[2], digits=2),"×HHSize",
#        " + ", round(lm1.pp.sq$coefficients[3], digits=2),"×HHSize^2\n", 
#        sep="")
#annotation.R2 <- expression(R[adj]^{2} == ".15"), cex=.8)
qplot(HHSize.Avg.Mean, Mass.Used.Mean/HHSize.Avg.Mean, data=hhwu,
        xlab="Mean Household Size (number of persons)",
        ylab="Wood Mass (dry Kg / (psn * day))") +
        geom_line(data=hhwu, aes(HHSize.Avg.Mean, predict(lmer3)))
ggsave("hhsize_vs_fwusage_pp.png", width=WIDTH, height=HEIGHT, dpi=DPI)

qplot(HHSize.Avg.Mean, Mass.Used.Mean/HHSize.Avg.Mean, geom="point", data=hhwu,
        xlab="Mean Household Size (number of persons)",
        ylab="Wood Mass (dry Kg / (psn * day))") + geom_smooth()
ggsave("hhsize_vs_fwusage_smoothed.png", width=WIDTH, height=HEIGHT, dpi=DPI)
