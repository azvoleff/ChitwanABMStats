##############################################################################
# Reads in household wood usage data, cleans it, and sets it up for use in R.  
# The resulting data is saved in binary R format.
###############################################################################

require(lme4)

require(ggplot2)
theme_update(theme_grey(base_size=18))
update_geom_defaults("line", aes(size=1))

load("hhwu1.Rdata")
load("hhwu2.Rdata")

# TODO: Add in own livestock???

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

# Make a "own stove other than gas" variable
Own.NW.Stove <- with(hhwu, apply(cbind(Stove.LPG, Stove.Kerosene,
        Stove.Biogas, Stove.Electric, Stove.Wooddust, Stove.Other), 1, sum))
Own.NW.Stove[Own.NW.Stove > 1] <- 1
Own.NW.Stove <- factor(Own.NW.Stove)

# Make an ethnicity indicator variable
Ethnic.Indic <- as.numeric(hhwu$Ethnicity)
Ethnic.Indic[Ethnic.Indic != 2] <- 0
Ethnic.Indic[Ethnic.Indic == 2] <- 1
Ethnic.Indic <- factor(Ethnic.Indic)

hhwu <- cbind(hhwu, Own.NW.Stove, Ethnic.Indic)

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

# Make mixed-effects models
lmer1 <- lmer(Mass.Used.1 ~ (HHSize.Avg.1 | ID.Full), hhwu.long)
# Look at a crude R^2 measure:
#cor(hhwu.long$Mass.Used.1, fitted(lmer1))^2
lmer2 <- lmer(Mass.Used.1 ~ Ethnicity + Stove.Wood +
        (HHSize.Avg.1 | ID.Full), hhwu.long)
lmer3 <- lmer(Mass.Used.1 ~ Ethnic.Indic + Own.NW.Stove +
        (HHSize.Avg.1 | ID.Full), hhwu.long)
lmer4 <- lmer(Mass.Used.1 ~ Period + Ethnic.Indic + Own.NW.Stove +
        (HHSize.Avg.1 | ID.Full), hhwu.long)

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
lm5.pp.sq <- lm(Mass.Used.Mean / HHSize.Avg.Mean ~ HHSize.Avg.Mean + Ethnic.Indic + Own.NW.Stove + I(HHSize.Avg.Mean^2), hhwu)
lm6 <- lm(Mass.Used.Mean ~ HHSize.Avg.Mean + Ethnic.Indic + Own.NW.Stove + Psn.Age.Mean, hhwu)


# lm5 is the best fit.  Try the fit again with the huge household 
# (HHSize.Avg.Mean=10.8) removed:
outlierrow <- which(hhwu$HHSize.Avg.Mean == max(hhwu$HHSize.Avg.Mean))
hhwu.rmoutlier <- hhwu[-outlierrow,]
lm5.rmooutlier <- lm(Mass.Used.Mean ~ HHSize.Avg.Mean + Ethnic.Indic + Own.NW.Stove, hhwu.rmoutlier)
lm1.pp.rmoutlier <- lm(Mass.Used.Mean / HHSize.Avg.Mean ~ HHSize.Avg.Mean, hhwu.rmoutlier)
lm1.pp.sq.rmoutlier <- lm(Mass.Used.Mean / HHSize.Avg.Mean ~ HHSize.Avg.Mean + I(HHSize.Avg.Mean^2), hhwu.rmoutlier)
lm1.rmoutlier <- lm(Mass.Used.Mean ~ HHSize.Avg.Mean, hhwu.rmoutlier)
lm1.rmoutlier.pp <- lm(Mass.Used.Mean/HHSize.Avg.Mean ~ HHSize.Avg.Mean, hhwu.rmoutlier)
lm1.rmoutlier.pp.sq <- lm(Mass.Used.Mean/HHSize.Avg.Mean ~ HHSize.Avg.Mean + I(HHSize.Avg.Mean^2), hhwu.rmoutlier)
lm5.rmoutlier.pp.sq <- lm(Mass.Used.Mean / HHSize.Avg.Mean ~ HHSize.Avg.Mean + Ethnic.Indic + Own.NW.Stove + I(HHSize.Avg.Mean^2), hhwu.rmoutlier)
# The fit is roughly the same. Also, when tested, none of the interaction terms 
# are significant.


###############################################################################
# Now, make plots for powerpoint
#
# Make a histogram of household sizes

# mai is specifed as c(bottom, left, top, right)
qplot(hhwu$HHSize.Avg.Mean, geom="histogram", binwidth=1,
        xlab="Household Size (number of persons)", ylab="Frequency",
        ylim=c(0,12), xlim=c(0,11))
ggsave("hhsizes_hist.png", width=8.33, height=5.53, dpi=300)

# Make a histogram of wood usage
qplot(hhwu$Mass.Used.Mean, geom="histogram", binwidth=.75,
        xlab="Wood Usage (dry Kg / (HH * day))", ylab="Frequency", xlim=c(0,5))
ggsave("fwusage_hist.png", width=8.33, height=5.53, dpi=300)

# Make a plot of household sizes vs wood usage with fit
#annotation.eqn <- paste("FW Mass = ", round(lm1$coefficients[1], digits=2), " + ",
#        round(lm1$coefficients[2], digits=2),"×HHSize\n", sep="")
#annotation.R2 <- expression(R[adj]^{2} == ".10")
ggplot() + geom_point(data=hhwu, aes(HHSize.Avg.Mean, hhwu$Mass.Used.Mean)) +
        geom_line(data=hhwu, aes(HHSize.Avg.Mean, predict(lm1))) + 
        scale_x_continuous(name="Mean Household Size (number of persons)") +
        scale_y_continuous(name="Wood Mass (dry Kg / (HH * day))")
ggsave("hhsize_vs_fwusage.png", width=8.33, height=5.53, dpi=300)


# Make a plot of household sizes vs wood usage with fit, on a per-person basis
#annotation.eqn <- paste("FW Mass = ", round(lm1.pp.sq$coefficients[1], digits=2),
#        round(lm1.pp.sq$coefficients[2], digits=2),"×HHSize",
#        " + ", round(lm1.pp.sq$coefficients[3], digits=2),"×HHSize^2\n", 
#        sep="")
#annotation.R2 <- expression(R[adj]^{2} == ".15"), cex=.8)
ggplot() + geom_point(data=hhwu.rmoutlier, aes(HHSize.Avg.Mean,
        Mass.Used.Mean/HHSize.Avg.Mean)) + geom_line(data=hhwu.rmoutlier,
        aes(HHSize.Avg.Mean, predict(lm1.rmoutlier.pp.sq))) +
        scale_x_continuous(name="Mean Household Size (number of persons)") +
        scale_y_continuous(name="Wood Mass (dry Kg / (psn * day))")
ggsave("hhsize_vs_fwusage_pp.png", width=8.33, height=5.53, dpi=300)
