#!/usr/bin/env Rscript
# Plots population pyramids from the household registry data
require(epicalc)
require(Hmisc)
require(ggplot2, quietly=TRUE)

# Load the hhreg dataframe (household registry data)
load("/media/Local_Secure/CVFS_HHReg/hhreg126.Rdata")
hhreg$gender <- factor(hhreg$gender, labels=c("male", "female"))

place.cols <- grep('^place[0-9]*$', names(hhreg))
age.cols <- grep('^age[0-9]*$', names(hhreg))
gender <- hhreg$gender

# Don't want to make a ridiculous number of plots - so just plot every 12th 
# month.

place.cols <- place.cols[c(TRUE, rep(FALSE,11))]
age.cols <- age.cols[c(TRUE, rep(FALSE,11))]
pdf("CVFS_hhreg_pop_pyramids.pdf")
for (n in 1:length(place.cols)) {
    hhregmonth <- gsub("^place", "", names(hhreg[place.cols[n]]))
    age <- hhreg[,age.cols[n]]

    place <- hhreg[,place.cols[n]]
    place[place > 151 | place < 1] <- FALSE
    place[place <= 151 & place >= 1] <- TRUE

    this_month <- data.frame(place=place, gender, age=age)
    this_month <- this_month[place==1,]
    this_month <- na.omit(this_month)

    pyramid(this_month$age, this_month$gender, main=paste("Month", hhregmonth, sep=" "))
}
dev.off()
