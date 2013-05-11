library(foreign)

nbht2_all <- read.dta("W:/Nepal/20120704_ICPSR_04538/DS0022/04538-0022-Data-REST.dta")

nbht2 <- data.frame(NEIGHID=nbht2_all$NEIGHID)

non_fam_58 <- nbht2_all[grep('^(SCHLFT58|HLTHFT58|BUSFT58|EMPFT58|MARFT58)$', 
                         names(nbht2_all))]
# Nepali year 2058 is equivalent to Western date 2001/2002
names(non_fam_58) <- sub('58', '_2001', names(non_fam_58))
nbht2 <- cbind(nbht2, non_fam_58)

nbht2$ELEC_2001 <- nbht2_all$ELEC58

save(nbht2, file="nbh_t2.Rdata")
write.csv(nbht2, file="nbh_t2.csv", row.names=FALSE)
