load("V:/Nepal/ICPSR_0538_Restricted/Recode/recoded_NBH_data.Rdata")

# Now make linear interpolation from month 1 up to month LAST_MONTH, in wide 
# format, for each neighborhood. Note that 40 months is the average time 
# between the T1 and T2 mapping.
FIRST_MONTH_interp1 <- 1
LAST_MONTH_interp1 <- 40
N_MONTHS_interp1 <- LAST_MONTH_interp1 - FIRST_MONTH_interp1 + 1
rt_chg <- (nbh_recode$percagveg_t2 - nbh_recode$percagveg_t1)/40
rt_chg_matrix <- matrix(rep(rt_chg, LAST_MONTH_interp1), nrow=nrow(nbh_recode))
initial_percagveg <- matrix(rep(nbh_recode$percagveg_t1, ncol(rt_chg_matrix)), ncol=ncol(rt_chg_matrix))
months_matrix <- matrix(seq(1, ncol(rt_chg_matrix)), ncol=ncol(rt_chg_matrix), 
                        nrow=nrow(nbh_recode), byrow=TRUE)
interp_percagveg_1 <- initial_percagveg + (rt_chg_matrix * months_matrix)
interp_percagveg_1[interp_percagveg_1<0] <- 0
interp_percagveg_1[interp_percagveg_1>100] <- 100
interp_percagveg_1 <- data.frame(interp_percagveg_1)
names(interp_percagveg_1) <- paste("interp_percagveg", 
                                   seq(FIRST_MONTH_interp1, LAST_MONTH_interp1), 
                                   sep="")


FIRST_MONTH_interp2 <- LAST_MONTH_interp1 + 1
LAST_MONTH_interp2 <- 126
N_MONTHS_interp2 <- LAST_MONTH_interp2 - FIRST_MONTH_interp2 + 1
rt_chg <- (nbh_recode$percagveg_t3 - nbh_recode$percagveg_t2) / N_MONTHS_interp2
rt_chg_matrix <- matrix(rep(rt_chg, N_MONTHS_interp2), nrow=nrow(nbh_recode))
initial_percagveg <- matrix(rep(nbh_recode$percagveg_t2, ncol(rt_chg_matrix)), 
                            ncol=ncol(rt_chg_matrix))
months_matrix <- matrix(seq(1, ncol(rt_chg_matrix)), ncol=ncol(rt_chg_matrix), 
                        nrow=nrow(nbh_recode), byrow=TRUE)
interp_percagveg_2 <- initial_percagveg + (rt_chg_matrix * months_matrix)
interp_percagveg_2[interp_percagveg_2<0] <- 0
interp_percagveg_2[interp_percagveg_2>100] <- 100
interp_percagveg_2 <- data.frame(interp_percagveg_2)
names(interp_percagveg_2) <- paste("interp_percagveg", 
                                   seq(FIRST_MONTH_interp2, LAST_MONTH_interp2), 
                                   sep="")

interp_percagveg<- data.frame(NEIGHID=nbh_recode$NEIGHID, interp_percagveg_1,
                              interp_percagveg_2)

save(interp_percagveg, file="interpolated_percent_agveg.Rdata")
write.csv(interp_percagveg, file="interpolated_percent_agveg.csv", row.names=FALSE)
