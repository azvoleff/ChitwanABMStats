###############################################################################
# Loads the t1 individual interview data, and calculates desired number of 
# children (for initializing the model).
###############################################################################
library(Hmisc) # contains label function
load("/media/Local_Secure/CVFS_R_format/t1indiv.Rdata")
columns <- grep('respid|f7$', names(t1indiv))
desnumchild <- t1indiv[columns]
names(desnumchild)[2] <- "numchild"
# People who said "it is god's will" were coded as 97, and reasked the 
# question, in f9.
godswill <- which(desnumchild$numchild==97)
desnumchild[godswill,]$numchild <- desnumchild$f9[godswill]
# 2 people said a range from low to high. Here, arbitrarily, take the high 
# number, stored in f7b.
range <- which(desnumchild$numchild==95)
desnumchild[range,]$numchild <- desnumchild$f7b[range]
# 28 people said they don't know. This is coded as -3 in the CVFS data. Recode 
# this as -1.
desnumchild$numchild[desnumchild$numchild==-3] <- -1
# Also recode no response given (NA in the dataset) as -1
desnumchild$numchild[is.na(desnumchild$numchild)] <- -1
# TODO: Also there are 22 individuals with # kids wanted in the thousands...  
# ask Dirgha what these are
desnumchild$numchild[desnumchild$numchild>1000] <- -1
save(desnumchild, file="desnumchild.Rdata")
