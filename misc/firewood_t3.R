###############################################################################
# Makes plots of the quantity of fuelwood collected (using t3 data).
###############################################################################

library(Hmisc)

pdf(file="firewood_t3_new.pdf", paper="letter")
par(mfrow=c(2,1))

t3hhca <- sasxport.get("/media/Restricted/Data/ICPSR_SupplementalData/Survey_conv/t3hhca.xpt")

# First plot total amount and days of firewood gathering last year
# Exclude those who did not use firewood last year.
firewoodusers <- t3hhca[t3hhca$t3e15==1,]

firewoodusers <- na.omit(firewoodusers)

hist(firewoodusers$t3e15.1a, main="Number of bari firewood used last year")
title(sub=paste("Mean:", round(mean(firewoodusers$t3e15.1a), digits=4), "Median:", median(firewoodusers$t3e15.1a), " Std. ", round(sd(firewoodusers$t3e15.1a), digits=4)))

hist(firewoodusers$t3e15.1b, main="Number of carts firewood used last year")
title(sub=paste("Mean:", round(mean(firewoodusers$t3e15.1b), digits=4), "Median:", median(firewoodusers$t3e15.1b), " Std. ", round(sd(firewoodusers$t3e15.1b), digits=4)))

hist(firewoodusers$t3e15.1c, main="Number of quintal firewood used last year")
title(sub=paste("Mean:", round(mean(firewoodusers$t3e15.1c), digits=4), "Median:", median(firewoodusers$t3e15.1c), " Std. ", round(sd(firewoodusers$t3e15.1c), digits=4)))

hist(firewoodusers$t3e26, main="Number of workdays gathering firewood last year")
title(sub=paste("Mean:", round(mean(firewoodusers$t3e26), digits=4), "Median:", median(firewoodusers$t3e26), " Std. ", round(sd(firewoodusers$t3e26), digits=4)))

# Now plot wood and firewood gathered the last time the national park was 
# opened, and how many people and how many days did they go.
# Exclude those who did not go to the national park during the last GCP.
gcpusers <- t3hhca[t3hhca$t3e37==TRUE,]

hist(gcpusers$t3e39a, main="Wood brought in last GCP (bhari)")
title(sub=paste("Mean:", round(mean(gcpusers$t3e39a), digits=4), "Median:", median(gcpusers$t3e39a), " Std. ", round(sd(gcpusers$t3e39a), digits=4)))

hist(gcpusers$t3e39b, main="Wood brought in last GCP (carts)")
title(sub=paste("Mean:", round(mean(gcpusers$t3e39b), digits=4), "Median:", median(gcpusers$t3e39b), " Std. ", round(sd(gcpusers$t3e39b), digits=4)))

hist(gcpusers$t3e40a, main="Firewood brought in last GCP (bhari)")
title(sub=paste("Mean:", round(mean(gcpusers$t3e40a), digits=4), "Median:", median(gcpusers$t3e40a), " Std. ", round(sd(gcpusers$t3e40a), digits=4)))

hist(gcpusers$t3e40b, main="Firewood brought in last GCP (carts)")
title(sub=paste("Mean:", round(mean(gcpusers$t3e40b), digits=4), "Median:", median(gcpusers$t3e40b), " Std. ", round(sd(gcpusers$t3e40b), digits=4)))

hist(gcpusers$t3e41a, main="Num people went to jungle in last GCP")
title(sub=paste("Mean:", round(mean(gcpusers$t3e41a), digits=4), "Median:", median(gcpusers$t3e41a), " Std. ", round(sd(gcpusers$t3e41a), digits=4)))

hist(gcpusers$t3e41b, main="Days went to jungle in last GCP")
title(sub=paste("Mean:", round(mean(gcpusers$t3e41b), digits=4), "Median:", median(gcpusers$t3e41b), " Std. ", round(sd(gcpusers$t3e41b), digits=4)))

dev.off()
