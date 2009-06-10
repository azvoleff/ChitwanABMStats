###############################################################################
# Makes plots of the quantity of fuelwood collected (using t3 data).
###############################################################################

pdf(file="firewood_t3.pdf", paper="letter")
par(mfrow=c(2,1))

t3hhca <- read.csv("/media/Restricted/Data/ICPSR_SupplementalData/Survey_conv/t3hhca.csv")

# First plot total amount and days of firewood gathering last year
# Exclude those who did not use firewood last year.
t3hhca$T3E15 <- as.logical(t3hhca$T3E15)
firewoodusers <- t3hhca[t3hhca$T3E15==TRUE,]

firewoodusers$T3E15_1c <- as.numeric(firewoodusers$T3E15_1c)
hist(firewoodusers$T3E15_1c, main="Number of quintal firewood used last year")
title(sub=paste("Mean:", round(mean(firewoodusers$T3E15_1c), digits=4), "Median:", median(firewoodusers$T3E15_1c), " Std. ", round(sd(firewoodusers$T3E15_1c), digits=4)))

firewoodusers$T3E26 <- as.numeric(firewoodusers$T3E26)
hist(firewoodusers$T3E26, main="Number of workdays gathering firewood last year")
title(sub=paste("Mean:", round(mean(firewoodusers$T3E26), digits=4), "Median:", median(firewoodusers$T3E26), " Std. ", round(sd(firewoodusers$T3E26), digits=4)))

# Now plot wood and firewood gathered the last time the national park was 
# opened, and how many people and how many days did they go.
# Exclude those who did not go to the national park during the last GCP.
t3hhca$T3E37 <- as.logical(t3hhca$T3E37)
gcpusers <- t3hhca[t3hhca$T3E37==TRUE,]

gcpusers$T3E39a <- as.numeric(gcpusers$T3E39a)
hist(gcpusers$T3E39a, main="Wood brought in last GCP (bhari)")
title(sub=paste("Mean:", round(mean(gcpusers$T3E39a), digits=4), "Median:", median(gcpusers$T3E39a), " Std. ", round(sd(gcpusers$T3E39a), digits=4)))

gcpusers$T3E39b <- as.numeric(gcpusers$T3E39b)
hist(gcpusers$T3E39b, main="Wood brought in last GCP (carts)")
title(sub=paste("Mean:", round(mean(gcpusers$T3E39b), digits=4), "Median:", median(gcpusers$T3E39b), " Std. ", round(sd(gcpusers$T3E39b), digits=4)))

gcpusers$T3E40a <- as.numeric(gcpusers$T3E40a)
hist(gcpusers$T3E40a, main="Firewood brought in last GCP (bhari)")
title(sub=paste("Mean:", round(mean(gcpusers$T3E40a), digits=4), "Median:", median(gcpusers$T3E40a), " Std. ", round(sd(gcpusers$T3E40a), digits=4)))

gcpusers$T3E40b <- as.numeric(gcpusers$T3E40b)
hist(gcpusers$T3E40b, main="Firewood brought in last GCP (carts)")
title(sub=paste("Mean:", round(mean(gcpusers$T3E40b), digits=4), "Median:", median(gcpusers$T3E40b), " Std. ", round(sd(gcpusers$T3E40b), digits=4)))

gcpusers$T3E41a <- as.numeric(gcpusers$T3E41a)
hist(gcpusers$T3E41a, main="Num people went to jungle in last GCP")
title(sub=paste("Mean:", round(mean(gcpusers$T3E41a), digits=4), "Median:", median(gcpusers$T3E41a), " Std. ", round(sd(gcpusers$T3E41a), digits=4)))

gcpusers$T3E41b <- as.numeric(gcpusers$T3E41b)
hist(gcpusers$T3E41b, main="Days went to jungle in last GCP")
title(sub=paste("Mean:", round(mean(gcpusers$T3E41b), digits=4), "Median:", median(gcpusers$T3E41b), " Std. ", round(sd(gcpusers$T3E41b), digits=4)))

dev.off()
