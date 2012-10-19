###############################################################################
# Loads the t1 individual interview data, and calculates desired number of 
# children (for initializing the model).
###############################################################################
library(Hmisc) # contains label function
library(ggplot2) # contains label function
library(foreign)

t1indiv <- read.xport("V:/Nepal/ICPSR_0538_Restricted/da04538-0012_REST.xpt")
t1indiv$GENDER <- factor(t1indiv$GENDER, labels=c("male", "female"))

make_txtprob <- function(probs, binlims, param.name) {
    # param.name is the name used by the ChitwanABM model for this parameter.
    binlims <- paste(round(binlims, digits=4), collapse=", ")
    probs <- paste(round(probs, digits=4), collapse=", ")
    txtprob <- paste("'", param.name, "' : [((", binlims, "), (", probs,  "))] | validate_prob_dist]", sep="")
    return(txtprob)
}

columns <- grep('GENDER|RESPID|F7$', names(t1indiv))

desnumchild <- t1indiv[columns]
names(desnumchild)[3] <- "numchild"
# People who said "it is god's will" were coded as 97, and reasked the 
# question, in F9.
godswill <- which(desnumchild$numchild==97)
desnumchild[godswill,]$numchild <- t1indiv$F9[godswill]
# Some still have "god's will", so recode these as -3 (don't know)
desnumchild$numchild[desnumchild$numchild==97] <- -3
# 2 people said a range from low to high. Here, arbitrarily, take the high 
# number, stored in F7B.
range <- which(desnumchild$numchild==95)
desnumchild[range,]$numchild <- t1indiv$F7B[range]
# 28 people said they don't know. This is coded as -3 in the CVFS data. Recode 
# this as -1.
desnumchild$numchild[desnumchild$numchild==-3] <- -1
# Also recode no response given (NA in the dataset) as -1
desnumchild$numchild[is.na(desnumchild$numchild)] <- -1
save(desnumchild, file="desnumchild.Rdata")

# Make a distribution to use in the model
numchild_prob <- data.frame(table(desnumchild$numchild))
names(numchild_prob) <- c("bin", "numchild")
numchild_prob$bin <- as.numeric(as.character(numchild_prob$bin))
numchild_prob <- cbind(numchild_prob, prob=(numchild_prob$numchild/sum(numchild_prob$numchild)))
write(make_txtprob(numchild_prob$prob, c(numchild_prob$bin, 10),
        "prob.num.children.desired"), file="prob.num.children.desired.txt")
write.csv(numchild_prob, file="desnumchild_probs.csv", row.names=FALSE)
qplot(numchild, facets=GENDER~., geom="histogram", 
        xlab="Desired Number of Children", ylab="Count", binwidth=1,
        data=desnumchild)
ggsave("desnumchild.png", width=8.33, height=5.53, dpi=300)

numchild_low_prob <- numchild_prob
# Eliminate people wanting more than 5 children, and double probability of 
# zero, one, or two:
numchild_low_prob$prob <- c(0.0125, 0.0011, 0.5896, 0.2715, 0.1660, 
                            0.106, 0.055, .01, .005, .001, .001)
numchild_low_prob$prob <- numchild_low_prob$prob / sum(numchild_low_prob$prob)
write(make_txtprob(numchild_low_prob$prob, c(numchild_low_prob$bin, 10),
        "prob.num.children.desired"), file="prob.num.children.desired_low.txt")
qplot(bin, prob*100, xlab="Desired Number of Children",
        ylab="Probability (%)", data=numchild_low_prob, binwidth=1)
ggsave("prob_first_birth_low.png", width=8.33, height=5.53, dpi=300)

numchild_high_prob <- numchild_prob
# Eliminate people wanting more than 5 children, and double probability of 
# zero, one, or two:
numchild_high_prob$prob <- c(0.0125, 0.0011, 0.0317, 0.1, 0.215, 0.5660, 
                            0.346, 0.15, 0.054, 0.02, 0.013)
numchild_high_prob$prob <- numchild_high_prob$prob / sum(numchild_high_prob$prob)
write(make_txtprob(numchild_high_prob$prob, c(numchild_high_prob$bin, 10),
        "prob.num.children.desired"), file="prob.num.children.desired_high.txt")
qplot(bin, prob*100, xlab="Desired Number of Children",
        ylab="Probability (%)", data=numchild_high_prob, binwidth=1)
ggsave("prob_first_birth_high.png", width=8.33, height=5.53, dpi=300)
