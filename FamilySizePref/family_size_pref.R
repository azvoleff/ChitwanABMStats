###############################################################################
# Loads the t1 individual interview data, and calculates desired number of 
# children (for initializing the model).
###############################################################################
library(Hmisc) # contains label function
library(ggplot2) # contains label function

load("/media/Local_Secure/CVFS_R_format/t1indiv.Rdata")
t1indiv$gender <- factor(t1indiv$gender, labels=c("male", "female"))

make.txtprob <- function(probs, binlims, param.name) {
    # param.name is the name used by the ChitwanABM model for this parameter.
    binlims <- paste(round(binlims, digits=4), collapse=", ")
    probs <- paste(round(probs, digits=4), collapse=", ")
    txtprob <- paste("'", param.name, "' : [((", binlims, "), (", probs,  "))] | validate_prob_dist]", sep="")
    return(txtprob)
}

columns <- grep('gender|respid|f7$', names(t1indiv))

desnumchild <- t1indiv[columns]
names(desnumchild)[3] <- "numchild"
# People who said "it is god's will" were coded as 97, and reasked the 
# question, in f9.
godswill <- which(desnumchild$numchild==97)
desnumchild[godswill,]$numchild <- t1indiv$f9[godswill]
# Some still have "god's will", so recode these as -3 (don't know)
desnumchild$numchild[desnumchild$numchild==97] <- -3
# 2 people said a range from low to high. Here, arbitrarily, take the high 
# number, stored in f7b.
range <- which(desnumchild$numchild==95)
desnumchild[range,]$numchild <- t1indiv$f7b[range]
# 28 people said they don't know. This is coded as -3 in the CVFS data. Recode 
# this as -1.
desnumchild$numchild[desnumchild$numchild==-3] <- -1
# Also recode no response given (NA in the dataset) as -1
desnumchild$numchild[is.na(desnumchild$numchild)] <- -1
save(desnumchild, file="desnumchild.Rdata")

# Make a distribution to use in the model
numchild.prob <- data.frame(table(desnumchild$numchild))
names(numchild.prob) <- c("bin", "numchild")
numchild.prob$bin <- as.numeric(as.character(numchild.prob$bin))
numchild.prob <- cbind(numchild.prob, prob=(numchild.prob$numchild/sum(numchild.prob$numchild)))
write(make.txtprob(numchild.prob$prob, c(numchild.prob$bin, 10),
        "prob.num.children.desired"), file="prob.num.children.desired.txt")

qplot(numchild, facets=gender~., geom="histogram", 
        xlab="Desired Number of Children", ylab="Count", binwidth=1,
        data=desnumchild)
ggsave("desnumchild.png", width=8.33, height=5.53, dpi=300)
