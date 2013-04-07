library(ggplot2)

load("nfos.Rdata")

nfo_types <- c('SCHLFT', 'HLTHFT', 'BUSFT', 'MARFT', 'EMPFT')
nfo_change <- c()
for (nfo_type in nfo_types) {
    # Below is for change over last 20 years
    nfo_cols <- grep(paste('^', nfo_type, '((3[2-9])|(4[0-9])|(5[0-2]))$', 
                           sep=''), names(nfos))
    # Below is for change over last 10 years
    #nfo_cols <- grep('^EMPFT((4[2-9])|(5[0-2]))$', names(nfos))
    mean_change <- rowMeans(nfos[nfo_cols[2:(length(nfo_cols))]] - nfos[nfo_cols[1:(length(nfo_cols)-1)]])
    nfo_change <- c(nfo_change, list(mean_change))
}
nfo_change <- data.frame(matrix(unlist(nfo_change), ncol=length(nfo_types)))
names(nfo_change) <- paste(nfo_types, 'change', sep='_')

apply(nfo_change, 2, function(x) sum(x == 0))
apply(nfo_change, 2, function(x) sum(x < 0))
apply(nfo_change, 2, function(x) sum(x > 0))

write.csv(nfo_change, file='NFO_change_mean.csv')

par(ask=T)
for (nfo_type in nfo_types) {
    low_dist_cutoff <- 10
    nfo_col_1996 <- grep(paste('^', nfo_type, '52$', sep=''), names(nfos))
    nfo_change_col <- grep(paste('^', nfo_type, '_change$', sep=''), names(nfo_change))
    low_1996_dist <- nfos[, nfo_col_1996] < low_dist_cutoff

    print(qplot(nfo_change[, nfo_change_col], geom='histogram', binwidth=1, 
          xlab=paste('Change in', nfo_type), facets=low_1996_dist~.))

    print(paste('Mean', nfo_type, 'in 1996 for', sum(!low_1996_dist), 'far NBHs:', mean(nfos[, nfo_col_1996][!low_1996_dist])))
    print(paste('Mean', nfo_type, 'in 1996 for', sum(low_1996_dist), 'close NBHs:', mean(nfos[, nfo_col_1996][low_1996_dist])))

}
