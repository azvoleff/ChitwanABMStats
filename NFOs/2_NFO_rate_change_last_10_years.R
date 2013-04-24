library(ggplot2)

load("nfos.Rdata")

nfo_types <- c('SCHLFT', 'HLTHFT', 'BUSFT', 'MARFT', 'EMPFT')
nfo_types_full_name <- c('school',
                         'health',
                         'bus stop',
                         'market',
                         'employer')
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

nfo_change <- cbind(NEIGHID=nfos$NEIGHID, nfo_change)
write.csv(nfo_change, file='NFO_change_mean_annual_minft.csv', row.names=FALSE)

for (n in 1:length(nfo_types)) {
    nfo_type <- nfo_types[n]
    low_dist_cutoff <- 10
    nfo_col_1996 <- grep(paste('^', nfo_type, '52$', sep=''), names(nfos))
    nfo_change_col <- grep(paste('^', nfo_type, '_change$', sep=''), names(nfo_change))
    low_1996_dist <- nfos[, nfo_col_1996] < low_dist_cutoff
    
    this_nfo_change <- nfo_change[, nfo_change_col]
    this_nfo_change_cut <- cut(this_nfo_change, right=FALSE,
                               breaks=c(-Inf, 5, -4, -3, -2, -1, 0, 1, Inf))
    this_nfo_change_cut <- as.character(this_nfo_change_cut)
    this_nfo_change_cut[this_nfo_change == 0] <- '0'
    this_nfo_change_cut[(this_nfo_change > 0) & (this_nfo_change < 1)] <- '(0, 1)'
    this_nfo_change_cut <- factor(this_nfo_change_cut, levels=c("[-Inf,-4)", "[-4,-3)", "[-3,-2)", "[-2,-1)", "[-1,0)", "0", "(0, 1)", "[1,Inf"))
    #TODO: Figure out where these NAs are coming from
    this_nfo_change_cut <- this_nfo_change_cut[!is.na(this_nfo_change_cut)]
    qplot(this_nfo_change_cut, geom='histogram', binwidth=1, 
          xlab=paste('Change in distance to', nfo_types_full_name, '(min. on foot / year)'), ylab='Count')
    ggsave(paste('change_in_', nfo_type, '.png', sep=''), width=8.33, 
           height=5.53, dpi=300)

    print(paste('Mean', nfo_type, 'in 1996 for', sum(!low_1996_dist), 'far NBHs:', mean(nfos[, nfo_col_1996][!low_1996_dist])))
    print(paste('Mean', nfo_type, 'in 1996 for', sum(low_1996_dist), 'close NBHs:', mean(nfos[, nfo_col_1996][low_1996_dist])))

}
