library(ggplot2)

theme_set(theme_grey(base_size=40))
theme_set(theme_bw(base_size=40))
#theme_update(panel.grid.major=element_blank(), panel.grid.minor=element_blank())
update_geom_defaults("smooth", aes(size=1))
update_geom_defaults("line", aes(size=1))
# The four below margins are the page margins in inches - by default figures 
# will be sized up to half the column width and 1/3 the page height )minus the 
# caption_space) at 300 DPI.
right_mar = 1.25
left_mar = 1.5
up_mar = 1.25
low_mar = 1.25
# How much space to leave for a caption (in inches) on a full page 2 col x 3 
# row multiplot?
caption_space <- 1
PLOT_WIDTH = (8.5 - left_mar - right_mar) / 2
PLOT_HEIGHT = (11 - up_mar - low_mar - caption_space) / 3
PLOT_DPI = 300

format_p <- function(p_val) {
    if (p_val < .001) {
        return('<.001')
    } else {
        return(round(p_val, digits=3))
    }
}

###############################################################################
# Function for adding equation to ggplot facets, plots modified from:  
# http://bit.ly/11SrhE2, and from http://bit.ly/10zlf8f
eqnfunc <- function(d, model_formula, two_lines=FALSE) {
    m <- lm(formula(model_formula), data=d)
    l <- list(y=names(m$model)[1],
              x=names(m$model)[2],
              a=format(coef(m)[1], digits=2),
              b=format(abs(coef(m)[2]), digits=2),
              r2=format(summary(m)$adj.r.squared, digits=2),
              pslope=format_p(summary(m)$coefficients[2, 4]))
    if (two_lines) {
        if (coef(m)[2] >= 0)  {
            eq <- substitute(atop(italic(y) == a + b %.% italic(x), italic(r)["adj"]^2~"="~r2), l)
        } else {
            eq <- substitute(atop(italic(y) == a - b %.% italic(x), italic(r)["adj"]^2~"="~r2), l)
        }
    } else {
        if (coef(m)[2] >= 0)  {
            eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)["adj"]^2~"="~r2, l)
        } else {
            eq <- substitute(italic(y) == a - b %.% italic(x)*","~~italic(r)["adj"]^2~"="~r2, l)
        }
    }
    c(eqn=as.character(as.expression(eq)))
}

###############################################################################
# Function for adding equation to ggplot facets, plots modified from:  
# http://bit.ly/11SrhE2, and from http://bit.ly/10zlf8f
eqnfunc_slope <- function(d, model_formula, two_lines=FALSE) {
    m <- lm(formula(model_formula), data=d)
    l <- list(b=format(abs(coef(m)[2]), digits=2),
              pslope=format_p(summary(m)$coefficients[2, 4]))
    if (coef(m)[2] >= 0)  {
        eq <- substitute("slope" == b*","*~~"p"[slope]==~pslope, l)
    } else {
        eq <- substitute("slope" == - b*","*~~"p"[slope]==~pslope, l)
    }
    c(eqn=as.character(as.expression(eq)))
}

###############################################################################
# Function to calculate if data values exceed (or are lower than) a certain 
# percentile.
is_extreme <- function(data_vec, prob, greater=TRUE, data_subset=NULL) {
    if (is.null(data_subset)) {data_subset<- rep(TRUE, length(data_vec))}
    if (greater) {
        return(data_vec > quantile(data_vec[data_subset], prob=prob/100, na.rm=TRUE))
    } else {
        return(data_vec < quantile(data_vec[data_subset], prob=prob/100, na.rm=TRUE))
    }
}
