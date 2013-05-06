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
