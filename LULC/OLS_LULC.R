###############################################################################
# This calculates an OLS model land use, broken into 3 categories, based on the 
# calculations in Table 5 of Axinn and Ghimire, PSC Research Report 07-617 
# (2007). It uses several household and neighborhood level predictors.
#
# LULC categories:
# 	Buildings and infrastructure
# 	Agricultural land area
# 	Non-agricultural land area

require(foreign)

load("CBR_nbh.Rdata")

t1.hhag <- read.xport("/media/Local_Secure/ICPSR_0538_Restricted/da04538-0002_REST.xpt")
t1.indivcensus <- read.xport("/media/Local_Secure/ICPSR_0538_Restricted/da04538-0004_REST.xpt")
lulc <- read.xport("/media/Local_Secure/ICPSR_SupplementalData/Survey_converted/landuse.xpt")
lulc <- lulc[order(names(lulc))]
lulc <- lulc[c(8, 24, 49, 1:7, 9:23, 25:48, 50:62)]

t1.indivhist <- read.xport("/media/Local_Secure/ICPSR_0538_Restricted/da04538-0014_REST.xpt")
