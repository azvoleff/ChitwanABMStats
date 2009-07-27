#!/usr/bin/Rscript
# This R script calculates summary statistics (deaths per month, births per 
# month, migrations per month) from the household registry data so that the 
# model results can be compared with the actual stats from the Chitwan Valley 
# Family Study.
#
# NOTE: Try using sasxport.get from the Hmisc library. It is much better at 
# handling xport files than is read.xport, including handling types, and 
# factors, and understanding SAS labels.

library(foreign)

hhreg <- read.xport("/media/Restricted/Data/ICPSR_0538_Restricted/da04538-0010_REST.xpt")
# A death is coded in the household registry data as a 3 in LIVNG1-LIVNG54. Any 

livngs <- hhreg[grep('^LIVNG[0-9]*$', names(hhreg))]
ages <- hhreg[grep('^AGE[0-9]*$', names(hhreg))]

# New marriages:

# Divorces:

# Divorces:

# Migration:

# Deaths:
livngs <- hhreg[grep('^LIVNG[0-9]*$', names(hhreg))]
