###
# Pulls out data needed to construct person, household, and neighborhood agents 
# for initializing the agent based model.
###

library("foreign")

# DS0003 is the PUBLIC household census from 1996
DS0003 <- read.xport("/media/Public_Data/ICPSR_04538/DS0003/04538-0003-Data.xpt")
DS0002 <- read.xport("/media/Public_Data/ICPSR_04538/da04538-0002_REST.xpt")
DS0010 <- read.xport("/media/Public_Data/ICPSR_04538/da04538-0010_REST.xpt")
DS0014 <- read.xport("/media/Public_Data/ICPSR_04538/da04538-0014_REST.xpt")
