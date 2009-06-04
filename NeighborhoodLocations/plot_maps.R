###############################################################################
# Plots Chitwan neighborhood locations and DEM.
###############################################################################

require("sp")

load(".Rdata")

pdf(file="maps.pdf", paper="letter")
par(mar=c(0, 0, 1, 0))
par(mfrow=c(2,2))
image(DEM, col=terrain.colors(30))
contour(DEM, col="black", add=TRUE)
points(hs, cex=.5, pch=19)
title(main="Wolong topography")

image(landclass, col=terrain.colors(30))
points(hs, cex=.5, pch=19)
title(main="Wolong land-use")

image(slope, col=terrain.colors(30))
points(hs, cex=.5, pch=19)
title(main="Wolong slope")
dev.off()
