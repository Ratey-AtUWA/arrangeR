# library(geoR)
library(sp)
utm50S <- CRS("+proj=utm +zone=50 +south")
boundary <- read.csv("sv_boundary.csv")
spacing <- 30
offset <- 22
n <- 40
# make rectangular grid
extremes <- c(min(floor(boundary[,1])),
              min(floor(boundary[,2])),
              max(ceiling(boundary[,1])),
              max(ceiling(boundary[,2])))
grid_big <- expand.grid(seq(extremes[1], 
                            extremes[3], by = spacing) + offset,
                        seq(extremes[2], 
                            extremes[4], by = spacing) + offset)
gridSP <- SpatialPoints(grid_big, proj4string = utm50S)

# make spatial polygon(s) and find ponts inside it
irregpoly <- Polygon(boundary[,c(1,2)], hole=F)
irregPolys = Polygons(list(irregpoly),1)
irregSpatialPolys = SpatialPolygons(list(irregPolys), 
                                    proj4string = CRS("+proj=utm +zone=50 +south"))

inOrOut <- as.vector(over(gridSP, irregSpatialPolys))
inside <- gridSP[which(inOrOut>0)]

# calculate rectangle and polygon areas
area <- irregpoly@area
maxarea <- (extremes[3] - extremes[1]) * (extremes[4] - extremes[2])

# plot the results
par(mfrow = c(1,1), mar = c(3,3,1,1), mgp = c(1.6, 0.3, 0), 
    tcl = 0.25, font.lab = 2, lend=1, ljoin=2, cex.axis = 0.8)
with(boundary, plot(Easting, Northing, asp = 1, type = "l"))
rect(extremes[1], extremes[2], 
     extremes[3], extremes[4], 
     border = "chocolate", lty = 2)
borders = boundary[, c(1, 2)]
points(inside, col = "purple", pch = 3, cex = 0.8)

legend("bottomleft", inset = 0.05, y.intersp = 1.2, bty = "o", box.col = 8,
       legend = c(paste0("Spacing = ",spacing,", offset = ", offset),
                  paste("Irregular polygon",signif(area,5),"m\u00B2"), 
                  paste("Max. extent",signif(maxarea,6),"m\u00B2")),
       pch = c(3,NA,NA), col = c("purple",1, "chocolate"), lty = c(NA,1,2))
mtext(paste("polygon has",length(inside),"points from original rectangular grid of",
            length(gridSP),"points"), 3,-1)
