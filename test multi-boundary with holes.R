require(sp)
setwd("C:/Users/00028958/LocalData/R Projects/arrangeR")

LongLat <- CRS("+proj=longlat +ellps=WGS84
           +datum=WGS84 +no_defs") # uses Earth ellipsis ex WGS84 datum
UTM50S <- CRS("+proj=utm +zone=50 +south") # just for Zone 50 S

p4 <- UTM50S

# make boundary coordinates into SpatialPolygons object
poly1 <- Polygon(sv_boundN[, c(1,2)], hole = FALSE)
poly2 <- Polygon(sv_boundS[, c(1,2)], hole = FALSE)
poly3 <- Polygon(sl_bound[, c(1,2)], hole = TRUE)
irregPolys <-  Polygons(list(poly1,poly2,poly3),3)
irregSpatialPolys <- SpatialPolygons(list(irregPolys),
                                    proj4string = p4)

# define limits of initial (maximal) grid
extremes <- c(floor(bbox(irregSpatialPolys)[1,]),
              ceiling(bbox(irregSpatialPolys)[2,]))
# calculate areas
area <- (poly1@area*poly1@ringDir) +
  (poly2@area*poly2@ringDir) +
  (poly3@area*poly3@ringDir)
maxarea <- diff(bbox(irregSpatialPolys)[1,]) *
             diff(bbox(irregSpatialPolys)[2,])

# make initial (maximal) grid
xspacing <- 20
xoffset <- 0
yspacing <- 20
yoffset <- 0
grid_big <- expand.grid(
  seq(floor(bbox(irregSpatialPolys)[1,1]),
      ceiling(bbox(irregSpatialPolys)[1,2]), by = xspacing) + xoffset,
  seq(floor(bbox(irregSpatialPolys)[2,1]),
      ceiling(bbox(irregSpatialPolys)[2,2]), by = yspacing) + yoffset
)
# convert initial grid to SpatialPoints object
gridSP <- SpatialPoints(grid_big, proj4string = p4)

# use over() function from sp package to mask initial grid
inOrOut <- as.vector(over(gridSP, irregSpatialPolys))
insideSP <- gridSP[which(inOrOut>0)]

par(mar = c(3,3,1,1), mgp = c(1.7,0.3,0), tcl = 0.25)
plot(irregSpatialPolys); axis(1); axis(2)
mtext("Easting (m)",1,1.7,font=2); mtext("Northing (m)", 2, 1.7,font=2)
mtext("Projection: EPSG 32750, UTM Zone 50",1,-1.2,cex=0.8,adj=0.05,col=8)
points(gridSP@coords, pch=3, cex = 0.8, col="orchid")
points(insideSP@coords, pch=19, cex = 1, col="blue3")
legend("left", inset = 0.025, legend = c("Sample here","Not here"),
       pch = c(19,3), col = c("blue3","orchid"), cex = 1.4)

# convert masked coordinates back into data frame
inside <- as.data.frame(insideSP@coords)
colnames(inside) <- c("x", "y")

# make output object
output <-
  list(
    points_in = inside,
    x_spacing = xspacing,
    y_spacing = yspacing,
    x_offset = xoffset,
    y_offset = yoffset,
    npoints = NROW(inside),
    max_points = NROW(grid_big),
    shape_area = area,
    max_area = maxarea,
    extremes = irregSpatialPolys@bbox,
    proj_4_str = p4
  )
