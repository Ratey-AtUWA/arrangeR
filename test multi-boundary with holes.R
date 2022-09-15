require(sp)

LongLat <- CRS("+proj=longlat +ellps=WGS84
           +datum=WGS84 +no_defs") # uses Earth ellipsis ex WGS84 datum
UTM50 <- CRS("+proj=utm +zone=50 +south") # just for Zone 50 S

p4 <- UTM50

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

par(mar = c(3,3,1,1))
plot(irregSpatialPolys)
points(gridSP@coords, pch=3, cex = 0.8, col="pink")
points(insideSP@coords, pch=19, cex = 1, col="blue3")

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
    extremes = c(min(floor(boundary[, 1])),
                 min(floor(boundary[, 2])),
                 max(ceiling(boundary[, 1])),
                 max(ceiling(boundary[, 2]))),
    proj_4_str = p4
  )
