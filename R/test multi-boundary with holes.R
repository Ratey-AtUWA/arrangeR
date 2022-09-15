require(sp)

LongLat <- CRS("+proj=longlat +ellps=WGS84
           +datum=WGS84 +no_defs") # uses Earth ellipsis ex WGS84 datum
UTM50 <- CRS("+proj=utm +zone=50 +south") # just for Zone 50 S

p4 <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs ")


# make boundary coordinates into SpatialPolygons object
irregpoly <- Polygon(sv_boundary[, c(1,2)], hole = F)
irregPolys = Polygons(list(irregpoly),1)
irregSpatialPolys = SpatialPolygons(list(irregPolys),
                                    proj4string = p4)

# define limits of initial (maximal) grid
extremes <- c(min(floor(boundary[, 1])),
              min(floor(boundary[, 2])),
              max(ceiling(boundary[, 1])),
              max(ceiling(boundary[, 2])))
# calculate areas
area <- irregpoly@area
maxarea <-
  (extremes[3] - extremes[1]) * (extremes[4] - extremes[2])

# make initial (maximal) grid
grid_big <- expand.grid(
  seq(extremes[1],
      extremes[3], by = xspacing) + xoffset,
  seq(extremes[2],
      extremes[4], by = yspacing) + yoffset
)
# convert initial grid to SpatialPoints object
gridSP <- SpatialPoints(grid_big, proj4string = p4)

# use over() function from sp package to mask initial grid
inOrOut <- as.vector(over(gridSP, irregSpatialPolys))
insideSP <- gridSP[which(inOrOut>0)]
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
