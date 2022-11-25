boundary <- read.csv("sv_boundary.csv")
boundary <- boundary[,1:2]
xspacing <- 20
yspacing <- xspacing
xoffset = 0
yoffset = xoffset
epsg = 32750
# {
    require(sf) # we're using simple features
    # if projection not defined, set to EPSG:4326 (WGS84 LongLat)
    if (is.null(epsg)) {
      ep <-
        st_crs(4326)
    } else {
      ep <- st_crs(epsg)
    }

    # make boundary coordinates into SpatialPolygons object
    # irregpoly <- st_as_sf(boundary[, c(3,4)], coords = c("Longitude","Latitude"),
    #                       crs=ep)
    irregpoly <- st_polygon(list(as.matrix(boundary)))
    irregpoly <- st_sf(st_sfc(irregpoly, crs = ep))
    irregPolys <- st_multipolygon(list(irregpoly))
    irregSpatialPolys = SpatialPolygons(list(irregPolys),
                                        proj4string = p4)

    # define limits of initial (maximal) grid
    extremes <- c(min(floor(boundary[, 1])),
                  min(floor(boundary[, 2])),
                  max(ceiling(boundary[, 1])),
                  max(ceiling(boundary[, 2])))
    # calculate areas
    area <- st_area(irregpoly)
    grid_big <- st_sf(st_cast(st_make_grid(irregpoly,
                                           cellsize=c(xspacing,yspacing)),
                              "MULTIPOINT"), crs = ep)
    grid_big <- st_sf(st_make_grid(irregpoly, cellsize = c(xspacing, yspacing),
                             what = "corners"), crs = ep)
    maxPoly <- st_polygon(list(cbind(st_bbox(grid_big)[c(1,1,3,3,1)],
                                  st_bbox(grid_big)[c(2,4,4,2,2)])))
    maxarea <- st_area(st_sf(st_sfc(maxPoly), crs = ep))

    plot(grid_big)
    axis(1);axis(2);box()
    plot(points_in,col="red",pch=19,add=T)
    lines(st_coordinates(irregpoly),col="#0000FF80",lwd=4)

    # use points_in() function from sf package to mask initial grid
    points_in <- st_intersection(grid_big, irregpoly)
    # convert masked coordinates back into data frame

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
        epsg = ep
      )
    return(output)
#  }

plot(grid_big, col="grey")
axis(1);axis(2);box()
plot(points_in,col="red",pch=19,add=T)
lines(st_coordinates(irregpoly),col="#0000FF80",lwd=4)
