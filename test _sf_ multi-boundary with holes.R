library(sf)

setwd("C:/Users/00028958/OneDrive - The University of Western Australia/R Projects/arrangeR")

LongLat <- st_crs(4326) # long-lat using Earth ellipsis ex WGS84 datum
UTM50S <- st_crs(32750)
# just for Zone 50 S

p4 <- UTM50S

# make boundary coordinates into sf MULTIPOLYGON object
poly1 <- st_polygon(list(as.matrix(sv_boundN[, c(1,2)])))
# poly2 <- Polygon(sv_boundS[, c(1,2)], hole = FALSE)
poly2 <- st_polygon(list(as.matrix(sv_boundS[, c(1,2)]),
                    as.matrix(sl_bound[, c(1,2)])))
#                                     proj4string = p4)
irregSFpolys <- st_multipolygon(list(poly1, poly2)) |> st_sfc()
st_crs(irregSFpolys) <- UTM50S

# define limits of initial (maximal) grid
extremes <- st_bbox(irregSFpolys)
# calculate areas
area <- st_area(irregSFpolys)
maxarea <- diff(st_bbox(irregSFpolys)[c(1,3)]) *
             diff(st_bbox(irregSFpolys)[c(2,4)])

# make initial (maximal) grid
xspacing <- 100/3
xoffset <- 0
yspacing <- 100/3
yoffset <- -20/3
grid_big <- st_make_grid(irregSFpolys,
                         cellsize = c(xspacing,yspacing),
                         offset = st_bbox(irregSFpolys)[c("xmin", "ymin")] +
                           c(xoffset, yoffset),
                         what = "centers")

# # use `st_intersection()` function from `sf` package to mask initial grid
insideSF <- st_intersection(irregSFpolys, grid_big)

par(mar = c(3,3,1,1), mgp = c(1.7,0.3,0), tcl = 0.25)
plot(irregSFpolys); axis(1); axis(2)
mtext("Easting (m)",1,1.7,font=2); mtext("Northing (m)", 2, 1.7,font=2)
mtext("Projection: EPSG 32750, UTM Zone 50",1,-1.2,cex=0.8,adj=0.05,col=8)
plot(grid_big, add=TRUE, pch=3, cex = 0.8, col="orchid")
plot(insideSF, add=TRUE, pch=19, cex = 1, col="blue3")
legend("left", inset = 0.025, legend = c("Sample here","Not here"),
       pch = c(19,3), col = c("blue3","orchid"), cex = 1.4)

# convert masked coordinates back into data frame
inside <- st_as_sf(insideSF) |> st_coordinates() |> as.data.frame()
colnames(inside) <- c("x", "y")

# make output object
output <-
  list(
    x_spacing = xspacing,
    y_spacing = yspacing,
    x_offset = xoffset,
    y_offset = yoffset,
    npoints = NROW(inside),
    max_points = NROW(grid_big),
    shape_area = area,
    max_area = maxarea,
    extremes = extremes,
    points_in = inside,
    proj_4_str = p4
  )
