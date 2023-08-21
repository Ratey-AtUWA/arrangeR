library(shiny)
library(ggplot2)
library(sf)

lcb <- read.csv("./arrange2/LC_SampledArea.csv", stringsAsFactors = TRUE)
lcw <- read.csv("./arrange2/LC_WaterEdge.csv", stringsAsFactors = TRUE)
lake <- st_polygon(list(as.matrix(lcw))) |> st_sfc(crs=st_crs(32750))
lcpoly <- st_polygon(list(as.matrix(lcb),as.matrix(lcw))) |>
  st_sfc(crs=st_crs(32750))

# give values of input parameters
xysp <- 70
xyadj <- 0
ifsq <- F
xyjit <- 0.25

# set up objects which will be reactive in shiny
input <- data.frame(xsp=xysp, ysp=xysp)
sampgrid <- st_make_grid(lcpoly,
               cellsize = c(input$xsp, input$ysp),
               offset = st_bbox(lcpoly)[c("xmin", "ymin")] + xyadj,
               what = "centers",
               square = ifsq)

# better without ggplot !?!
par(mar=c(3.5,3.5,0.5,0.5), mgp=c(1.7,0.3,0), tcl=0.25, font.lab=2)
plot(lcpoly, col="#F0E68C80", border="#e0d67C", bg="#00000000", axes=T,
     xlab="Easting (m, UTM Zone 50S",
     ylab="Northing (m, UTM Zone 50S")
plot(lake, col="#e0e0ff", border="#00000000", axes=T, add = T)
grid()
text(384400, 6461850, labels="Lake\nClaremont",
         font = 3, col="royalblue", cex=1.2)
plot(st_intersection(sampgrid, lcpoly), add=T, pch=3, cex=0.8, col="azure3")
plot(st_intersection(st_jitter(sampgrid, amount = xysp*xyjit), lcpoly),
     add=T, pch=10, col="blue3")
# c("antiquewhite"="#FAEBD7", "khaki"="#F0E68C", "wheat"="#F5DEB3")
