library(shiny)
library(ggplot2)
library(sf)
library(stars)

# lcnm <- read_stars("C:/Users/00028958/OneDrive - The University of Western Australia/R Projects/Lake_Claremont/EPSG32750_Date20230430_Lat-31.974026_Lon115.776758_Mpp1.194.jpg")
lcbkgd <- read_stars("LCbkgd.jpg", RasterIO = list(bands=1:3))
lcb <- read.csv("./arrange2/LC_SampledArea.csv", stringsAsFactors = TRUE)
lcw <- read.csv("./arrange2/LC_WaterEdge.csv", stringsAsFactors = TRUE)
lake <- st_polygon(list(as.matrix(lcw))) |> st_sfc(crs=st_crs(32750))
lcpoly <- st_polygon(list(as.matrix(lcb),as.matrix(lcw))) |>
  st_sfc(crs=st_crs(32750))

# give values of input parameters
xysp <- 80
xyadj <- 0
ifsq <- F
xyjit <- 0.0
rand0 <- 0

# set up objects which will be reactive in shiny
input <- data.frame(xsp=xysp, ysp=xysp, xyadj=xyadj, ifsq=ifsq,
                    xyjit=xyjit,rand0=rand0)
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


# ...but ggplot version anyway ####
ggplot() +
  geom_stars(data=lcbkgd, show.legend=FALSE) +
  scale_fill_gradient(low = "black", high = "white", na.value = "transparent") +
  geom_sf(data=lake, bg="#b0e0ff", col="#b0e0ff") +
  geom_sf(data=lcpoly, bg="#e0e08060", col="#808020") +
  geom_sf(data=st_as_sf(st_jitter(st_intersection(sampgrid, lcpoly),
                                  amount=input$rand0)),
          shape=10, col="blue3", size=3, stroke=1) +
  xlab(label = "Easting (m, UTM Zone 50S)") +
  ylab(label = "Northing (m, UTM Zone 50S)") +
  geom_text(aes(x=384400, y=6461800, label="Lake\nClaremont",
                fontface = "italic"), col="royalblue", size=6) +
  theme_bw() +
  theme(axis.title = element_text(size=18, face = "bold"),
        axis.text = element_text(size=14),
        axis.ticks.length = unit(-0.2, "cm")) +
  coord_sf(datum = st_crs(32750))

