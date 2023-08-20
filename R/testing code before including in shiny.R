lcb <- read.csv("./arrange2/LC_SampledArea.csv", stringsAsFactors = TRUE)
lcw <- read.csv("./arrange2/LC_WaterEdge.csv", stringsAsFactors = TRUE)
lcpoly <- st_polygon(list(as.matrix(lcb),as.matrix(lcw))) |>
  st_sfc(crs=st_crs(32750))

gap <- 80
off0 <- 28
ifsq <- T
input <- data.frame(xsp=gap, ysp=gap)
sampgrid <-st_make_grid(lcpoly,
               cellsize=c(input$xsp,input$ysp),
               offset = st_bbox(lcpoly)[c("xmin", "ymin")]+off0,
               what="centers", square=ifsq)

ggplot(lcpoly) +
    geom_sf(data=lcpoly, bg="#e0c08040", col="#806020") +
    geom_sf(data=st_intersection(sampgrid, lcpoly),
            shape=10, col="tan4", size=3, stroke=0.5) +
    xlab(label = "Longitude") +
    ylab(label = "Latitude") +
    geom_text(aes(x=384400, y=6461800, label="Lake\nClaremont",
                  fontface = "italic"), col="royalblue", size=6) +
    theme_bw() +
    theme(axis.title = element_text(size=14, face = "bold"),
          axis.text = element_text(size=12)) +
    coord_sf(crs = st_crs(32750))
