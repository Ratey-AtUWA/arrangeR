library(rosm)
library(prettymapr)
library(OpenStreetMap)
library(ggmap)

# use the following for help:
help(package = "rosm")
# and/or
help(package = "ggmap")

# > osm.types() # for rosm osm.plot()
# [1] "osm"              "opencycle"         "hotstyle"      "loviniahike"
# [5] "loviniacycle"     "hikebike"          "hillshade"     "osmgrayscale"
# [9] "stamenbw"         "stamenwatercolor"  "osmtransport"  "thunderforestlandscape"
# [13] "thunderforestoutdoors"  "cartodark"        "cartolight"
# > bmaps.types() # for rosm bmaps.plot()
# [1] "Aerial"           "AerialWithLabels" "Road"

par(mar = c(3,3,1,1), mgp = c(1.7,0.3,0), lend=2, ljoin=1, tcl=0.25)

# specify limits of map in order N, E, S, W
udubua.rosm <- makebbox(-31.9751,115.8284,-31.9882,115.8143)

# plot an osm-type map
osm.plot(udubua.rosm, type="osm", zoomin=0)

# use formula to make web mercator axis ticks with long-lat axes labels
xLL <- pretty(udubua.rosm[1,])[-1]
yLL <- pretty(udubua.rosm[2,])[-1]

# vertex = c(115.82, -31.98)
smRadius = 6378136.98
smRange = smRadius * pi * 2.0
smLonToX = smRange / 360.0
smRadiansOverDegrees = pi / 180.0

xWM <- xLL * smLonToX

yWM <- yLL * smRadiansOverDegrees
yWM <- log(tan(yWM) + (1.0 / cos(yWM)), base = exp(1))
yWM <- yWM * smRadius

osm.plot(udubua.rosm, type="osm", zoomin=0)
axis(1, cex.axis = 1.2, at = xWM, labels = xLL)
mtext("Longitude (\u00B0E)", 1, 1.7, font = 2, cex = 1.5)
axis(2, cex.axis = 1.2, at = yWM, labels = yLL)
mtext("Latitude (\u00B0S)", 2, 1.7, font = 2, cex = 1.5)
rm(list = c("xLL", "yLL", "xWM", "yWM","smRadius", "smRange",
            "smLonToX", "smRadiansOverDegrees"))

# add some points to the plot
osm.points(c(115.82, 115.8186, 115.8211, 115.8182, 115.8184),
           c(-31.9835, -31.9829, -31.9821, -31.9790, -31.9765),
           col="purple", lwd=3, pch=4, cex=1.5)

# another option using a Bing map as background
bmaps.plot(udubua.rosm, type="AerialWithLabels", zoomin=0)
osm.points(c(115.82, 115.8186, 115.8211, 115.8182, 115.8184),
           c(-31.9835, -31.9829, -31.9821, -31.9790, -31.9765),
           col="yellow", lwd=3, pch=3, cex=1.5)
osm.text(c(115.825,115.826), c(-31.98, -31.987),
         labels = c("Matilda\nBay", "Pelican Point"),
         col = c("lightcyan","palegreen"), font = 3)
addnortharrow(border = "white", text.col = "white")
addscalebar(plotepsg = 3857, linecol = "white", label.col = "white")

# embedding rosm functions inside prettymap()
png(filename="TEST2_rosm_to_png.png", height = 1080, width = 800)
prettymap({
  bmaps.plot(udubua.rosm, type="AerialWithLabels", zoomin = -1)
  osm.points(c(115.82, 115.8186, 115.8211, 115.8182, 115.8184),
             c(-31.9835, -31.9829, -31.9821, -31.9790, -31.9765),
             col="yellow", lwd=3, pch = 0, cex=1.7)
  osm.points(c(115.82, 115.8186, 115.8211, 115.8182, 115.8184),
             c(-31.9835, -31.9829, -31.9821, -31.9790, -31.9765),
             col="red", lwd=3, pch = 15, cex=1.5)
  osm.text(115.825, -31.98,
           labels = "Matilda\nBay",
           font = 3, col = "aliceblue")
  osm.text(115.824, -31.9861,
           labels = "Pelican Point",
           font = 3, col = "darkgreen")
  },
  oma = c(3,3,1,1), mai=c(0.5,0.4,0.4,0.25), drawarrow = T, arrow.scale = 1.5,
  arrow.border = "pink", arrow.text.col = "pink",
  arrow.cols = c("pink","black"), scale.bar.cols = c("pink","black"),
  scale.htin = 0.2, scale.label.cex = 1.5, scale.label.col = "pink",
  scale.linecol = "pink", scale.pos = "bottomright"
)
legend(par("usr")[1] + 0.1*(par("usr")[2]-par("usr")[1]),
       par("usr")[3] + 0.2*(par("usr")[4]-par("usr")[3]),
       legend = "Coffee is found here",
       pch = 22, col = "yellow", pt.bg = "red",
       inset = 0.02, cex = 1.5, pt.cex = 2.5, pt.lwd=3,
       bg="#204020", text.col = "white")
# box()

xtix <- projectMercator(rep(udubua.rosm["y","min"], length(pretty(udubua.rosm[1,])[-1])),
                        pretty(udubua.rosm[1,])[-1])
axis(1, cex.axis = 1.2, at = xtix[,1],
     labels = round(pretty(udubua.rosm[1,])[-1],3))
mtext("Longitude (\u00B0E)", 1, 2.7, font = 2, cex = 1.5)
ytix <- projectMercator(pretty(udubua.rosm[2,])[-1],
                        rep(udubua.rosm["x","min"], length(pretty(udubua.rosm[2,])[-1])))
axis(2, cex.axis = 1.2, at = ytix[2:(length(pretty(udubua.rosm[2,]))-1),2],
     labels = round(pretty(udubua.rosm[2,])[2:(length(pretty(udubua.rosm[2,]))-1)],3))
mtext("Latitude (\u00B0S)", 2, 2.7, font = 2, cex = 1.5)
rm(list = c("xtix", "ytix"))
dev.off();options(device="RStudioGD");getOption("device")

# now trying ggmap ####
# make ggmap object
register_google(key = GoogleMapsKey)
udubua.gg <- get_googlemap(center=c(115.8213,-31.98165),
                            zoom = 15, maptype = "roadmap", color = "color")
#
# plot ggmap object using ggplot grammar
ggmap(udubua.gg) +
  labs(y="Latitude (\u00B0S)", x = "Longitude (\u00B0E)") +
  geom_text(aes(x = 115.825, y = -31.98, label = "Swan\nRiver",
                fontface = "italic", family="sans"),
            size = 5, vjust = 0, hjust = 0, color="steelblue") +
  theme(axis.text=element_text(size=14, color="black"),
        axis.title=element_text(size=16,face="bold"))
