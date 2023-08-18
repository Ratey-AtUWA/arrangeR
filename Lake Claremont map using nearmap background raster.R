library(sf)
library(raster) # slope parameters from raster::terrain() function
library(rgdal)
library(ggplot2)
library(ggpubr)
library(viridis)
library(stars)
library(prettymapr)
library(TeachingDemos)
GDA94 <- st_crs(28350) #"+proj=geocent +ellps=GRS80 +units=m +no_defs +type=crs"

jinf <- GDALinfo("Lake-Claremont-Nearmap_20230430.jpg") # just file info

# read georeferencing data in .jgw file
# Line 1: length of a pixel in the x direction (horizontal)
# Line 2: angle of rotation (is usually 0 or ignored)
# Line 3: angle of rotation (is usually 0 or ignored)
# Line 4: negative length of a pixel in the y direction (vertical)
# Line 5: x coordinate at center of pixel in the top left corner of the image
# Line 6: y coordinate at center of pixel in the top left corner of the image

# jgw <- 
# read.table("EPSG32750_Date20230430_Lat-31.974026_Lon115.776758_Mpp0.299.jgw",
#            header=F)
# row.names(jgw) <- c("xpix","ang1","ang2","ypix","xTL","yTL")

# so long as the filenames of the .jpg and .jpw files match, the raster extent 
# and coordinates are obtained or calculated from the values in the .jpw file

LC_nearmap <- 
  read_stars("EPSG32750_Date20230430_Lat-31.974026_Lon115.776758_Mpp1.194.jpg")
str(LC_nearmap)
st_crs(LC_nearmap) <- st_crs(32750)
st_transform(LC_nearmap, crs=st_crs(32750))

LC_rgb <- st_rgb(LC_nearmap)
st_crs(LC_rgb) <- st_crs(32750)
print(LC_rgb)

samplocs <- st_as_sf(x = data.frame(Easting=c(384300,384550),
                                    Northing=c(6461840,6461440)),
         coords = c("Easting", "Northing"), crs = st_crs(32750))

palette(c("white",viridis::viridis(12)[2:9], "black","transparent"))
par(oma=c(2,2,1,1), mar=c(0,0,0,0), tcl=0.25,xpd=F, mgp=c(1,-0.4,-1))
image(LC_rgb, main="", pch=21,bg=3,col=10,cex=2)
plot(LC_BoundLake, col="#00FFFF40", add=T)
plot(drainsC, col="#b0b0ff", lwd=3, add=TRUE)
contour(x=seq(min(elev_LC$Easting),max(elev_LC$Easting), l=40),
        y=seq(min(elev_LC$Northing),max(elev_LC$Northing), l=40),
        z=matrix(elev_LC$Elevation, nrow=40,byrow=TRUE),
        add=T,labcex=0.8, nlevels=10, vfont=NULL, col="#FFFF80", lty="13",lwd=2)
plot(lc_bordUTM, add=T, type="l", col="cyan", lwd=3)
axis(1, mgp=c(1,-0.4,-1)); axis(2, mgp=c(1,-0.4,-1))
legend("topleft", legend=c("Reserve Boundary","Drains"),ncol=1,
       pch=NA, lwd=3, seg.len=1.5, col=c("cyan","#b0b0ff"), text.col = "white",
       cex=1, inset=c(0.06,0.05), box.col = "transparent", bg="#000000a0")
axis(3,labels=F, mgp=c(0.2,-0.3,-1)); axis(4, labels=F)#; grid(lty=3, col=1)
mtext("Easting (UTM Zone 50S, m)",1,.8,font=2,cex=1.2)
mtext("Northing (UTM Zone 50S, m)",2,.9,font=2,cex=1.2)

plot(LC_pts, add=T,pch=21,col="black",bg="cyan",lwd=2,cex=1.6)
shadowtext(st_coordinates(LC_pts), pos=4, col="cyan", cex=1.5,
             labels = row.names(LC_pts))

drains <- st_read("C:/Users/00028958/LocalData/R Projects/Microplastics/water_corp_drains.shp")
lots <- st_read("../Microplastics/WaterCorpLots_FromCadastral.shp")
drains_utm <- st_transform(drains,crs=st_crs(32750))
lots_utm <- st_transform(lots,crs=st_crs(32750))
plot(drains_utm[6], add=TRUE)

points(c(384320,384570),c(6461820,6461420),
       pch=21,bg=5,col=1,cex=2,lwd=2)
# plot(0:1,0:1,type="n",bty="n",xaxt="n",yaxt="n",ann=F)
addnortharrow(pos="topleft",padin = c(0.62,0.62), 
              text.col = 10, border = 10, scale=1.4)
addnortharrow(pos="topleft",padin = c(0.6,0.6), 
              text.col = 1, border = 1, scale=1.4)
addscalebar(plotepsg=32750, pos="topleft",linecol = 11,label.col = 10,
            padin = c(2.02,0.62),htin = 0.2, label.cex = 1.5, bar.cols=c(11,11))
addscalebar(plotepsg=32750, pos="topleft",linecol = 1,label.col = 1,
            padin = c(2.0,0.6),htin = 0.2, label.cex = 1.5)
legend("bottomleft", title=expression(bold("Lake Claremont Precinct")),
       legend=c("Map source: NearMap",
                "Projection: UTM Zone 50 S",
                "EPSG: 32750",
                "Geoid: WGS84"),
       box.col = "white",inset=0.02, cex=1, y.intersp = 1.4)

LC_bbox <- st_as_sf(
  data.frame(E = c(383878.3, 384880.1), 
             N = c(6461094.7,6462340.6)), 
  coords = c("E","N"), 
  crs = st_crs(32750)
  )

wetlands <- st_read("../shapefiles/Geo_Wetlands_Swan_Coastal_Plain_DBCA_019.shp")
wetlandsUTM <- st_transform(wetlands, crs=st_crs(32750))
LakeClaremont <- st_crop(wetlandsUTM,LC_bbox)
LC_LakeEdge <- st_as_sf(data.frame(x=st_coordinates(LakeClaremont)[,1],
                                   y=st_coordinates(LakeClaremont)[,2]),
                        coords=c("x","y"), crs=st_crs(32750))

lcb <- as.matrix(st_coordinates(LC_strict))
lkh <- as.matrix(st_coordinates(LC_LakeEdge))

LC_BoundLake <- st_polygon(list(lcb,lkh)) |> st_sfc(crs=st_crs(32750))
plot(LC_BoundLake, col="#E0FFc080")
