rowz <- 1:35
plot(sv_boundary[rowz,1:2], asp=1, type="c", xlim = c(391050,391440))
text(sv_boundary[rowz,1:2], labels=row.names(sv_boundary)[rowz],
     col="red3",cex=0.75)

sv_boundN <-
  rbind(sv_boundary[1:12,1:2],sv_boundary[26:36,1:2])
row.names(sv_boundN) <- NULL
rowz <- 1:23
plot(sv_boundN[rowz,1:2], asp=1, type="c", ylim = c(6466245,6466763))
text(sv_boundN[rowz,1:2], labels=row.names(sv_boundN)[rowz],col="red3",cex=0.75)

sv_boundS <-
  rbind(c(391244.5,6466528),sv_boundary[13:25,1:2],c(391244.5,6466528))
row.names(sv_boundS) <- NULL
rowz <- 1:15
lines(sv_boundS[rowz,1:2], asp=1, type="c", xlim = c(391050,391440))
text(sv_boundS[rowz,1:2], labels=row.names(sv_boundS)[rowz],col="red3",cex=0.75)

sl_bound <- read.csv("SmithsLk.boundary.csv")
str(sl_bound)
tempUTM <- SpatialPoints(sl_bound,proj4string = UTM50)
tempLL <- spTransform(tempUTM, CRSobj = LongLat)
sl_bound$Longitude <- tempLL@coords[,1]
sl_bound$Latitude <- tempLL@coords[,2]
