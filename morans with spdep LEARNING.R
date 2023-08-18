library(spdep)
ngsaWA_knn <- knearneigh(ngsaWAsf,k=8) # don't use longlat=TRUE
ngsaWA_nb <- knn2nb(ngsaWA_knn)
ngsaWA_nb
class(ngsaWA_nb)
ngsaWA_nbwt <- nb2listw(ngsaWA_nb)

moran(log10(ngsaWAsf$As.AR), ngsaWA_nbwt, 8, 305, NAOK=T)


lmi_data <- na.omit(ngsaWAsf[,"As.AR"])
lmi_data$As.log <- log10(lmi_data$As.AR)
lmi_nb <- knearneigh(ngsaWAsf,k=4) |> knn2nb()
lmi_wts <- nb2listw(lmi_nb)

lmi_As <- localmoran(lmi_data$As.log, lmi_wts)
str(lmi_As)
lmi_AsDF <- cbind(as.data.frame(lmi_As), attributes(lmi_As)$quadr)
str(lmi_AsDF)
summary(lmi_AsDF)

which(lmi_AsDF$`Pr(z != E(Ii))` <= 0.05)
lmi_AsDF[which(lmi_AsDF$`Pr(z != E(Ii))` <= 0.05),c(1,5,7)]
