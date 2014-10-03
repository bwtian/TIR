source("~/SparkleShare/TIR/demo/tirSettings.R")
### lcc
shp  <- "~/Dropbox/2data/dataRaw/japan_ver71/HokkaidoUnion_lccWgs84.shp"
hkdlcc  <- readShapePoly(shp)
proj4string(hkdlcc) <- CRS(lccWgs84)
plot(hkdlcc)
hkdlccbig  <- phd.largestPolys(hkdlcc, Polygon = T)
plot(hkdlccbig)

### wgs
jp1 <- getData(path = "~/Dropbox/2data/dataRaw/gadm2/JPN_adm1.RData")
