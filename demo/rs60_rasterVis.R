library(maptools)
library(raster)
source("~/SparkleShare/Rprofile/R/sourceDir.R")
sourceDir("~/SparkleShare/Rprofile/R/")
dir.tmp <- "~/Share500sda/Landsat8/raster_tmp"
rasterOptions(tmpdir = dir.tmp)
dir.toaTbK  <-  "~/Share500sda/Landsat8/at1_TOA/toaTbK"
dir.toaTbKlcc  <-  "~/Share500sda/Landsat8/at1_TOA/toaTbKlcc"

tif <- list.files(path= dir.toaTbK ,
                  pattern= "B10.tif$",
                  all.files=TRUE,
                  full.names=TRUE,
                  recursive=TRUE,
                  ignore.case=TRUE)
r.rst  <- lapply(tif, raster)
par()
par(mfcol =  c(4,4))
lapply(r.rst, plot)

lapply(r.rst, function(x) plot(x, col = terrain.colors(225)))
library(rasterVis)



hkdshp  <- "~/Dropbox/2data/dataRaw/japan_ver71/HokkaidoUnion_lccWgs84.shp"

hkdshp  <- readShapePoly(hkdshp)
proj4string(hkdshp) <- CRS(lccWgs84)
hkdshpb  <- phd.largestPolys(hkdshp, Polygon = T)


### Raster Stacked
tif <- list.files(path= dir.toaTbKlcc ,
                  pattern= "B10.tif$",
                  all.files=TRUE,
                  full.names=TRUE,
                  recursive=TRUE,
                  ignore.case=TRUE)
r.rst  <- lapply(tif, raster)
r.stack  <- stack(r.rst)
r.stackm  <- scale(r.stack,  center=TRUE, scale=FALSE)
#plot(r.stack, col = heat.colors(255), zlim = c(290, 320))
plot(r.stackm, col = heat.colors(255))
levelplot(r.stack)
miat = c(0, 0.25, 0.5, 0.75, 1)
levelplot(rprob, contour = TRUE, margin = FALSE, at = miat)
levelplot(r.stack, col = heat.colors(255), zlim = c(290, 320))


hkdmaskb  <- readRDS("~/SparkleShare/TIR/hkdmskb_grdi2d1h.Rds")
plot(hkdmaskb, add = T)
for (i in r.rst) {
        outName  <- paste0(names(i), ".tif")
        #projectRaster(from = i, crs = toCRS,  method = "ngb",
        projectRaster(from = i,  to = hkdmaskb,
                      filename =  file.path(dir.toaTbKlcc, outName),
                      overwrite=TRUE)
        raster::removeTmpFiles(h = 1) ## Improtant tips for save hardisk
}
#test = projectRaster(r.rst[[1]], crs = toCRS, method = "ngb")
