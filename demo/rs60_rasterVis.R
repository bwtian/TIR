source("~/SparkleShare/TIR/demo/tirSettings.R")
tif <- list.files(path= dir.toaTbK ,
                  pattern= "B10.tif$",
                  all.files=TRUE,
                  full.names=TRUE,
                  recursive=TRUE,
                  ignore.case=TRUE)
r.rst  <- lapply(tif, raster)
# par()
# par(mfcol =  c(4,4))
# lapply(r.rst, plot)
#
# lapply(r.rst, function(x) plot(x, col = terrain.colors(225)))



hkdshp  <- "~/Share500sda/2data/dataRaw/japan_ver71/HokkaidoUnion_lccWgs84.shp"

hkdshp  <- readShapePoly(hkdshp)
proj4string(hkdshp) <- CRS(lccWgs84)


### Raster Stacked
tif <- list.files(path= dir.toaTbKlcc ,
                  pattern= "B10.tif$",
                  all.files=TRUE,
                  full.names=TRUE,
                  recursive=TRUE,
                  ignore.case=TRUE)
r.rst  <- lapply(tif, raster)
r.stack  <- stack(r.rst)
hkdmaskb  <- readRDS("~/SparkleShare/TIR/hkdmskb_grdi2d1h.Rds")
r.mask  <- mask(r.stack, hkdmaskb)
r.center  <- scale(r.mask,  center=TRUE, scale=FALSE)
r.mosaic  <- mosaic(r.center)
levelplot(r.center)

summary(r.stackm)
#plot(r.stack, col = heat.colors(255), zlim = c(290, 320))
#plot(r.stackm, col = heat.colors(255))

tcat  <- c(seq())
levelplot(r.stackm, contour = TRUE, margin = FALSE, at = tcat, layout = c(4, 4))
miat = c(0, 0.25, 0.5, 0.75, 1)
# levelplot(rprob, contour = TRUE, margin = FALSE, at = miat)
# levelplot(r.stack, col = heat.colors(255), zlim = c(290, 320))


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
