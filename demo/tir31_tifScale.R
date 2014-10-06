source("~/SparkleShare/TIR/demo/tirSettings.R")
# tif <- list.files(path= dir.toaTbK ,
#                   pattern= "B10.tif$",
#                   all.files=TRUE,
#                   full.names=TRUE,
#                   recursive=TRUE,
#                   ignore.case=TRUE)
# r.rst  <- lapply(tif, raster)
# par()
# par(mfcol =  c(4,4))
# lapply(r.rst, plot)
#
# lapply(r.rst, function(x) plot(x, col = terrain.colors(225)))


## lOAD Additional data
hkdshp  <- "~/Share500sda/2data/dataRaw/japan_ver71/HokkaidoUnion_lccWgs84.shp"
hkdshp  <- readShapePoly(hkdshp)
proj4string(hkdshp) <- CRS(lccWgs84)
hkdmaskb  <- readRDS("~/SparkleShare/TIR/hkdmskb_grdi2d1h.Rds")
### Raster Stacked
tif10 <- list.files(path= dir.toaTbKlcc ,
                  pattern= "B10.tif$",
                  all.files=TRUE,
                  full.names=TRUE,
                  recursive=TRUE,
                  ignore.case=TRUE)
r.rst  <- lapply(tif10, raster)
r.stack  <- stack(r.rst)

r.mask  <- mask(r.stack, hkdmaskb)
r.merge  <- merge(r.mask)
plot(r.mask)
plot(r.merge, col = bpy.colors(255))
levelplot(r.merge, par.settings =  BuRdTheme)
## Center values and Merge
r.center  <- scale(r.mask,center=TRUE, scale=FALSE)
r.centerMerge  <- merge(r.center)
plot(r.centerMerge, col = bpy.colors(255))
plot(r.centerMerge, maxpixels= 1e6, col = oceColorsJet(255))
levelplot(r.centerMerge,maxpixels=1e6, par.settings =  BuRdTheme)

## Center valuse and Mosaic
r.center$fun <- mean
r.centerMos10 <- do.call(mosaic, r.center)
writeRaster(r.centerMos10, "L8B10CenterMos.tif")

# levelplot(rprob, contour = TRUE, margin = FALSE, at = miat)
# levelplot(r.stack, col = heat.colors(255), zlim = c(290, 320))
