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
volA  <- readRDS("~/Share500sda//2data/dataProduct/hkd/")
volAlcc  <- spTransform(volA, CRS(lccWgs84))
phd.crsTransfer
proj4string(volA)
plot(volAlcc, pch = 2, size = 6,add =T)
## Center valuse and Mosaic
r.center$fun <- mean
r.centerMos10 <- do.call(mosaic, r.center)
plot(r.centerMos10, col = oceColorsJet(255))

levelplot(r.centerMerge,maxpixels=1e6, par.settings =  BuRdTheme)

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
