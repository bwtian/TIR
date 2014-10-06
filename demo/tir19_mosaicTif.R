#' Mosaic Rasters
#'
#' @author Bingwei Tian
#' @param tif a list of tif files
#' @date 926
source("./tirSettings.R")
if (!file.exists(dir.toaTbKlccScaleMos)){
        dir.create(dir.toaTbKlccScaleMos)
}
tif10 <- list.files(path= dir.toaTbKlccScale,
                  pattern= "B10.tif$",
                  all.files=TRUE,
                  full.names=TRUE,
                  recursive=TRUE,
                  ignore.case=TRUE)
r10.rst  <- lapply(tif10, raster)
# B10 <- mosaic(r10.rst, fun = mean,
#         filename = file.path(dir.toaTbKlccScaleMos, "B10Mosaic.tif")
# )
## Mosaic a list of raster*
r10.rst$fun <- mean
mos10 <- do.call(mosaic, r10.rst)
writeRaster(mos10,
            filename = file.path(dir.toaTbKlccScaleMos, "B10Mosaic.tif"),
            overwrite = TRUE)
jpeg(filename = file.path(dir.toaTbKlccScaleMos, "B10Mosaic.jpeg"))
plot(mos10)
dev.off()

tif11 <- list.files(path= dir.toaTbKlccScale,
                    pattern= "B11.tif$",
                    all.files=TRUE,
                    full.names=TRUE,
                    recursive=TRUE,
                    ignore.case=TRUE)
###Core Code
r11.rst  <- lapply(tif11, raster)
r11.rst$fun <- mean
mos11 <- do.call(mosaic, r11.rst)
writeRaster(mos11,
            filename = file.path(dir.toaTbKlccScaleMos, "B11Mosaic.tif"),
            overwrite = T)
jpeg(filename = file.path(dir.toaTbKlccScaleMos, "B11Mosaic.jpeg"))
plot(mos11)
dev.off()
