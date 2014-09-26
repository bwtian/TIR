#' Mosaic Rasters
#'
#' @author Bingwei Tian
#' @param tif a list of tif files
#' @date 926
library(sp)
library(rgdal)
library(raster)

source("~/SparkleShare/Rprofile/R/sourceDir.R")
sourceDir("~/SparkleShare/Rprofile/R/")
dir.tmp <- "~/Share500sda/Landsat8/raster_tmp"
rasterOptions(tmpdir = dir.tmp)

dir.toaTbKlccScale  <-  "~/Share500sda/Landsat8/at1_TOA/toaTbKlccScale"
dir.toaTbKlccScaleMos  <- "~/Share500sda/Landsat8/at1_TOA/toaTbKlccScaleMos"
toCRS  <- sp::CRS(lccWgs84)
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
mosaic(r10.rst, fun = mean,
        filename = file.path(dir.toaTbKlccScaleMos, "B10Mosaic.tif")
)
tif11 <- list.files(path= dir.toaTbKlccScale,
                    pattern= "B11.tif$",
                    all.files=TRUE,
                    full.names=TRUE,
                    recursive=TRUE,
                    ignore.case=TRUE)
mosaic(r11.rst, fun = mean,
       filename = file.path(dir.toaTbKlccScaleMos, "B11Mosaic.tif")
)
