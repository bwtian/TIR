#' Reproject Raster
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
dir.toaTbK  <- "~/Share500sda/Landsat8/at1_TOA/toaTbK/"
dir.toaTbKlcc  <-  "~/Share500sda/Landsat8/at1_TOA/toaTbKlcc"

toCRS  <- sp::CRS(lccWgs84)
if (!file.exists(dir.toaTbKlcc)){
        dir.create(dir.toaTbKlcc)
}
tif <- list.files(path= dir.toaTbK ,
                  pattern= ".tif$",
                  all.files=TRUE,
                  full.names=TRUE,
                  recursive=TRUE,
                  ignore.case=TRUE)
r.rst  <- lapply(tif, raster)
for (i in r.rst) {
        outName  <- paste0(names(i), ".tif")
        projectRaster(i, crs = toCRS,  method = "ngb",
                      filename =  file.path(dir.toaTbKlcc, outName),
                      overwrite=TRUE)
        raster::removeTmpFiles(h = 1) ## Improtant tips for save hardisk
}
#test = projectRaster(r.rst[[1]], crs = toCRS, method = "ngb")
