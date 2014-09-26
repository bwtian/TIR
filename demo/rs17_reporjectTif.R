#' Reproject Raster
#'
#' @author Bingwei Tian
#' @param tif a list of tif files
#' @date 926
library(sp)
library(rgdal)
library(raster)
toCRS  <- sp::CRS(lccWgs84)

dir.toaTbK  <- "~/Share500sda/Landsat8/at1_TOA/toaTbK/"
dir.toaTbKlcc  <-  "~/Share500sda/Landsat8/at1_TOA/toaTbKlcc"
if (!file.exists(dir.toaTbKlcc)){
        dir.create(dir.toaTbKlcc)
}
tif <- list.files(path= dir.toaTbK ,
                  pattern= ".tif$",
                  all.files=TRUE,
                  full.names=TRUE,
                  recursive=TRUE,
                  ignore.case=TRUE)
for (i in r.lst) {
        outName  <- paste0(names(i), "tif")
        projectRaster(i, res = 30, crs = toCRS,
                      filename =  file.path(dir.toaTbKlcc, outName))

}

projectRaster()

