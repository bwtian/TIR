#' Scale Raster
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
dir.toaTbKlcc  <-  "~/Share500sda/Landsat8/at1_TOA/toaTbKlcc"
dir.toaTbKlccScale  <-  "~/Share500sda/Landsat8/at1_TOA/toaTbKlccScale"
toCRS  <- sp::CRS(lccWgs84)
if (!file.exists(dir.toaTbKlccScale)){
        dir.create(dir.toaTbKlccScale)
}
tif <- list.files(path= dir.toaTbKlcc,
                  pattern= ".tif$",
                  all.files=TRUE,
                  full.names=TRUE,
                  recursive=TRUE,
                  ignore.case=TRUE)
r.rst  <- lapply(tif, raster)
hkdmaskb  <- readRDS("~/SparkleShare/TIR/hkdmskb_grdi2d1h.Rds")
r.rstm  <- lapply(r.rst, function(x) mask(x, hkdmaskb))
for (i in r.rst) {
        outName  <- paste0(names(i), ".tif")
        #projectRaster(from = i, crs = toCRS,  method = "ngb",
        zscore  <- raster::scale(i)
        writeRaster(zscore,
                    filename = file.path(dir.toaTbKlccScale, outName),
                    overwrite = T)
        raster::removeTmpFiles(h = 1) ## Improtant tips for save hardisk
}
#test = projectRaster(r.rst[[1]], crs = toCRS, method = "ngb")
