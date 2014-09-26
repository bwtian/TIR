#' Reproject Raster
#'
#' @author Bingwei Tian
#' @param tif a list of tif files
library(sp)
library(rgdal)
library(raster)
toCRS  <- sp::CRS(lccWgs84)

dir.toaTbK  <- "~/Share500sda/Landsat8/at1_TOA/toaTbK/"
tif <- list.files(path= dir.toaTbK ,
                  pattern= ".tif$",
                  all.files=TRUE,
                  full.names=TRUE,
                  recursive=TRUE,
                  ignore.case=TRUE)
r.lst  <- lapply(tif, raster)
r.lst

