#' @usage source("./tirSettings.R")
#' @author Bingwei Tian <bwtian@gmail.com>
#'
### Load Library
library(sp)
library(rgdal)
library(gdalUtils)
library(maptools)
library(raster)
library(rasterVis)
library(plotKML)
library(gstat)
library(plyr)
library(rgeos)
library(ggplot2)
library(ggmap)

library(lattice)
library(latticeExtra)
#library(oce)
### Source Functions
source("~/SparkleShare/Rprofile/R/RprofilesAuto/sourceDir.R")
sourceDir("~/SparkleShare/Rprofile/R/")
sourceDir("~/SparkleShare/TIR/R/")
sourceDir("~/SparkleShare/rLandsat8/src/main/R/rLandsat8/R")
###  Options
if(.Platform$OS.type == "windows"){
        windowsFonts(Times=windowsFont("TT Times New Roman"))
        windowsFonts(times=windowsFont("TT Times New Roman"))
}

driver     <- "~/Share500sda/" # Linux and Windows Symbolink
dir.tmp    <- file.path(driver, "raster_tmp")
rasterOptions(tmpdir = dir.tmp)
raster::removeTmpFiles(h = 24)
gc()

### Colors
rainbow1  <- rainbow(n = 255, start = 2/6)
oceColorsJet  <- function (n)
{
        if (missing(n) || n <= 0)
                colorRampPalette(c("#00007F", "blue", "#007FFF", "cyan",
                                   "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"))
        else {
                colorRampPalette(c("#00007F", "blue", "#007FFF", "cyan",
                                   "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"))(n)
        }
}
### Define Drivers
#dir.tar  <- file.path(driver, "Landsat8/L1T")
dir.sat  <- file.path(driver, "Landsat8")
dir.tif <- file.path(dir.sat, "at0_Sensor")
dir.toa <- file.path(dir.sat, "at1_TOA")
dir.toaRad <- file.path(dir.toa, "toaRad")
dir.toaTbKlccScale  <- file.path(dir.toa,"toaTbKlccScale")
dir.toaTbKlccScaleMos <- file.path(dir.toa,"toaTbKlccScaleMos")
dir.toaRef  <- file.path(dir.toa,"toaRef")
dir.toaRefSun  <- file.path(dir.toa,"toaRefSun")
dir.toaTbK <- file.path(dir.toa, "toaTbK")
dir.toaTbC <- file.path(dir.toa, "toaTbC")
dir.toaTbKE <- file.path(dir.toa, "toaTbKE")
dir.toaEmi  <-   file.path(dir.toa, "toaEmi")
dir.toaTbKlcc  <-  file.path(dir.toa,"toaTbKlcc")
dir.toaTbKlccScale  <-  file.path(dir.toa,"toaTbKlccScale")
dir.toaTbKlccScaleMos <-  file.path(dir.toa,"toaTbKlccScaleMos")
dir.surface  <- file.path(dir.sat, "at2_Surface")
dir.sufTsKlcc  <-  file.path(dir.surface, "sufTsK")
dir.database  <- file.path(driver, "at9_Database")
dir.lulc  <- file.path(dir.database, "LULC")
dir.AG100B  <- "~/Share500sda/AG100B/"
### Files
#hkdmaskb  <- readRDS("~/SparkleShare/TIR/hkdmskb_grdi2d1h.Rds")

