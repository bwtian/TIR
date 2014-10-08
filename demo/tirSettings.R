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
sourceDir("~/SparkleShare/phd/R/")
sourceDir("~/SparkleShare/rLandsat8/src/main/R/rLandsat8/R")
sourceDir("~/SparkleShare/geothermaR/R/")
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
dir.toaTbKlccCenterMos <-  file.path(dir.toa,"toaTbKlccCenterMos")
dir.surface  <- file.path(dir.sat, "at2_Surface")
dir.sufTsKlcc  <-  file.path(dir.surface, "sufTsK")
dir.database  <- file.path(driver, "at9_Database")
dir.lulc  <- file.path(dir.database, "LULC")
dir.AG100B  <- "~/Share500sda/AG100B/"
### Files
#hkdmaskb  <- readRDS("~/SparkleShare/TIR/hkdmskb_grdi2d1h.Rds")

xlimJP <- c(128.5, 146.5)
ylimJP <- c(30.2, 45.8)
certerJp <- c(137.5, 38)
phd.rainbow <- grDevices::colorRampPalette(c("purple","blue","cyan","green","yellow", "orange","red"))
# Coordinate Reference Systems of Japan
## Geographic Coordinate Reference Systems
#tokyoGRS <- "+init=epsg:4301"
tokyoGRS  <- "+proj=longlat +ellps=bessel +no_defs"
#wgs84GRS <- "+init=epsg:4326"
wgs84GRS  <-"+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"
#jgd2000GRS <- "+init=epsg:4612"
jgd2000GRS  <- "+proj=longlat +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +no_defs"

## Projected Coordinate Reference Systems
lccBessel <- "+proj=lcc +lat_1=32.8 +lat_2=43.2 +lat_0=38 +lon_0=137.5 +x_0=1000000 +y_0#=1000000 +ellps=bessel +towgs84=-146.414,507.337,680.507,0,0,0,0 +units=m +no_defs"
## change the datum to WGS84 20140508
lccWgs84 <- "+proj=lcc +lat_1=32.8 +lat_2=43.2 +lat_0=38 +lon_0=137.5 +x_0=1000000 +y_0=1000000 +datum=WGS84 +units=m +no_defs"
#  +ellps=WGS84 +towgs84=0,0,0
codeDir <- "~/Dropbox/1code"
dataDir <- "~/Dropbox/2data"
dataData <- "~/Dropbox/2data/data"
dataRaw <- "~/Dropbox/2data/dataRaw"
dataPro <- "~/Dropbox/2data/dataProduct"
figsDir <- "~/Dropbox/3figs"
