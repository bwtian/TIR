### Source Functions
source("~/SparkleShare/Rprofile/R/sourceDir.R")
sourceDir("~/SparkleShare/Rprofile/R/")
sourceDir("~/SparkleShare/TIR/R/")
sourceDir("~/SparkleShare/rLandsat8/src/main/R/rLandsat8/R")
### Define Drivers
#driver     <- "~/Share500sda/Landsat8/" # Linux and Windows Symbolink
#dir.tar  <- "~/Share300sdb/Landsat8/N0/LISTEJapan"
dir.tif <- file.path(driver, "at0_Sensor")
dir.toa <- file.path(driver, "at1_TOA")
dir.toaRad <- file.path(dir.toa, "toaRad")

dir.toaTbKlccScale  <-  file.path(dir.toa,"toaTbKlccScale")
dir.toaTbKlccScaleMos  <-  file.path(dir.toa,"toaTbKlccScaleMos")


### Raster Options
dir.tmp    <- file.path(driver, "raster_tmp")
rasterOptions(tmpdir = dir.tmp)
### Load Library
library(sp)
library(rgdal)
library(raster)
library(rasterVis)
