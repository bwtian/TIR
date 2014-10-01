### Source Functions
source("~/SparkleShare/Rprofile/R/sourceDir.R")
sourceDir("~/SparkleShare/Rprofile/R/")
sourceDir("~/SparkleShare/TIR/R/")
sourceDir("~/SparkleShare/rLandsat8/src/main/R/rLandsat8/R")
### Define Drivers
driver     <- "~/Share500sda/Landsat8/" # Linux and Windows Symbolink
dir.tif    <- file.path(driver, "at0_Sensor")
dir.toaRad <- file.path(driver, "at1_TOA/toaRad")
dir.toaTbKlccScale  <-  "~/Share500sda/Landsat8/at1_TOA/toaTbKlccScale"
dir.toaTbKlccScaleMos  <- "~/Share500sda/Landsat8/at1_TOA/toaTbKlccScaleMos"


### Raster Options
dir.tmp    <- file.path(driver, "raster_tmp")
rasterOptions(tmpdir = dir.tmp)
### Load Library
library(sp)
library(rgdal)
library(raster)
library(rasterVis)
