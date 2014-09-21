
source("~/SparkleShare/Rprofile/R/sourceDir.R")
sourceDir("~/SparkleShare/rLandsat8/src/main/R/rLandsat8/R")
sourceDir("~/SparkleShare/TIR/R/")
inRaster <- list.files("~/Share500sda/Landsat8", pattern = ".tif$", recursive = TRUE)
rs.tif2png(inRaster)
