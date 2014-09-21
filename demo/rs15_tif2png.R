
source("~/SparkleShare/Rprofile/R/sourceDir.R")
sourceDir("~/SparkleShare/rLandsat8/src/main/R/rLandsat8/R")
sourceDir("~/SparkleShare/TIR/R/")
dataDir <- "~/Share500sda/Landsat8/at1_TOA/toaBT"
setwd(dataDir)
inRaster <- list.files(dataDir, pattern = ".tif$", recursive = TRUE)
rs.tif2png(inRaster)
