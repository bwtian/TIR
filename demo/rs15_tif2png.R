
source("~/SparkleShare/Rprofile/R/sourceDir.R")
sourceDir("~/SparkleShare/rLandsat8/src/main/R/rLandsat8/R")
sourceDir("~/SparkleShare/TIR/R/")
dataDir <- "~/Share500sda/Landsat8/at1_TOA"
setwd(dataDir)
rs.tif2png()
