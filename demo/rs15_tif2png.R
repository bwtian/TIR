
source("~/SparkleShare/Rprofile/R/sourceDir.R")
sourceDir("~/SparkleShare/rLandsat8/src/main/R/rLandsat8/R")
sourceDir("~/SparkleShare/TIR/R/")
#dataDir <- "~/Share500sda/Landsat8/at1_TOA"
dataDir  <- dir.toaTbKlcc
setwd(dataDir)
r = raster("LC81050292014153LGN00_B10.tif")
png("test.png")
plot(r)
dev.off()

rs.tif2png()
