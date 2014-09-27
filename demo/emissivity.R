library(raster)
library(sp)

source("~/SparkleShare/Rprofile/R/sourceDir.R")
sourceDir("~/SparkleShare/Rprofile/R/")
dir.tmp <- "~/Share500sda/Landsat8/raster_tmp"
dir.lulc  <- "~/Share500sda/Landsat8/at9_Database/LULC/"
rasterOptions(tmpdir = dir.tmp)
setwd(dir.lulc)
hkdLulc   <- raster("hkdLulc100.tif")
hkdmaskb  <- readRDS("~/SparkleShare/TIR/hkdmskb_grdi2d1h.Rds")
projectRaster(from = hkdLulc,  to = hkdmaskb,
              filename =  "hkdLulc100lcc.tif",
              overwrite=TRUE)
raster::removeTmpFiles(h = 1)
### Reclassify("is", "become")
lulc  <- raster("hkdLulc100lcc.tif")
m  <- c(1, 0.95, # Rice paddy
        2, 0.96, # Farm land
        5, 0.97, # Forest
        6, 0.93, # Vacant land
        7, 0.94, # Buildings
        9, 0.92, # Roads
        10, 0.95, # Other lands
        11, 0.99, # Inland water
        14, 0.96, # Seashore
        15, 0.99, # Ocean water
        16, 0.97, # Golf Courses
        17, 0.95 # Railway
        )
rclmat  <- matrix(m, ncol=2, byrow=TRUE)  <-
now <- format(Sys.time(), "_%y%m%d_%H%M%S")
emiName  <- paste0("hkdEmissivity", now, ".tif")
emi <- reclassify(lulc, rclmat, filename = emiName)
