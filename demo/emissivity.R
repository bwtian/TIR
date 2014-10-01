source("./tirSettings.R")
setwd(dir.lulc)
hkdLulc   <- raster("hkdLulc100.tif")
hkdmaskb  <- readRDS("~/SparkleShare/TIR/hkdmskb_grdi2d1h.Rds")
projectRaster(from = hkdLulc,  to = hkdmaskb, method = "ngb",
              filename =  "hkdLulc100lcc.tif",
              overwrite=TRUE)
raster::removeTmpFiles(h = 1)
### Reclassify("is", "become")
lulc  <- raster("hkdLulc100lcc.tif")
hkdlulc <-  mask(lulc, hkdmaskb)
m  <- c(1, 0.95, # Rice paddy
        2, 0.96, # Farm land
        5, 0.98, # Forest
        6, 0.93, # Vacant land
        7, 0.94, # Buildings
        9, 0.93, # Roads
        10, 0.95, # Other lands
        11, 0.99, # Inland water
        14, 0.96, # Seashore
        15, 0.99, # Ocean water
        16, 0.97, # Golf Courses
        17, 0.95) # Railway

## Reclass matirx
rclmat <- matrix(m, ncol=2, byrow=TRUE)
#now <- format(Sys.time(), "_%y%m%d_%H%M%S")
#emiName  <- paste0("hkdEmissivity", now, ".tif")
hkdEmi <- reclassify(hkdlulc, rclmat, filename = "hkdEmissivity.tif")
#log(seq(0.910,0.999,0.001))
