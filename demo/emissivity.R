library(raster)
library(sp)
library(ggplot2)

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

emi  <- raster("hkdLulc100lcc.tif")
p$emi[p$emi == 1]  <- 0.95  # Rice paddy
p$emi[p$emi == 2]  <- 0.96  # Farm land
p$emi[p$emi == 5]  <- 0.97  # Forest
p$emi[p$emi == 6]  <- 0.93  # Vacant land
p$emi[p$emi == 7]  <- 0.94  # Buildings
p$emi[p$emi == 9]  <- 0.92  # Road
p$emi[p$emi == 10]  <- 0.95 # Other lands
p$emi[p$emi == 11]  <- 0.99 # Inland water
p$emi[p$emi == 14]  <- 0.96 # Seashore
p$emi[p$emi == 15]  <- 0.99 # Ocean water
p$emi[p$emi == 16]  <- 0.97 # Golf Courses
p$emi[p$emi == 17]  <- 0.95 # Railway


writeRaster(pp, filename="emissivity.tif", format="GTiff", overwrite=TRUE)

lst  <- b/(1 + (10.8 * b / 14380) * ln(a))



point_r[merge = 1]  <- 1
writeRaster()
summary(point_r)
point_r
plot(point_r)
class(point_r)

class(lucc)
class(point)

head(point_m)
summary(point_m)
plot(lucc)
help.search("point")
lucc_r
