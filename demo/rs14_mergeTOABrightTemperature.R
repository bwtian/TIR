
source("~/SparkleShare/Rprofile/R/sourceDir.R")
sourceDir("~/SparkleShare/rLandsat8/src/main/R/rLandsat8/R")
sourceDir("~/SparkleShare/TIR/R/")
dir.tif   <- "~/Share500sda/Landsat8/at0_Sensor"
dir.toaTb <- "~/Share500sda/Landsat8/at1_TOA/toaTb"
dir.toaTs <- "~/Share500sda/Landsat8/at1_TOA/toaTs"
dir.toaTe <- "~/Share500sda/Landsat8/at1_TOA/toaTe"
dir.tmp   <- "~/Share500sda/Landsat8/raster_tmp"
library(raster)
rasterOptions(tmpdir = dir.tmp)
setwd(dir.tif)  ## very important tips for use rLandsat8
## files  <- sapply(file.path(dir.tif,list.files(dir.tif)), tools::file_path_as_absolute)
## basename(files)
## l8.lst  <- lapply(basename(files), ReadLandsat8)
l8.lst   <- lapply(dir(dir.tif), ReadLandsat8)
bandnames <-c("tirs1", "tirs2")
sceneList <- list.files(dir.toaTb, full.names = TRUE)
for (i in sceneList) {
        bandList <- list.files(sceneList, full.names = TRUE)
        emiName <- paste0(basename(i), ".tif")
        pngName <- paste0(emiName,".png")
        Tb10 <- raster::raster(bandList[1])
        Tb11 <- raster::raster(bandList[2])
        TbS  <- raster::stack(Tb10, Tb11)
        a <- 1.438*10^-2
        L10 <- 10.9
        L11 <- 12.0
        TbE  <- exp((a*(Tb10 - Tb11))/(Tb10*Tb11*(L10-L11)))
        writeRaster(TbE, filename = file.path(dir.toaTe,emiName), overwrite = T)
        png(file.path(dir.toaTe,pngName))
        plot(TbE)
        dev.off()
        raster::removeTmpFiles(h = 1) ## Improtant tips for save hardisk
}

