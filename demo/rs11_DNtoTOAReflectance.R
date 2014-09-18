
sourceDir("~/SparkleShare/rLandsat8/src/main/R/rLandsat8/R")
sourceDir("~/SparkleShare/TIR/R/")
dir.tif  <- "~/Share500sda/Landsat8/at0_Sensor"
dir.toaRef  <- "~/Share500sda/Landsat8/at1_TOA/toaRef"
library(raster)
setwd(dir.tif)  ## very important tips for use rLandsat8
## files  <- sapply(file.path(dir.tif,list.files(dir.tif)), tools::file_path_as_absolute)
## basename(files)
## l8.lst  <- lapply(basename(files), ReadLandsat8)
l8.lst   <- lapply(dir(dir.tif), ReadLandsat8)
bandnames <-c("aerosol", "blue", "green", "red",
              "nir", "swir1", "swir2",
              "panchromatic",
              "cirrus",
              "tirs1", "tirs2")
for (i in l8.lst) {
        sceneName  <- i$metadata$landsat_scene_id
        if (!file.exists(file.path(dir.toaRad, sceneName))) {
                dir.create(file.path(dir.toaRad, sceneName), recursive = T)
        }
        for(j in bandnames){
                idx <- seq_along(bandnames)[sapply(bandnames, function(x) j %in% x)] # a number
                bandidx <- paste0("file_name_band_", idx)
                bandName <-  sapply(i, "[[", bandidx)[[1]]
                fileName <- paste0(tools::file_path_sans_ext(bandName), "_TOARef.tif")
                Ref.rst  <- ToTOAReflectance(i, j)
                writeRaster(Ref.rst, filename = file.path(dir.toaRef, sceneName, fileName), overwrite = T)
                raster::removeTmpFiles(h = 0.2) ## Improtant tips for save hardisk
        }
}
