
sourceDir("~/SparkleShare/rLandsat8/src/main/R/rLandsat8/R")
sourceDir("~/SparkleShare/TIR/R/")
driver     <- "D://tian/Landsat8/"      # Windows
driver     <- "~/Share500sda/Landsat8/" # Linux
dir.tif    <- file.path(driver, "at0_Sensor")
dir.toaRad <- file.path(driver, "at1_TOA/toaRad")
dir.tmp    <- file.path(driver, "raster_tmp")
rasterOptions(tmpdir = dir.tmp)  
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
                pngName <- paste0(bandName, ".png")
                #fileName <- paste0(tools::file_path_sans_ext(bandName), "_TOARad.tif")               
                Rad.rst  <- ToTOARadiance(i, j)
                writeRaster(Rad.rst, filename = file.path(dir.toaRad, sceneName,
                                     bandName), overwrite = T)
                png(filename = file.path(dir.toaRad, sceneName, pngName))
                par(family = "times")
                plot(Rad.rst)
                title(main = paste("Radiance at TOA of Band ", idx),
                      sub = expression(paste("[", W*sr^-1*m^-2*mu*m^-1,"]")))
                dev.off()
                raster::removeTmpFiles(h = 0.5) ## Improtant tips for save hardisk
        }
}
