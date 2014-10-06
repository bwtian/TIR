source("./tirSettings.R")
setwd(dir.tif)  ## very important tips for use rLandsat8
l8.lst   <- lapply(dir(dir.tif), ReadLandsat8)
bandnames <-c("tirs1", "tirs2")
for (i in l8.lst) {
        sceneName  <- i$metadata$landsat_scene_id
        if (!file.exists(file.path(dir.toaBT, sceneName))) {
                dir.create(file.path(dir.toaBT, sceneName), recursive = T)
        }
        for(j in bandnames){
                idx <- seq_along(bandnames)[sapply(bandnames, function(x) j %in% x)] + 9 # a number
                bandidx <- paste0("file_name_band_", idx)
                bandName <-  sapply(i, "[[", bandidx)[[1]]
                #fileName <- paste0(tools::file_path_sans_ext(bandName), "_TOABT.tif")
                BT.rst  <- ToAtSatelliteBrightnessTemperature(i, j)
                writeRaster(BT.rst, filename = file.path(dir.toaBT, sceneName, bandName), overwrite = T)
                raster::removeTmpFiles(h = 1) ## Improtant tips for save hardisk
        }
}
