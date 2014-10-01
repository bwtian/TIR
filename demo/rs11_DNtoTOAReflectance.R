source("./tirSettings.R")
setwd(dir.tif)  ## very important tips for use rLandsat8
l8.lst   <- lapply(dir(dir.tif), ReadLandsat8)
bandnames <-c("aerosol", "blue", "green", "red",
              "nir", "swir1", "swir2",
              "panchromatic",
              "cirrus")
for (i in l8.lst) {
        sceneName  <- i$metadata$landsat_scene_id
        if (!file.exists(file.path(dir.toaRef, sceneName))) {
                dir.create(file.path(dir.toaRef, sceneName), recursive = T)
        }
        for(j in bandnames){
                idx <- seq_along(bandnames)[sapply(bandnames, function(x) j %in% x)] # a number
                bandidx <- paste0("file_name_band_", idx)
                bandName <-  sapply(i, "[[", bandidx)[[1]]
                # fileName <- paste0(tools::file_path_sans_ext(bandName), "_TOARef.tif")
                Ref.rst  <- ToTOAReflectance(i, j)
                writeRaster(Ref.rst, filename = file.path(dir.toaRef, sceneName, bandName), overwrite = T)
                raster::removeTmpFiles(h = 0.5) ## Improtant tips for save hardisk
        }
}
