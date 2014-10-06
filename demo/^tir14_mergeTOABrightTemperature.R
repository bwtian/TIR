source("./tirSettings.R")
setwd(dir.tif)  ## very important tips for use rLandsat8
l8.lst   <- lapply(dir(dir.tif), ReadLandsat8)
bandnames <-c("tirs1", "tirs2")
sceneList <- list.files(dir.toaTbk, full.names = TRUE)
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
        Ts10  <- Tb10/(1 + (L10*Tb10/a)*log(TbE)
        writeRaster(TbE, filename = file.path(dir.toaTe,emiName), overwrite = T)
        png(file.path(dir.toaTe,pngName))
        plot(TbE)
        dev.off()
        raster::removeTmpFiles(h = 1) ## Improtant tips for save hardisk
        }
}
