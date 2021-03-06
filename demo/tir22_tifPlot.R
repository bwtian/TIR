source("~/SparkleShare/TIR/demo/tirSettings.R")
setwd(dir.toaTbKlccCenterMos)
mos  <- raster("L8B10CenterMos.tif")
volA  <- readRDS("~/Share500sda//2data/dataProduct/hkd/hkdVol20a_140812_175023.Rds")
volAlcc  <- spTransform(volA, CRS(lccWgs84))
proj4string(volA)
plot(volAlcc, pch = 2, size = 6,add =T)
plot(mos, col = oceColorsJet(255))
plot(mos, col = bpy.colors(255))
plot(volAlcc, pch = 2, size = 6,add =T)
levelplot(mos)
levelplot(mos,maxpixels=1e6, par.settings =  BuRdTheme)
