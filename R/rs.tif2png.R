
rs.tif2png <- function(inRaster){
        for (i in inRaster) {
                outName <- gsub("\\.tif", "\\.tif.png", i)
                pict <- raster::spplot(i, col.regions = rainbow(200, start = 2/6, end = 1))
                png(filename = outName)
                print(pict)
                dev.off()
        }
}
