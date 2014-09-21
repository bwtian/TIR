
rs.tif2png <- function(inRaster){
        for (i in inRaster) {
                outName <- gsub("\\.tif", "\\.tif.png", i)
                r <- raster(i)
                pict <- spplot(r, col.regions = rainbow(200, start = 2/6, end = 1))
                png(filename = outName)
                print(pict)
                dev.off()
        }
}
