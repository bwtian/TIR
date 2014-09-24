
rs.tif2png <- function(dir = getwd()){
        inRaster <- list.files(path = dir, pattern = ".tif$", full.names = TRUE, recursive = TRUE)
        for (i in inRaster) {
                outName <- gsub("\\.tif", "\\.tif.png", i)
                r <- raster(i)
                png(filename = outName)
                plot(r, col = rainbow(255, start = 2/6, end = 1))
                dev.off()
        }
}
