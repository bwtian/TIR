library(raster)
r <- raster(system.file("external/test.grd", package="raster"))
s <- stack(r, r*2)
names(s) <- c('meuse', 'meuse x 2')

library(ggplot2)
library(rasterVis)
theme_set(theme_bw())
x  <-
        gplot(s) + geom_tile(aes(fill = value)) +
        facet_wrap(~ variable) +
        scale_fill_gradient(low = 'green', high = 'red',na.value = "white", limits = c(1, 33000 )) +
        coord_equal() + theme_bw(base_family = "times")
x
png("test.png")
x
dev.off()
summary(s)

setwd("~/Landsat8/tif")
images  <- list.files(no.. = TRUE)
images
for (i in images){
        i = images[[1]]
        require(raster)
        require(rasterVis)
        bands  <- (list.files(i, pattern = "*_B[^A-Z8]+\\.TIF$", full.names = T))
        raw.l  <- lapply(bands, raster)
        scaled.l  <- lapply(raw.l, scale)
        s  <- stack(raw.l)
}
raster::mean(raw.l)
c(raw.l, scaled.l, mean.l)
s  <- stack()
}
library(raster)
library(rasterVis)
r  <- lapply(dir(pattern = "*_B[^A-Z8]+\\.TIF$"), raster)
s  <- stack(r)

