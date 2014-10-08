source("~/SparkleShare/TIR/demo/tirSettings.R")
#setwd(dir.toaTbKlccCenterMos)
setwd("~/toaTbKlccCenterMos/")
mos  <- raster("L8B10CenterMos.tif")
# cols  <-  bpy.colors(8)
# cols = oceColorsJet(255)
# brks  <- c(-20, -15,-10,-5, seq(0,20,4))
# brks
# plot(mos, maxpixels=1e6, col=cols, lab.breaks=brks)
# cellStats(mos,min)
# #levelplot(mos)
# xlims  <- c(1273000,1283000)
# ylims  <- c(1433000,1443000)
# plot(mos, maxpixels=1e6, col=cols, xlim = xlims, ylim = ylims, lab.breaks=brks, xlab = "Easting", ylab = "Northing")
# p1  <- gplot(mos, maxpixels=1e4) + geom_tile(aes(fill = value))
# p1
 p11  <- gplot(mos, maxpixels=1e4) +geom_raster(aes(fill = value))
# p11
# mos.p  <- rasterToPoints(mos)
#
# mos.df  <- as.data.frame(mos.p)
# names(mos.df)  <- c("x", "y", "t")
# p12  <- ggplot(mos.df, aes(x,y, fill = t)) + geom_raster()
#p12
# p13  <- ggplot(mos.df, aes(x,y, fill = t)) + geom_point()
# p13
# p14  <- ggplot(mos.df, aes(x,y, fill = t)) + geom_jitter()
# p14
# gc()
p2  <- p11 + scale_x_continuous(label = function(x) x/1000) +
     scale_y_continuous(label = function(x) x/1000) +
     xlab("Easting (km)") +
     ylab("Northing (km)") +
     theme_bw(base_size = 12, base_family = "times")
#p2
cols  <-  bpy.colors(8)
cols = oceColorsJet(255)
brks  <- c(-20, -15,-10,-5, seq(0,20,4))
p3  <- p2 + scale_fill_gradientn(colours = cols,
                          breaks = brks,
                          name = expression(Temperature~(degree*C)))
#p3
sourceDir("~/SparkleShare/geothermaR/R")
#ge.ggsave(p3
### North Arror and scale bar

library(ggplot2)
library(grid)
x = 1600000
y = 1700000
dx = 0
dy =10000
# north  <- geom_segment(mapping = aes(x=x,y=y, xend=x+dx, yend = y+dy),
#                         arrow = arrow(),
#                         size = 3,
#                         color = "blue") +
#         geom_point(data = north, mapping = aes(x,y),size = 5, shape =21, fill = "white") +
#         geom_text(data = north, x = x+dx, y = y+dy/2, label = "N", size = 12 )

p4  <- p3 + geom_segment(mapping = aes(x=x,y=y, xend=x+dx, yend = y+dy),
                       arrow = arrow(),
                       size = 3,
                       color = "blue") +
        geom_point(data = north, mapping = aes(x,y),size = 5, shape =21, fill = "white") +
        geom_text(data = north, x = x+dx, y = y+dy/2, label = "N", size = 12 )
p4
# d  <- as.data.frame(rbind(c(41.92, 140.87),
#                  c(42.23, 139.92),
#                  c(42.78, 141.31),
#                  c(43.47, 144.16)))
# d  <- d[,2:1]
# dlcc  <- phd.crsTransform(d, lon, lat, xlcc, ylcc, wgs84GRS,lccWgs84)
# round(dlcc)
# names(d)  <- c("lat","lon")
#
#
# dms2d <- function(d,m=0,s=0){
#     d1  <- d
#     d2  <- m/60
#     d3  <- s/3600
#     return(d1+d2+d3)
# }
#
#
#
#
#
#
# plot(mos, col = )
# volA  <- readRDS("~/Share500sda//2data/dataProduct/hkd/hkdVol20a_140812_175023.Rds")
# volAlcc  <- spTransform(volA, CRS(lccWgs84))
# proj4string(volA)
# plot(volAlcc, pch = 2, size = 6,add =T)
# plot(mos, col = oceColorsJet(255))
# plot(mos, col = bpy.colors(255))
# plot(volAlcc, pch = 2, size = 6,add =T)
# levelplot(mos)
# levelplot(mos,maxpixels=1e6, par.settings =  BuRdThem
