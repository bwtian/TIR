source("~/SparkleShare/TIR/demo/tirSettings.R")
setwd(dir.toaTbKlccCenterMos)
#setwd("~/toaTbKlccCenterMos/")
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
#p1  <- gplot(mos, maxpixels=100000) + geom_tile(aes(fill = value))
# p11  <- gplot(mos, maxpixels=1e6) + geom_tile(aes(fill = value))
# p1
#ncell(mos)
# p11  <- gplot(mos, maxpixels=24539100) + geom_raster(aes(fill = value))
# p11
#mos.p  <- rasterToPoints(mos)
#mos.df  <- as.data.frame(mos.p)
mos.spdf  <- rasterToPoints(mos, spatial=TRUE)
mos.df  <- as.data.frame(mos.spdf)
names(mos.df)  <- c("x", "y", "tc")
#names(mos.df)  <- c("x", "y", "t")
p12  <- ggplot(mos.df, aes(x,y, fill = t)) + geom_raster()
#p12
#p13  <- ggplot(mos.df, aes(x,y, fill = t)) + geom_point()
# p13
# p14  <- ggplot(mos.df, aes(x,y, fill = t)) + geom_jitter()
# p14
# gc()
p2  <- p12 + scale_x_continuous(label = function(x) x/1000) +
     scale_y_continuous(label = function(x) x/1000) +
     xlab("Easting (km)") +
     ylab("Northing (km)")
#p2
#cols  <-  bpy.colors(8)
cols = oceColorsJet(10)
brks  <- c(-20, -15,-10,-5, 0, 5, 10, 15, 20)
p3  <- p2 + scale_fill_gradientn(colours = cols,
                                 na.value="white",
                          breaks = brks,
                          name = expression(Temperature~(degree*C)))
#p3

#ge.ggsave(p3
### North Arror and scale bar

library(ggplot2)
library(grid)

# north  <- geom_segment(mapping = aes(x=x,y=y, xend=x+dx, yend = y+dy),
#                         arrow = arrow(),
#                         size = 3,
#                         color = "blue") +
#         geom_point(data = north, mapping = aes(x,y),size = 5, shape =21, fill = "white") +
#         geom_text(data = north, x = x+dx, y = y+dy/2, label = "N", size = 12 )
north  <- data.frame(rbind(c(1600000,1400000,0,60000),c(1550000,1400000,100000,0)))
names(north)  <- c("x", "y", "dx", "dy")
p4  <- p3 +

        geom_segment(data = north[1,], aes(x=x,y=y, xend=x+dx, yend = y+dy),
                     arrow = arrow(angle =25),
                     size = 1,
                     color = "black"
                     ) +
        geom_segment(data = north[2,], aes(x=x,y=y, xend=x+dx, yend = y+dy),
                   arrow = arrow(angle =90, ends = "both", length = unit(0.1, "cm")),
                   size = 1
                   ) +
        geom_point(data = north[1,],
                   mapping = aes(x,y),
                   size = 3,
                   shape =21, fill = "white"
        ) +
        geom_text(x = north[1,]$x, y = north[1,]$y+north[1,]$dy/2,
                   label = "N"
                   #size =
                   ) +
        geom_text(x = north[1,]$x+north[1,]$dx/2, y = north[1,]$y -north[1,]$dy/4,
                  label = "100 km"
                  )
sourceDir("~/SparkleShare/geothermaR/R")
# p4
# ge.ggsave(p4)
# jp1  <- raster::getData('GADM', country='JPN', level=1, path = "~/Dropbox/2data//dataRaw/gadm2")
# plot(jp1)
# hkd  <- ge.LargestPolys(jp1, Polygon =T)
# plot(hkd)
# volA  <- readRDS("~/Dropbox/2data/dataProduct/jpVolcanoes/jpVol110_140812_174525.Rds")
# volQ  <- readRDS("~/Dropbox/2data/dataProduct/jpVolcanoes/jpVol455_140812_172148.Rds")
# proj4string(volA)  <- proj4string(hkd)
# proj4string(volQ)  <- proj4string(hkd)
# volQhkd <- volQ[hkd,]
# volAhkd <- volA[hkd,]
# volQhkdlcc  <- spTransform(volQhkd, CRS(lccWgs84))
# volAhkdlcc  <- spTransform(volAhkd, CRS(lccWgs84))
# volQhkdlcc@coords

# plot(volAhkd)
# p5  <- p4 + geom_point(data = as.data.frame(volQhkdlcc@coords),
#                 aes(as.numeric(lon), as.numeric(lat),
#                 color="blue"), shape = 2, alpha = 0.7
#                 ) +
#      geom_point(data = as.data.frame(volAhkdlcc@coords),
#                 aes(as.numeric(lon), as.numeric(lat),
#                 color="red"),  shape = 2, size = 3
#                 ) +
#         scale_color_manual(name =  "Volcanoes", values = c("blue","red"), labels = c("Quaternary Volcanoes","Active Volcanoes"))
# #ge.ggsave(p5)

### focused on rect
d  <- as.data.frame(rbind(c(41.92, 140.87),
                  c(42.23, 139.92),
                  c(42.78, 141.31),
                  c(43.47, 144.16)))
names(d)  <- c("lat", "lon")
dlcc  <- ge.crsTransform(d, lon, lat, xlcc, ylcc, wgs84GRS,lccWgs84)
dlcc$xmin  <- round(dlcc$xlcc, -3) -2500
dlcc$xmax  <- round(dlcc$xlcc, -3) +2500
dlcc$ymin  <- round(dlcc$ylcc, -3) -2500
dlcc$ymax  <- round(dlcc$ylcc, -3) +2500
dlcc$id  <- 1:nrow(dlcc)
# ggplot() +
#         geom_rect(data = dlcc,
#                   aes(NULL, NULL, xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = NULL, color = NULL), alpha =0.1, color = "red")

p6  <- p4 + geom_rect(data = dlcc,
               aes(NULL, NULL, xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = NULL, color = NULL), alpha =0.1, color = "red")
#
# p3 +  coord_cartesian(xlim = c(dlcc[1,]$xmin, dlcc[1,]$xmax),                          ylim = c(dlcc[1,]$ymin, dlcc[1,]$ymax))
# p3)
p7  <- p6 + theme_bw(base_size = 12, base_family = "Times") + coord_equal()
#ggsave("p71009.pdf")
ge.ggsave(p7)

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
ps1  <- p7 +  xlim(dlcc[1,]$xmin, dlcc[1,]$xmax) +
              ylim(dlcc[1,]$ymin, dlcc[1,]$ymax)

ps2  <- p7 +  xlim(dlcc[2,]$xmin, dlcc[2,]$xmax) +
        ylim(dlcc[2,]$ymin, dlcc[2,]$ymax)
ps3  <- p7 +  xlim(dlcc[3,]$xmin, dlcc[3,]$xmax) +
        ylim(dlcc[3,]$ymin, dlcc[3,]$ymax)
ps4  <- p7 +  xlim(dlcc[4,]$xmin, dlcc[4,]$xmax) +
        ylim(dlcc[4,]$ymin, dlcc[4,]$ymax)
library(gridExtra)
# tiff("ps.tif")
# grid.arrange(ps1, ps2, ps3, ps4, ncol=2)
# dev.off()

