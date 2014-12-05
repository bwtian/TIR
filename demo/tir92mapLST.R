source("~/SparkleShare/TIR/demo/tirSettings.R")
mapRaster  <- levelplot(LSTCenter, par.settings = BuRdTheme, FUN.margin=median, axis.margin = TRUE,
                        at = seq(-40, 40,2),
                        xlab='Easting (km)', ylab='Northing (km)',
                        yscale.components=function(...){
                                yc <- yscale.components.default(...)
                                yc$left$labels$labels <- yc$left$labels$at/1000 ## convert to strings as pct
                                return(yc)
                        },
                        xscale.components=function(...){
                                xc <- xscale.components.default(...)
                                xc$bottom$labels$labels <- xc$bottom$labels$at/1000 ## convert to strings as pct
                                return(xc)
                        }
)
volA  <- readRDS("~/Dropbox/2data/dataProduct/hkd//hkdVol20a_140812_175023.Rds")
volQ  <- readRDS("~/Dropbox/2data/dataProduct/hkd//hkdVol_140530_113923.Rds")
plot(volA)
plot(volQ)
mapVector  <-
        + spplot()
layer({SpatialPolygonsRescale(layout.north.arrow(),
                              offset = c(1800000,1600000),
                              scale = 400)
})

levelplot(LSTCenter, at=seq(min(LSTCenter[], na.rm=T), max(LSTCenter[], na.rm=T), len=100),
           col.regions=colorRampPalette(c('#2c7bb6', '#abd9e9', '#ffffbf',
'#fdae61', '#d7191c')))
summary(LSTmask)

show.settings()
