source("~/SparkleShare/TIR/demo/tirSettings.R")
setwd(dir.AG100B)
### Hokkaido Area
bins.l <- list.files(path=dir.AG100B,
                   pattern="bin$",
                   all.files=TRUE,
                   #full.names=TRUE,
                   recursive=TRUE,
                   ignore.case=TRUE)
bins.df <- as.data.frame(cbind(bins.l, do.call(rbind, strsplit(bins.l, "[.]"))), stringsAsFactors = FALSE)
colnames(bins.df)  <- c("ID", "Product", "Ver", "Lat", "Lon", "Num", "Format")
bins.df[,4]  <- as.numeric(bins.df[,4])
bins.df[,5]  <- as.numeric(bins.df[,5])
hkd  <- subset(bins.df, Lon >= 139 & Lon <=146 & Lat <=46 )
#plot(hkd$Lon, hkd$Lat)
hkd.l  <- hkd[[1]]
hkdAG100B.l   <- readAG100B(hkd.l)
hkdAG100B.df  <- do.call(rbind, hkdAG100B.l)
saveRDS(hkdAG100B.df, file = "hkdAG100B.Rds")
### Rasterlize
library(sp)
library(raster)
library(plotKML)
hkdAG100spdf  <- readRDS("~/Share500sda/AG100B/hkdAG100B.Rds")
head(hkdAG100spdf)
coordinates(hkdAG100spdf)  <- ~Lon+Lat
proj4string(hkdAG100spdf)  <- wgs84GRS
gc()
hkdmaskb  <- readRDS("~/SparkleShare/TIR/hkdmskb_grdi2d1h.Rds")
proj4string(hkdmaskb)
gc()
dir.tmp <- "D:./rasterTmp/"
rasterOptions(tmpdir = dir.tmp)
extract
####sgdf
hkdAG100sgdfLSTm  <- vect2rast(hkdAG100spdf, fname = "LSTm", cell.size = 100)
summary(hkdAG100sgdfLSTm)
hkdAG100sgdfLSTmR  <- raster(hkdAG100sgdfLSTm)
LSTcrop  <- crop(hkdAG100sgdfLSTmR, hkdmaskb, filename = "hkdAG100sgdfLSTm.tif", overwrite=TRUE)

# plot(LSTcrop)
# proj4string(hkdAG100sgdfLSTmR)
# proj4string(hkdmaskb)
# extent(LSTcrop)
# extent(hkdmaskb)
LSTrsp  <- resample(LSTcrop, hkdmaskb)
LSTrsp2  <- resample(hkdAG100sgdfLSTmR, hkdmaskb)
compareRaster(LSTrsp, LSTrsp2)
extent(LSTrsp2)
extent(hkdmaskb)
plot(LSTrsp)
summary(LSTrsp)
LSTmask  <- mask(LSTrsp2, hkdmaskb, filename = "hkdAG100LSTmask.tif", overwrite=TRUE)
plot(LSTmask, zlim =c(252, 320))

levelplot(LSTmask, par.settings = BuRdTheme)

levelplot(LSTmask, par.settings = BuRdTheme, at  = seq(252,320, 2))
plotKML(LSTmask)
LSTCenter  <- scale(LSTmask, center = TRUE, scale = FALSE)
summary(LSTCenter)
windowsFonts(Times=windowsFont("TT Times New Roman"))
levelplot(LSTCenter, par.settings = BuRdTheme, FUN.margin=median, axis.margin = TRUE,
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

          })
+
        layer({SpatialPolygonsRescale(layout.north.arrow(),
                                       offset = c(1800000,1600000),
                                       scale = 400)
        })

# levelplot(LSTCenter, at=seq(min(LSTCenter[], na.rm=T), max(LSTCenter[], na.rm=T), len=100),
#           col.regions=colorRampPalette(c('#2c7bb6', '#abd9e9', '#ffffbf',
#                                          '#fdae61', '#d7191c')))
summary(LSTmask)

show.settings()

####
hkdAG100rb  <- rasterize(hkdAG100spdf, hkdmaskb)
class(hkdAG100r)
saveRDS(hkdAG100r, file = "hkdAG100Br.Rds")
