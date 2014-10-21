setwd("/media/tian/Share300sda/www.eorc.jaxa.jp/ALOS/lulc/data/ver1402_LC_GeoTiff")
dir = getwd()
pattern = ".tif$"
tifs<- list.files(path= dir,
                    pattern= pattern,
                    all.files=TRUE,
                    full.names=TRUE,
                    recursive=TRUE,
                    ignore.case=TRUE)
library(raster)
rsts  <- lapply(tifs,raster)
merge  <- do.call(merge,rsts)
plot(merge)

writeRaster(merge,
            filename = file.path(dir, "jpLULCver1402Merge.tif"),
            overwrite = TRUE)

brks  <- c(1,2,3,4,5,6,8,10,11)
labs  <- c("Water", "Urban", "Paddy", "Crop","Grass", "DeciduousForest",
           "EvergreenForest", "Bare", "SnowAndIce")
cols  <- c("cyan", "red", "purple", "yellow", "yellowgreen", "springgreen", "forestgreen", "saddlebrown", "white")

plot(merge, breaks = brks, col = cols)
hkdGrid  <- readRDS("~/Dropbox/2data/hkd/hkd_grid1k_140521_140220.Rds")
plot(hkdGrid)
hkdLULC  <- mask(merge, hkdLand)
writeRaster(hkdLULC,
            filename = file.path(dir, "hkdLULCver1402Merge.tif"),
            overwrite = TRUE)
plot(hkdLULC, breaks = brks, col = cols, labels = labs)
xmin <- 139
xmax <- 146
ymin <- 41.4
ymax <- 45.8
bbox.SPDF <- ge.xy2bboxSPDF(xmin,xmax,ymin,ymax,wgs84GRS)
hkdSub  <- merge[bbox.SPDF,]
