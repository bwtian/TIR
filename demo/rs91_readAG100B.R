source("./tirSettings.R")
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
LSTmask  <- mask(LSTcrop, hkdmaskb, filename = "hkdAG100LSTmask.tif", overwrite=TRUE)
# plot(LSTcrop)
# proj4string(hkdAG100sgdfLSTmR)
# proj4string(hkdmaskb)
# extent(LSTcrop)
# extent(hkdmaskb)
LSTrsp  <- resample(LSTcrop, hkdmaskb)
LSTrsp2  <- resample(hkdAG100sgdfLSTmR, hkdmaskb)
hkdAG100rb  <- rasterize(hkdAG100spdf, hkdmaskb)




class(hkdAG100r)
saveRDS(hkdAG100r, file = "hkdAG100Br.Rds")
