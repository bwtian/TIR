source("~/SparkleShare/Rprofile/R/sourceDir.R")
sourceDir("~/SparkleShare/Rprofile/R/")
sourceDir("~/SparkleShare/TIR/R/")

dir.AG100B  <- "~/Share500sda/AG100B/"
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
hkdAG100  <- readRDS("~/Share500sda/AG100B/hkdAG100B.Rds")
hohead(hkdAG100)
coordinates(hkdAG100)  <- ~Lon+Lat
proj4string(hkdAG100)  <- wgs84GRS
hkdmaskb  <- readRDS("~/SparkleShare/TIR/hkdmskb_grdi2d1h.Rds")
proj4string(hkdmaskb)
dir.tmp <- "~/Share500sda/Landsat8/raster_tmp"
rasterOptions(tmpdir = dir.tmp)
rasterize(hkdAG100, hkdmaskb, filename = "hkdAG100r.tif")

