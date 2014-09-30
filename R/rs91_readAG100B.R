source("~/SparkleShare/Rprofile/R/sourceDir.R")
sourceDir("~/SparkleShare/Rprofile/R/")
sourceDir("~/SparkleShare/TIR/R/")

dir.tmp <- "~/Share500sda/Landsat8/raster_tmp"
rasterOptions(tmpdir = dir.tmp)

dir.AG100B  <- "~/Share500sda/AG100B/"

### Hokkaido Area
bins.l <- list.files(path=dir.AG100B,
                   pattern="bin$",
                   all.files=TRUE,
                   #full.names=TRUE,
                   recursive=TRUE,
                   ignore.case=TRUE)
bins.df <- as.data.frame(cbind(bins.l, do.call(rbind, strsplit(bins.l, "[.]"))))
colnames(bins.df)  <- c("ID", "Product", "Ver", "Lat", "Lon", "Num", "Format")
bins.df[4:5]  <-  sapply(bins.df[, 4:5], as.numeric)

hkd  <- subset(bins.df, Lon >= 139 & Lon <=146 & Lat <=46 )
plot(hkd$Lon, hkd$Lat)
hkd.l  <- hkd[[1]]
hkdAG100B.l   <- readAG100B(hkd.l)
hkdAG100B.df  <- do.call(rbind, hkdAG100B.l)
