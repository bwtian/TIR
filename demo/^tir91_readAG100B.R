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
saveRDS(hkdAG100B.df, file = "hkdAG100B_df.Rds")
