#' Read NASA ASTER Global Emissivity Database (ASTER GED) Binary Datasets
#'
#'
#' @details Layers: 19
#'          Resolution: 100 m * 100 m
#'          Dimensions: 1000*1000*19
#'          NA: -9999

#' @author Bingwei Tian
#'
#' @param emiB10m  layer1 is  Emissivity mean of Band10 scaled by 1000
#' @param emiB11m  layer2 is  Emissivity mean of Band11 scaled by 1000
#' @param emiB12m  layer3 is  Emissivity mean of Band12 scaled by 1000
#' @param emiB13m  layer4 is  Emissivity mean of Band13 scaled by 1000
#' @param emiB14m  layer5 is  Emissivity mean of Band14 scaled by 1000
#' @param emiB10sd  layer6 is  Emissivity sdev of Band10 scaled by 10000
#' @param emiB11sd  layer7 is  Emissivity sdev of Band11 scaled by 10000
#' @param emiB12sd  layer8 is  Emissivity sdev of Band12 scaled by 10000
#' @param emiB13sd  layer9 is  Emissivity sdev of Band13 scaled by 10000
#' @param emiB14sd layer10 is  Emissivity sdev of Band14 scaled by 10000
#' @param LSTm    layer11 is  LST mean scaled by 100, Unit Kelvin(K)
#' @param LSTsd    layer12 is  LST sdev scaled by 100, Unit Kelvin(K)
#' @param NDVIm   layer13 is  NDVI mean scaled by 100 at TOA
#' @param NDVIsd   layer14 is  NDVI sdev scaled by 100 at TOA
#' @param Water   layer15 is  Land-Water Map
#' @param Obs     layer16 is  Observations, Images used
#' @param Lat     layer17 is  Latitude scaled by 1000
#' @param Lon     layer18 is  Longitude scaled by 1000
#' @param GDEM    layer19 is  ASTER Global DEM
#' @param bin     A  files need to read
dir.AG100B  <- "~/Share500sda/AG100B/"
bins <- list.files(path=dir.AG100B,
                   pattern="bin$",
                   all.files=TRUE,
                   full.names=TRUE,
                   recursive=TRUE,
                   ignore.case=TRUE)
readAG100B <- function(bins){
        out  <- list()
        for (i in bins) {
                toRead  <- file(i, "rb")
                data.v  <- readBin(toRead, integer(), size = 4, n = 19000000)
                close(toRead)
                layer.l <- split(data.v, ceiling(seq_along(data.v)/1000000))
                layer.d <- as.data.frame(layer.l)
                layer.d[layer.d == -9999]  <- NA
                layer.m  <- as.matrix(layer.d)
                scales  <- c(1000, 1000, 1000, 1000, 1000,
                             10000, 10000, 10000, 10000, 10000,
                             100, 100, 100, 100,
                             1, 1, 1000, 1000, 1)
                layer.ok  <- mapply("/",layer.d, scales)
                #layer.ok  <- layer.d/scales
                layer.df  <- as.data.frame(layer.ok)
                colnames(layer.df)  <- c("emiB10m", "emiB11m", "emiB12m",
                                         "emiB13m", "emiB14m", "emiB10sd",
                                         "emiB11sd", "emiB12sd", "emiB13sd",
                                         "emiB14sd", "LSTm"
                out[[seq_along(i)]]  <- layer.df
        }
        return(out)
}


col

st  <- readAG100B(bins[1])
head(st[[1]])
## Make SPDF
if(!require(sp)){
        install.packages("sp")
}

wgs84GRS <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"
coords  <- layer.ok[, c(17,18)]
m  <- as.matrix(coords) #sp need numeric matrix
mode(m)  <- "numeric"
sp  <- sp::SpatialPoints(m)
spdf <- sp::SpatialPointsDataFrame(m, data = as.data.frame(layer.ok))
#return(spdf)
spdf@data
}
# if(!require(raster)){
#         install.packages("raster")
# }
# emiB10m <- raster::rasterFromXYZ(layer.t[,c(17,18,1)])
# emiB11m <- raster::rasterFromXYZ(layer.t[,c(17,18,2)])
# emiB12m <- raster::rasterFromXYZ(layer.t[,c(17,18,3)])
# emiB13m <- raster::rasterFromXYZ(layer.t[,c(17,18,4)])
# emiB14m <- raster::rasterFromXYZ(layer.t[,c(17,18,5)])
# emiB10s <- raster::rasterFromXYZ(layer.t[,c(17,18,6)])
# emiB11s <- raster::rasterFromXYZ(layer.t[,c(17,18,7)])
# emiB12s <- raster::rasterFromXYZ(layer.t[,c(17,18,8)])
# emiB13s <- raster::rasterFromXYZ(layer.t[,c(17,18,9)])
# emiB14s <- raster::rasterFromXYZ(layer.t[,c(17,18,10)])
# LSTm <- raster::rasterFromXYZ(layer.t[,c(17,18,11)])
# LSTs <- raster::rasterFromXYZ(layer.t[,c(17,18,12)])
# NDVIm <- raster::rasterFromXYZ(layer.t[,c(17,18,13)])
# NDVIs <- raster::rasterFromXYZ(layer.t[,c(17,18,14)])
# Water <- raster::rasterFromXYZ(layer.t[,c(17,18,15)])
# obs <- raster::rasterFromXYZ(layer.t[,c(17,18,16)])
# Lat <- raster::rasterFromXYZ(layer.t[,c(17,18,17)])
# Lon <- raster::rasterFromXYZ(layer.t[,c(17,18,18)])
# GDEM  <- raster::rasterFromXYZ(layer.t[,17:19])

