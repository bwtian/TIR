#' Read NASA ASTER Global Emissivity Database (ASTER GED) Binary Datasets
#'
#'
#' @details layers: 19
#'          resolution: 100m
#'          dimensions: 1000*1000*19
#'          NA: -9999

#' @author Bingwei Tian
#'
#' @param emiB10m  layer1 is  Emissivity mean of Band10 scaled by 1000
#' @param emiB11m  layer2 is  Emissivity mean of Band11 scaled by 1000
#' @param emiB12m  layer3 is  Emissivity mean of Band12 scaled by 1000
#' @param emiB13m  layer4 is  Emissivity mean of Band13 scaled by 1000
#' @param emiB14m  layer5 is  Emissivity mean of Band14 scaled by 1000
#' @param emiB10s  layer6 is  Emissivity sdev of Band10 scaled by 10000
#' @param emiB11s  layer7 is  Emissivity sdev of Band11 scaled by 10000
#' @param emiB12s  layer8 is  Emissivity sdev of Band12 scaled by 10000
#' @param emiB13s  layer9 is  Emissivity sdev of Band13 scaled by 10000
#' @param emiB14s layer10 is  Emissivity sdev of Band14 scaled by 10000
#' @param LSTm    layer11 is  LST mean scaled by 100, Unit Kelvin(K)
#' @param LSTs    layer12 is  LST sdev scaled by 100, Unit Kelvin(K)
#' @param NDVIm   layer13 is  NDVI mean scaled by 100 at TOA
#' @param NDVIs   layer14 is  NDVI sdev scaled by 100 at TOA
#' @param Water   layer15 is  Land-Water Map
#' @param obs     layer16 is  Observations
#' @param Lat     layer17 is  Latitude scaled by 1000
#' @param Lon     layer18 is  Longitude scaled by 1000
#' @param GDEM    layer19 is  ASTER Global DEM

toRead  <- file("~/ASTB/AG100B.v003.43.142.0001.bin", "rb")

data.v  <- readBin(toRead, integer(), size = 4, n = 19000000)
close(toRead)

layer.l <- split(data.v, ceiling(seq_along(data.v)/1000000))
layer.d <- as.data.frame(layer.l)
layer.m  <- as.matrix(layer.d)
scales  <- c(1000, 1000, 1000, 1000, 1000,
             10000, 10000, 10000, 10000, 10000,
             100, 100, 100, 100,
             1, 1, 1000, 1000, 1)
layer.t  <- mapply("/",layer.d, scales)

#layer.t  <- layer.m/scales
if(!require(raster)){
        install.packages("raster")
}
emiB10m <- raster::rasterFromXYZ(layer.t[,c(17,18,1)])
summary(layer.d)

emiB11m
emiB12m
emiB13m
emiB14m
emiB10s
emiB11s
emiB12s
emiB13s
emiB14s
LSTm
LSTs
NDVIm
NDVIs
Water
obs
Lat
Lon
GDEM  <- raster::rasterFromXYZ(layer.t[,17:19])
