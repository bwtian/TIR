#' Read NASA ASTER Global Emissivity Database (ASTER GED) Binary Datasets
#'
#'
#' @details resolution 100m, 19 layers, dimensions 1000*1000*19

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

layer.l <-  split(data.v, ceiling(seq_along(data.v)/1000000))
layer.m  <- as.matrix(layer.l)
