#' Read NASA ASTER Global Emissivity Database (ASTER GED) Binary Datasets
#'
#' @export
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
#' @param bins    A list of Binary files need to read
#' @return A list of dataframes
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
                layer.ok  <- mapply("/", layer.d, scales)
                #layer.ok  <- layer.d/scales
                layer.df  <- as.data.frame(layer.ok)
                colnames(layer.df)  <- c("emiB10m", "emiB11m", "emiB12m",
                                         "emiB13m", "emiB14m", "emiB10sd",
                                         "emiB11sd", "emiB12sd", "emiB13sd",
                                         "emiB14sd", "LSTm", "LSTsd", "NDVIm",
                                         "NDVIsd", "Water", "Obs",
                                         "Lat", "Lon", "GDEM")
                out[[seq_along(i)]]  <- layer.df
        }
        return(out)
}
