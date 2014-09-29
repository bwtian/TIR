#' Read NASA ASTER Global Emissivity Database (ASTER GED) Binary Datasets
#'
#'
#'
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
layers  <- 19
res  <- 100
dims  <- 1000*1000*19
data.v  <- readBin(toRead, integer(), size = 4, n = dim)
close(toRead)

layer.l <-  split(data.v, ceiling(seq_along(data.v)/1000000))
str(layer.l)
n  <- seq_along(data.v)
max(n)
ceiling(max(n)/1000000)
###Method2
str(layer.l)
splitAt <- function(x, pos) unname(split(x, cumsum(seq_along(x) %in% pos)))
splitAt2 <- function(x, pos) {
        out <- list()
        pos2 <- c(1, pos, length(x)+1)
        for (i in seq_along(pos2[-1])) {
                out[[i]] <- x[pos2[i]:(pos2[i+1]-1)]
        }
        return(out)
}
splitAt2()
function(x, pos) {pos <- c(1L, pos, length(x) + 1L); Map(function(x, i, j) x[i:j], list(x), head(pos, -1L), tail(pos, -1L) - 1L)}
qs <- quantile(seqa, seq(0, 1, length.out =20))
y <- cut(x, round(qs), include.lowest = TRUE)
split(1:1900, 1:100)
splitAt <- function(x, pos) unname(split(x, cumsum(seq_along(x) %in% pos)))
splitAt(1:1900, 1:10*10)
cut(1:12, 2)
d  <- 1:1900
ceiling(seq_along(d)/100)
s  <- split(d, ceiling(seq_along(d)/100))
class(s)
split(1:1900, )
ceiling(0.2)
