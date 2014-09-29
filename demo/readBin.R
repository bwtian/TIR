#'
#'
#'
#'
#'
#' @param emiB10m  scale 1000
#' @param emiB11m  scale 1000
#' @param emiB12m  scale 1000
#' @param emiB13m  scale 1000
#' @param emiB14m  scale 1000
#' @param
#' @param
#' @param
#' @param
#' @param
#' @param
#' @param










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
