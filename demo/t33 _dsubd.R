#' clip a data frame by a xmin, xmax
dsubd  <- function(data, sub){
        out  <- list() # a list of dataframe
        for (i in 1:nrow(sub)){
                xmin  <- sub[i,]$xmin
                xmax  <- sub[i,]$xmax
                ymin  <- sub[i,]$ymin
                ymax  <- sub[i,]$ymax
                out[[i]]  <-  data[x >= xmin & x <= xmax & y  >= ymin & y <= ymax,]
        }
        return(out)

}
source("~/SparkleShare/TIR/demo/tirSettings.R")
#setwd(dir.toaTbKlccCenterMos)
setwd("~/toaTbKlccCenterMos/")
mos  <- raster("L8B10CenterMos.tif")
mos.spdf  <- rasterToPoints(mos, spatial=TRUE)
mos.df  <- as.data.frame(mos.spdf)
names(mos.df)  <- c("x", "y", "tCenter")
head(mos.df)

d  <- as.data.frame(rbind(c(41.92, 140.87),
                          c(42.23, 139.92),
                          c(42.78, 141.31),
                          c(43.47, 144.16)))
names(d)  <- c("lat", "lon")
dlcc  <- ge.crsTransform(d, lon, lat, xlcc, ylcc, wgs84GRS,lccWgs84)
dlcc$xmin  <- round(dlcc$xlcc, -3) -2500
dlcc$xmax  <- round(dlcc$xlcc, -3) +2500
dlcc$ymin  <- round(dlcc$ylcc, -3) -2500
dlcc$ymax  <- round(dlcc$ylcc, -3) +2500
dlcc$id  <- 1:nrow(dlcc)



small  <- function(){
        data  <- mos.df
        sub  <- dlcc
        out  <- list() # a list of dataframe
        for (i in 1:nrow(sub)){
                xmin  <- sub[i,]$xmin
                xmax  <- sub[i,]$xmax
                ymin  <- sub[i,]$ymin
                ymax  <- sub[i,]$ymax
                x  <- data$x
                y  <- data$y
                out[[i]]  <-  data[x >= xmin & x <= xmax & y  >= ymin & y <= ymax,]
        }
        return(out)

}

clipper.l  <- small()

names(clipper.l)  <- c("a", "b", "c", "d")
str(clipper.l)
dfs <- lapply(clipper.l, get)
clipper.df  <- do.call(rbind, clipper.l)
clipper.df$id  <- as.factor(substr(row.names(clipper.df),1,1))
summary(clipper.df)
ggplot(clipper.l[[1]],aes(x,y, fill = tCenter)) + geom_point() +
facet_wrap(~ id)
cols = oceColorsJet(10)
brks  <- seq(-20, 20, 2)
grobs  <- lapply(clipper.l, function(d) {
        ggplot(d) +
        geom_raster(aes(x,y, fill = tCenter)) +
        scale_x_continuous(label = function(x) x/1000) +
        scale_y_continuous(label = function(x) x/1000) +
        xlab("x (km)") +
        ylab("y (km)") +
        scale_fill_gradientn(colours = cols,
                             na.value="white",
                             breaks = brks,
                             name = expression(~(degree*C))) +
                theme_bw(base_size = 12, base_family = "Times") +
                coord_equal()

        })

library(gridExtra)

do.call(grid.arrange, grobs)
# set.seed(1011)
# x  <- rnorm(100,mean  =50, sd = 25)
# y  <- rnorm(100,mean  =50, sd = 25)
# data  <- as.data.frame(cbind(x,y))
# data
# ### get rectangle map
# sub  <- data.frame(rbind(c(20,60,30,70),
#                          c(80,90,80,90)))
# sub
# names(sub)  <- c("xmin","xmax","ymin","ymax")
# library(ggplot2)
# g1  <- ggplot(data) + geom_point(aes(x =x,y =y)) +
#         geom_rect(data = sub, aes(xmin= xmin, xmax =xmax, ymin = ymin, ymax =ymax),
#                   col ="red", fill = NA)
#
# # subset
# g1 + xlim(80,90) + ylim(80,90)
# g1 + scale_x_continuous(limits = c(80, 90)) + scale_y_continuous(limits = c(80, 90))
# ## zoom out
# g1 + coord_cartesian(xlim = c(80,90), ylim = c(80,90))
# # subdata
# out  <- dsubd(data,sub)
# lapply(out,class)
# library(gridExtra)
# gl  <- lapply(out, function(df){
#         ggplot(df) + geom_point(aes(x =x,y =y))
# })
# do.call(grid.arrange, c(gl, list(ncol = 2)))
#
