setwd("C:\\landsat\\luccHkd")
library(raster)
library(sp)
library(ggplot2)

lucc_r  <- raster("merge.tif")
plot(lucc_r)
### delete nodata and assign  
point_m  <- rasterToPoints(lucc_r, fun=function(x){x > 0})
point_d   <- as.data.frame(point_m)
point_d$x1 <- point_d$x
point_d$y1  <-  point_d$y
point_lcc <- phd.crsTransfer(point_d,x1,y1,xlcc,ylcc,jgd2000GRS, lccWgs84)
names(point_lcc) 
point <- point_lcc[, c(4,5,1,2,3)]
names(point)  <- c("lon", "lat", "xlcc", "ylcc", "code")
table(point$code)
point$emi  <- point$code
head(point)
p  <- point
class(p)
p$emi[p$emi == 1]  <- 0.95  # Rice paddy
p$emi[p$emi == 2]  <- 0.96  # Farm land
p$emi[p$emi == 5]  <- 0.97  # Forest
p$emi[p$emi == 6]  <- 0.93  # Vacant land
p$emi[p$emi == 7]  <- 0.94  # Buildings
p$emi[p$emi == 9]  <- 0.92  # Road
p$emi[p$emi == 10]  <- 0.95 # Other lands
p$emi[p$emi == 11]  <- 0.99 # Inland water
p$emi[p$emi == 14]  <- 0.96 # Seashore
p$emi[p$emi == 15]  <- 0.99 # Ocean water
p$emi[p$emi == 16]  <- 0.97 # Golf Courses
p$emi[p$emi == 17]  <- 0.95 # Railway

head(p)
pp  <- p[,c(3,4,6)]
# dir  <- c(1:9)
# mode(dir)  <- "character" 
# lapply(dir, dir.create)
#' rasterize
coordinates(pp)  <- c("xlcc", "ylcc")
gridded(pp)  <- TRUE
pp <- raster(point_d)
writeRaster(pp, filename="emissivity.tif", format="GTiff", overwrite=TRUE)

lst  <- b/(1 + (10.8 * b / 14380) * ln(a))



point_r[merge = 1]  <- 1
writeRaster()
summary(point_r)
point_r
plot(point_r)
class(point_r)

class(lucc)
class(point)

head(point_m)
summary(point_m)
plot(lucc)
help.search("point")
lucc_r
