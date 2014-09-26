#' Make 2D 100m * 100m Grid with lccWgs84 CRS
#' 
library(sp)
library(gstat)  
library(plyr)
library(rgdal)
library(maptools)
library(rgeos)


## BBOX
P  <- Polygon(cbind(c(1152500, 1152500, 1673500, 1673500, 1152500),
                    c(1372500, 1843500, 1843500, 1372500, 1372500)))
Ps  <- Polygons(list(P), "P")
SpP  <- SpatialPolygons(list(Ps))
bbgrid1h  <- spsample(SpP,type = "regular", cellsize = c(100,100),
                      offset = c(0.5, 0.5))
proj4string(bbgrid1h) <- CRS(lccWgs84)

## Hokkaido Shape
