setwd("C:\\landsat\\L10Result\\106r")
library(raster)
extent  <- extent(c(1152500, 1673500, 1378500, 1843500))

r1  <- raster("RT_LC81060292013189LGN00_B10.TIF")
r2  <- raster("RT_LC81060302013189LGN00_B10.TIF")
r3  <- raster("RT_LC81060312013189LGN00_B10.TIF")  
r4  <- raster("RT_LC81070292014103LGN00_B10.TIF")
r5  <- raster("RT_LC81070302014119LGN00_B10.TIF")
r6  <- raster("RT_LC81070312014119LGN00_B10.TIF")
r7  <- raster("RT_LC81080292014078LGN00_B10.TIF")
r8  <- raster("RT_LC81080302014110LGN00_B10.TIF")
r9  <- raster("RT_LC81080312014110LGN00_B10.TIF")
### Crop
r11 <- crop(r1, extent)
r21 <- crop(r2, extent)
r31 <- crop(r3, extent)
r41 <- crop(r4, extent)
r51 <- crop(r5, extent)
r61 <- crop(r6, extent)
r71 <- crop(r7, extent)
r81 <- crop(r8, extent)
r91 <- crop(r9, extent)
## mosaic
mosaic(r11,r21,r31,r41,r51,r61,r71,r81,r91, fun = max, tolerance=15, filename="test.tif")
plot(r91)


library(raster)

# read the individual rasters into a list of RasterLayer objects
getwd()
input.rasters <- lapply(list.files(pattern=".TIF$"), raster)
input.rasters

# create an empty output raster that spans the full extent of all input
# rasters, and uses the same coordinate reference system; in this case
# we know that all input rasters share the same CRS, so we can
# arbitrarily extract CRS information from the first one
unionExtent <- function(x, ...) {
    objects <- c(x, list(...))
    if (length(objects) == 1) {
        return(extent(x))
    }
    e <- extent(objects[[1]])
    for (i in 2:length(objects)) {
        e2 <- extent(objects[[i]])
        e@xmin <- min(e@xmin, e2@xmin)
        e@xmax <- max(e@xmax, e2@xmax)
        e@ymin <- min(e@ymin, e2@ymin)
        e@ymax <- max(e@ymax, e2@ymax)
    }
    return(e)
}

full.extent <- unionExtent(input.rasters)
extent  <- extent(c(1152500, 1673500, 1378500, 1843500))
#full.extent <- unionExtent(r11,r21,r31,r41,r51,r61,r71,r81,r91)
bounding.raster <- raster(extent,
                          crs=projection(input.rasters[[1]]))

# set the output resolution to match the center tile (somewhat
# arbitrarily); this can also be specified manually if preferred
res(bounding.raster) <- 1000

# for each input raster, extract the corresponding sub-extent of the
# empty output raster, and use this as the basis for resampling
# !! note that this may take several minutes to run !!
resampled.rasters <- lapply(input.rasters, function(input.raster) {
    target.raster <- crop(bounding.raster, input.raster)
    resample(input.raster, target.raster, method="ngb")
})
l  <- resampled.rasters 
l[[1]]
# mosaic all 9 resampled rasters, taking the maximum pixel value in
# cases where there is overlap
raster.mosaic <- mosaic(l[[1]], l[[2]],l[[3]],l[[4]],l[[5]],l[[6]],l[[7]],l[[8]],l[[9]],l[[10]],fun=max, tolerance=15, filename="mergedmax.tif" ,overwrite = T)
plot(raster.mosaic)
m  <- raster.mosaic
dsn  <- "../"
layer  <- "HokkaidoUnion_lccWgs84.shp"
#ogrListLayers(shp
#ogrInfo(dsn, layer)
library(maptools)
jpn  <- readOGR(dsn, layer)
hkd  <- readShapePoly("../HokkaidoUnion_lccWgs84.shp")
class(hkd)
i  <- intersect(m, hkd)
i
plot(i)
#o  <- over(m,hkd)
library(rgeos)
class(hkd)
m_m  <- rasterToPoints(m)
m_d  <- as.data.frame(m_m)
names(m_d)
coordinates(m_d)  <- c("x", "y")
proj4string(m_d)  <- CRS(lccWgs84)
proj4string(hkd)  <- CRS(lccWgs84)
gi  <- gIntersection(hkd, m_d)
#o  <- over(m_d,hkd)
str(m_d)
#summary(o)
#plot(o)
summary(gi)
names(gi)
#bt  <- spsample(m_d, 78267, type = "regular")

bt  <- mask(m, hkd)
plot(bt)
str(bt)
head(bt)


a  <- raster("../result//emissivity1000.tif")
b  <- bt
#lst  <- b / (1 + (10.8 * b /14380 ) * log(a))

lst  <- raster("../result//lst.tif")
plot(lst)

lst.p  <- rasterToPoints(lst)
lst.d  <- as.data.frame(lst.p)
names(lst.d)
g0  <- ggplot(subset(lst.d, lst > 250), aes(x, y))+
    geom_raster(aes(fill = lst)) 
g0
tColors6 <- colorRampPalette(c("blue", "cyan", "green", "yellow", "orange", "red"))
color_grad  <- scale_fill_gradientn(colours = tColors6(200),name="Temperature (K)")
g1 <- g0 + 
    #geom_point(data = vol_df, aes(X, Y), color = "red", pch = 17) +
    scale_x_continuous(label = function(x) x/1000) +
    scale_y_continuous(label = function(x) x/1000) +
    color_grad +
    #scale_color_continuous(rainbow()) +
    #scale_color_manual(values= tColors6) +
    coord_equal() +
    xlab("x (km)") +
    ylab("y (km)") +
    theme_bw()
g1
dir.create("ggsave")
phd.ggsave(lst)
lst.sp  <- lst.d
names(lst.sp)
coordinates(lst.sp)  <- ~x+y
proj4string(lst.sp)  <- CRS(lccWgs84)
samp spsample(lst.sp, 1500, type = "random")





writeRaster(b, "BT.tif")
origin(a)
origin(b)
plot(a)
plot(b)
spplot(a)
# R script demonstrating use of the 'raster' package to resample and
# then mosaic rasters derived from multiple Landsat TM images that
# together cover the state of Ohio.
#
# Input Data
#  * TmB50MosaicImg*.tif: Nine partially overlapping GeoTIFF files
#    derived from Landsat Thematic Mapper images, band 5. All data are
#    WGS84 lat-lon. As a consequence of prior processing, these raster
#    all have slightly different resolutions (but all in the vicinity of
#    300m x 300m pixels) and thus are imperfectly aligned.
#  * Ohio.*: Shapefile containing the Ohio state border; used here
#    solely for visualization purposes
#
# Notes
#  * Resolution of the output mosaic here is arbitrarily set to be the
#    same as the 5th input raster, i.e. the middle raster in the 3x3
#    array of arranged inputs.
#  * Resampling is done using the bilinear method, which is reasonable
#    for continuous numeric data.
#  * The script may take several minutes to run to completion; testing
#    on different hardware, we've observed run times ranging from 1.5
#    minutes to ~10 minutes.
#
# Authors: Jim Regetz & Rick Reeves
# Originally created: October 2010 [reeves]
# Last modified: 15-Dec-2011 [regetz]
# National Center for Ecological Analysis and Synthesis (NCEAS),
# http://www.nceas.ucsb.edu/scicomp

require(raster)

# read the individual rasters into a list of RasterLayer objects
input.rasters <- lapply(list.files(pattern="^TmB50.*[.]tif$"), raster)

# create an empty output raster that spans the full extent of all input
# rasters, and uses the same coordinate reference system; in this case
# we know that all input rasters share the same CRS, so we can
# arbitrarily extract CRS information from the first one
full.extent <- unionExtent(input.rasters)
bounding.raster <- raster(full.extent,
                          crs=projection(input.rasters[[1]]))

# set the output resolution to match the center tile (somewhat
# arbitrarily); this could also be specified manually if preferred
res(bounding.raster) <- res(input.rasters[[5]])

# for each input raster, extract the corresponding sub-extent of the
# empty output raster, and use this as the basis for resampling
# !! note that this may take several minutes to run !!
resampled.rasters <- lapply(input.rasters, function(input.raster) {
    target.raster <- crop(bounding.raster, input.raster)
    # bilinear resampling is sensible for continuous data, but nearest
    # neighbor would be appropriate for categorical data
    resample(input.raster, target.raster, method="bilinear")
})

# mosaic all 9 resampled rasters, taking the maximum pixel value in
# cases where there is overlap
raster.mosaic <- mosaic(resampled.rasters, fun=max)

# read in Ohio state border shapefile and generate output map
ohio <- readOGR(".", "Ohio")
png("map-ohio-mosaic.png", height=500, width=500)
plot(raster.mosaic, col=grey((0:256)/256))
plot(ohio, border="yellow", add=TRUE)
dev.off()

# write the result out to a GeoTIFF, rounding pixel values to the
# nearest integer to preserve the original 1-byte integer format
writeRaster(round(raster.mosaic), filename="ohio-mosaic.tif",
            datatype="INT1U")
