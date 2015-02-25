source("~/SparkleShare/Rprofile/R/Rsettings/phdRsettings.R")

### Make study Boundary

jp1.SPDF  <- getData('GADM', country='JPN', level=1, path = "~/Dropbox/2data//dataRaw/gadm2")
basemap.r  <- readRDS("~/Dropbox/2data/dataProduct/hkd/hkd_google_satellite_142.5_43.5_zoom6_141018_2339.Rds")
jpVolA.spdf  <- readRDS("~/Dropbox/2data/dataProduct/jpVolcanoes/jpVol110_140812_174525.Rds")
jpVolQ.spdf  <- readRDS("~/Dropbox/2data/dataProduct/jpVolcanoes/jpVol455_140812_172148.Rds")
sap.spdf  <- readRDS("~/Dropbox/2data/data/Sapporo_140817_162919.Rds")
xmin <- 139
xmax <- 146
ymin <- 41.4
ymax <- 45.8
bbox.SPDF <- ge.xy2bboxSPDF(xmin,xmax,ymin,ymax,wgs84GRS)
bh <- readRDS("~/Dropbox/2data/dataProduct/hkd/hkd_profiles_140806_164333.Rds")
bh_xy  <- bh[!duplicated(bh$ID),]
bh_xy$grp <- cut(bh_xy$TD, breaks = c(0,500,1000,1500,2000,2200),
                 labels = c("0-500","500-1000","1000-1500","1500-2000","2000-2200"))

proj4string(jpVolQ.spdf) <- proj4string(bbox.SPDF)
volQ <- jpVolQ.spdf[bbox.SPDF,]
volA <- jpVolA.spdf[bbox.SPDF,]
hkdLand  <- ge.LargestPolys(jp1.SPDF, Polygon=T)
plot(hkdLand)
volQ2  <- jpVolQ.spdf[hkdLand,]


limitsX  <- c(138,147)
breaksX  <- seq(limitsX[1], limitsX[2],1)
labelsX=parse(text=paste(breaksX, "^o ", "*E", sep=""))
limitsY  <- c(41,47)
breaksY  <- seq(limitsY[1],limitsY[2],1)
labelsY=parse(text=paste(breaksY, "^o ", "*N", sep=""))
## Layer0: Base map
ggBH  <-  ggmap(basemap.r, extent = "panel") +
        ### Layers
        geom_point(data = bh_xy, aes(Lon, Lat,fill = grp, size = grp),
                   shape = 21, alpha = 0.9) +
        ### X
        xlab("Lontitude") +
        scale_x_continuous(breaks=breaksX,
                           labels=labelsX,
                           limits=limitsX,
                           expand = c(0.01,0.01)) +
        theme(axis.text.y = element_text(angle = 90, hjust = 0.5, vjust = 0)) +
        ### Y
        ylab("Latitude") +
        scale_y_continuous(breaks=breaksY,
                           labels=labelsY,
                           limits=limitsY,
                           expand = c(0.01,0.01)) +
        ###Legend
        ### Size
        labs(size = "Borehole Depth (m)") +
        scale_size_manual(values=c(1,1.5,2,3,4)) +
        ### fill
        scale_fill_brewer("Borehole Depth (m)", palette="Blues")
ggBH

        ### Color

ggVol  <- ggBH  +
        geom_point(data = volQ@data,
                   aes(as.numeric(lon), as.numeric(lat),
                       color="blue"), shape = 17, alpha = 0.7) +
        geom_point(data = volA@data,
                   aes(as.numeric(lon), as.numeric(lat),
                      color="red"),  shape = 17, size = 3)  +
        scale_color_manual(name =  "Volcanoes", values = c("orange","red"), labels = c("Quaternary Volcanoes","Active Volcanoes")) +
        geom_path(data = volQ2@data, aes(as.numeric(lon), as.numeric(lat)),size = 12, alpha = 0.3, colour = "yellow",lineend = "round")


ggVol

ggSap  <- ggVol + geom_point(data = sap.spdf, aes(x = lon, y = lat), colour = "White")  + geom_text(data = sap.spdf, aes(x = lon, y = lat, label = name), hjust = -0.1,family="Times", face="italic", colour="white")

# ggSap

ggBar  <- ggSap +scaleBar(lon = 145.5, lat = 41, distanceLon = 50, distanceLegend = 30,distanceLat = 15, dist.unit = "km", arrow.length = 60, arrow.distance = 500, arrow.North.size = 4,legend.colour = "white", arrow.North.color = "white", arrow.colour = "blue")

ggFont  <- ggBar +
        #coord_equal() +
        theme_bw(base_family = "Times", base_size = 12)
ggFont
f01_hkdStudyArea  <- ggFont
##ge.ggsave(hkdStudyArea)

## ggWRS2

library(wrspathrow)
wrs2.SPDF  <- pathrow_num(x = hkdLand, as_polys = TRUE)
plot(wrs2.SPDF,col = "red")
wrs2.SPDF@data
wrs2.df  <- fortify(wrs2.SPDF)

ggWRS  <- ggFont + geom_polygon(aes(long,lat,group=group),
                    color = "grey", alpha = 0.3, fill = NA,
                    linetype = 3,
                    data=wrs2.df) +
        geom_point(data = wrs2.SPDF@data, color = "black",
                   aes(x = centroid_x, y = centroid_y, shape = MODE ))

ggWRS2  <- ggWRS +  geom_text(data = wrs2.SPDF@data,
                  aes(x = centroid_x, y = centroid_y),
                  label = paste(wrs2.SPDF@data$PATH, wrs2.SPDF@data$ROW," "),
                  family="Times", face = "Italic", colour="black") +
        scale_shape_manual(name =  " WRS 2", values = 20 , labels = c("Path and Row"))

ggWRS2
f01_hkdStudyArea  <-ggWRS2
ge.ggsave(f01_hkdStudyArea)


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
library(raster)
r <- raster(system.file("external/test.grd", package="raster"))
s <- stack(r, r*2)
names(s) <- c('meuse', 'meuse x 2')

library(ggplot2)
library(rasterVis)
theme_set(theme_bw())
x  <-
        gplot(s) + geom_tile(aes(fill = value)) +
        facet_wrap(~ variable) +
        scale_fill_gradient(low = 'green', high = 'red',na.value = "white", limits = c(1, 33000 )) +
        coord_equal() + theme_bw(base_family = "times")
x
png("test.png")
x
dev.off()
summary(s)

setwd("~/Landsat8/tif")
images  <- list.files(no.. = TRUE)
images
for (i in images){
        i = images[[1]]
        require(raster)
        require(rasterVis)
        bands  <- (list.files(i, pattern = "*_B[^A-Z8]+\\.TIF$", full.names = T))
        raw.l  <- lapply(bands, raster)
        scaled.l  <- lapply(raw.l, scale)
        s  <- stack(raw.l)
}
raster::mean(raw.l)
c(raw.l, scaled.l, mean.l)
s  <- stack()
}
library(raster)
library(rasterVis)
r  <- lapply(dir(pattern = "*_B[^A-Z8]+\\.TIF$"), raster)
s  <- stack(r)

#' Read NASA ASTER Global Emissivity Database (ASTER GED) Binary Datasets
#'
#'
#' @details Layers: 19
#'          Resolution: 100 m * 100 m
#'          Dimensions: 1000*1000*19
#'          NA: -9999

#' @author Bingwei Tian [2014-09-30 Tue]
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


st  <- readAG100B(bins[1])
head(st[[1]])
summary(st[[1]])
## Make SPDF
if(!require(sp)){
        install.packages("sp")
}
layer.ok  <- st[[1]]
spatial
wgs84GRS <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"
coords  <- layer.ok[, c(18,17)]
length(layer.ok)
m  <- as.matrix(coords) #sp need numeric matrix
mode(m)  <- "numeric"
sp  <- sp::SpatialPoints(m)
spdf <- sp::SpatialPointsDataFrame(m, data = as.data.frame(layer.ok))
#return(spdf)
summary(spdf@data)
}
plot(spdf)
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

# Generate data
pp <- function (n,r=4) {
        x <- seq(-r*pi, r*pi, len=n)
        df <- expand.grid(x=x, y=x)
        df$r <- sqrt(df$x^2 + df$y^2)
        df$z <- cos(df$r^2)*exp(-df$r/6)
        df
}
p <- ggplot(pp(20), aes(x=x,y=y))

p + geom_tile() #pretty useless!

# Add aesthetic mappings
p + geom_tile(aes(fill=z))

# Change scale
p + geom_tile(aes(fill=z)) + scale_fill_gradient(low="green", high="red")

# Use qplot instead
qplot(x, y, data=pp(20), geom="tile", fill=z)
qplot(x, y, data=pp(100), geom="tile", fill=z)

# Missing values
p <- ggplot(pp(20)[sample(20*20, size=200),], aes(x=x,y=y,fill=z))
p + geom_tile()

# Input that works with image
image(t(volcano)[ncol(volcano):1,])
library(reshape2) # for melt
ggplot(melt(volcano), aes(x=Var1, y=Var2, fill=value)) + geom_tile()

# inspired by the image-density plots of Ken Knoblauch
cars <- ggplot(mtcars, aes(y=factor(cyl), x=mpg))
cars + geom_point()
cars + stat_bin(aes(fill=..count..), geom="tile", binwidth=3, position="identity")
cars + stat_bin(aes(fill=..density..), geom="tile", binwidth=3, position="identity")

cars + stat_density(aes(fill=..density..), geom="tile", position="identity")
cars + stat_density(aes(fill=..count..), geom="tile", position="identity")

# Another example with with unequal tile sizes
x.cell.boundary <- c(0, 4, 6, 8, 10, 14)
example <- data.frame(
        x = rep(c(2, 5, 7, 9, 12), 2),
        y = factor(rep(c(1,2), each=5)),
        z = rep(1:5, each=2),
        w = rep(diff(x.cell.boundary), 2)
)

qplot(x, y, fill=z, data=example, geom="tile")
qplot(x, y, fill=z, data=example, geom="tile", width=w)
qplot(x, y, fill=factor(z), data=example, geom="tile", width=w)

# You can manually set the colour of the tiles using
# scale_manual
col <- c("darkblue", "blue", "green", "orange", "red")
qplot(x, y, fill=col[z], data=example, geom="tile", width=w, group=1) +
        scale_fill_identity(labels=letters[1:5], breaks=col)

[Package ggplot2 version 1.0.0 Index]

### Plot

p0 <- ggplot(data=df,aes(x=Distance,y=Elevation)) +
        xlab("Distance(Km)") +
        ylab("Elevation(m)") +
        theme_classic() +
        scale_x_continuous(expand = c(0,0), breaks = seq(0,3000000,500000),labels = function(x) x/1000) +
        scale_y_continuous(expand = c(0,0), limit = c(0,6000),breaks = seq(0,6000,1000)) +
        coord_fixed(ratio = 100)


p12  <- p0 + geom_area(fill="cyan") +
        geom_line(aes(group=1),size = 0,colour="sienna2") #sienna2

p12
p10  <- p0 + geom_point(size =1, colour="red")
p10
p11  <- p0 + geom_line(aes(group=1),colour="red")
p11

p14  <- p0 + geom_step()
p14
p20  <- p11 + stat_summary(geom="ribbon", fun.ymin="min", fun.ymax="max")
p20
source("~/Dropbox/workbox/R/")

p  <- p12
ggsave(p, file="curen-wuhan6.png",dpi = 600, unit = "cm")
png("test.png")
plot(p)
dev.off()
### Control the Graphy
p <- p + geom_line(aes(fill = "Elevation"))
p
p <- p + geom_smooth()
p


library(ggplot2)
ggplot(df, aes(x = Distance)) +
        geom_ribbon(aes(ymin = 0, # change this to match your min below
                        ymax = 6000),
                    fill = "#1B9E77") + # put your altitude variable here if not using moving averages
        labs(x = "Miles",
             y = "Elevation")
+
        scale_y_continuous(limits = c(600,1200)) # change this to limits appropriate for your region





# Geometry Layers
geom_line() +
        # Smooth Zone and Curve
        geom_smooth()
# Title
# labs(title = "willing")+
# X,Y Labels
xlab(expression(paste("Temperature (",degree,"C)"))) +
        coord_cartesian(xlim=c(10.5, 23)) +
        scale_x_continuous(breaks=seq(10.5, 23, by = 2.5)) +
        ylab(expression(paste("NPP (g C ",m^-2," ",yr^-1,")"))) +
        coord_cartesian(ylim=c(600, 850)) +
        scale_y_continuous(breaks=seq(600, 850, by = 50))

# Theme with white background, grey grid, and Font
p <- p + theme_bw(base_size = 12, base_family = "Times New Roman")

# Eliminates baground, gridlines, and chart border
p <- p + theme(
        plot.background = element_blank()
        ,panel.grid.major = element_blank()
        ,panel.grid.minor = element_blank()
        ,panel.border = element_blank()
        ,panel.background = element_blank())
## Axes:
# Axes:draws x and y axis line
p <- p + theme(axis.line = element_line(color = 'black'))

# Axes:Scales of X and Y


library(lattice)
trellis.par.set(sp.theme()) # sets bpy.colors() ramp
data(meuse)
coordinates(meuse) <- ~xy
l2 = list("SpatialPolygonsRescale", layout.north.arrow(), offset = c(181300,329800),
            scale = 400)
l3 = list("SpatialPolygonsRescale", layout.scale.bar(), offset = c(180500,329800),
             	scale = 500, fill=c("transparent","black"))
l4 = list("sp.text", c(180500,329900), "0")
l5 = list("sp.text", c(181000,329900), "500 m")
spplot(meuse, c("ffreq"), sp.layout=list(l2,l3,l4,l5), col.regions= "black",
                  	pch=c(1,2,3), key.space=list(x=0.1,y=.95,corner=c(0,1)))
spplot(meuse, c("zinc", "lead"), sp.layout=list(l2,l3,l4,l5, which = 2),
          	key.space=list(x=0.1,y=.95,corner=c(0,1)))
 # plotting factors:
         meuse$f = factor(sample(letters[6:10], 155, replace=TRUE),levels=letters[1:10])
 meuse$g = factor(sample(letters[1:5], 155, replace=TRUE),levels=letters[1:10])
 spplot(meuse, c("f","g"), col.regions=bpy.colors(10))

         if (require(RColorBrewer)) {
                 	spplot(meuse, c("ffreq"), sp.layout=list(l2,l3,l4,l5),
                           		col.regions=brewer.pal(3, "Set1"))
                 }
Loading required package: RColorBrewer

         data(meuse.grid)
 gridded(meuse.grid)=~xy
 meuse.grid$g = factor(sample(letters[1:5], 3103, replace=TRUE),levels=letters[1:10])
 meuse.grid$f = factor(sample(letters[6:10], 3103, replace=TRUE),levels=letters[1:10])
 spplot(meuse.grid, c("f","g"))
 spplot(meuse.grid, c("f","g"), col.regions=bpy.colors(10))


f <- system.file("external/test.grd", package="raster")
r <- raster(f)

levelplot(r) +
        layer({
                xs <- seq(181000, 181400, by=100)
                grid.rect(x=xs, y=330500,
                          width=100, height=30,
                          gp=gpar(fill=rep(c('transparent', 'black'), 2)),
                          default.units='native')
                grid.text(x= xs - 50, y=330560, seq(0, 400, by=100),
                          gp=gpar(cex=0.5), rot=30,
                          default.units='native')
        })
levelplot(r, margin=FALSE, auto.key=FALSE, scales=list(draw=FALSE)) +
        layer({
                SpatialPolygonsRescale(layout.north.arrow(),
                                       offset = c(179000,332500),
                                       scale = 400)
        })

#' clip a data frame by a xmin, xmax
# dsubd  <- function(data, sub){
#         out  <- list() # a list of dataframe
#         for (i in 1:nrow(sub)){
#                 xmin  <- sub[i,]$xmin
#                 xmax  <- sub[i,]$xmax
#                 ymin  <- sub[i,]$ymin
#                 ymax  <- sub[i,]$ymax
#                 out[[i]]  <-  data[x >= xmin & x <= xmax & y  >= ymin & y <= ymax,]
#         }
#         return(out)
#
# }
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
rad  <- 4000
dlcc$xmin  <- round(dlcc$xlcc, -3) -rad
dlcc$xmax  <- round(dlcc$xlcc, -3) +rad
dlcc$ymin  <- round(dlcc$ylcc, -3) -rad
dlcc$ymax  <- round(dlcc$ylcc, -3) +rad
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
#dfs <- lapply(clipper.l, get)
clipper.df  <- do.call(rbind, clipper.l)
clipper.df$id  <- as.factor(substr(row.names(clipper.df),1,1))
# summary(clipper.df)
# ggplot(clipper.df,aes(x,y, fill = tCenter)) + geom_point() +
# facet_wrap(~ id)
cols = oceColorsJet(10)
col.brks  <- seq(-20, 20, 2)
col.labs  <- as.character(colbrks)
grobs  <- lapply(clipper.l, function(d) {
        ggplot(d) +
        geom_raster(aes(x,y, fill = tCenter)) +
        scale_x_continuous(labels = function(x) x/1000) +
        scale_y_continuous(labels = function(x) x/1000) +
        xlab("x (km)") +
        ylab("y (km)") +
        scale_fill_gradientn(colours = cols,
                             na.value="white",
                             breaks = col.brks,
                             labels = col.labs,
                             name = expression(~(degree*C))) +
                theme_bw(base_size = 12, base_family = "Times") +
                coord_equal() # +
                #theme(axis.title.y = element_blank())

        })

#library(gridExtra)
# tiff("clipper.tiff", h = 2000, w = 2000, res = 300)
# png("clipper.png")
# do.call(grid.arrange, c(grobs, nrow =2))

### Better
grid.newpage()
grid.draw(rbind(
        cbind(ggplotGrob(grobs[[1]]), ggplotGrob(grobs[[2]]), size="last"),
        cbind(ggplotGrob(grobs[[3]]), ggplotGrob(grobs[[4]]), size="last"),
        size = "last"))
# Extracxt the legend from p1 !!!but that is just for p1
# legend = gtable_filter(ggplot_gtable(ggplot_build(grobs[[1]])), "guide-box")
#
# grid.draw(legend)    # Make sure the legend has been extracted
# grid.newpage()
# # Arrange and draw the plot as before
# grid.arrange(arrangeGrob(grobs[[1]] + theme(legend.position="none"),
#                          grobs[[2]] + theme(legend.position="none"),
#                          grobs[[3]] + theme(legend.position="none"),
#                          grobs[[4]] + theme(legend.position="none"),
#                          nrow = 2,
#                          main = textGrob("Main Title", vjust = 1, gp = gpar(fontface = "bold", cex = 1.5)),
#                          left = textGrob("Global Y-axis Label", rot = 90, vjust = 1)),
#              legend,
#              widths=unit.c(unit(1, "npc") - legend$width, legend$width),
#              nrow=1)
#dev.off()
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

source("./tirSettings.R")
worklist  <- paste0(hkdMinCloud$ID,".tgz")
rs.untar(worklist, fromdir = dir.tar, todir = dir.tif)
source("./tirSettings.R")
worklist  <- paste0(hkdMinCloud$ID,".tgz")
rs.todown(worklist, dir.tar, "todown.txt")
source("./tirSettings.R")
setwd(dir.tif)  ## very important tips for use rLandsat8
## files  <- sapply(file.path(dir.tif,list.files(dir.tif)), tools::file_path_as_absolute)
## l8.lst  <- lapply(basename(files), ReadLandsat8)
l8.lst   <- lapply(dir(dir.tif), ReadLandsat8)
bandnames <-c("aerosol", "blue", "green", "red",
              "nir", "swir1", "swir2",
              "panchromatic",
              "cirrus",
              "tirs1", "tirs2")
for (i in l8.lst) {
        sceneName  <- i$metadata$landsat_scene_id
        if (!file.exists(file.path(dir.toaRad, sceneName))) {
                dir.create(file.path(dir.toaRad, sceneName), recursive = T)
        }
        for(j in bandnames){
                idx <- seq_along(bandnames)[sapply(bandnames, function(x) j %in% x)] # a number
                bandidx <- paste0("file_name_band_", idx)
                bandName <-  sapply(i, "[[", bandidx)[[1]]
                pngName <- paste0(bandName, ".png")
                #fileName <- paste0(tools::file_path_sans_ext(bandName), "_TOARad.tif")
                Rad.rst  <- ToTOARadiance(i, j)
                writeRaster(Rad.rst, filename = file.path(dir.toaRad, sceneName,
                                     bandName), overwrite = T)
                png(filename = file.path(dir.toaRad, sceneName, pngName))
                par(family = "times")
                plot(Rad.rst)
                title(main = paste("Radiance at TOA of Band ", idx))
                require("grid")
                grid.text(expression(paste("[", W*sr^-1*m^-2*mu*m^-1,"]")), x=unit(0.975, "npc"), y=unit(0.50, "npc"), rot=-90)
                dev.off()
                raster::removeTmpFiles(h = 1) ## Improtant tips for save hardisk
        }
}
source("./tirSettings.R")
setwd(dir.tif)  ## very important tips for use rLandsat8
l8.lst   <- lapply(dir(dir.tif), ReadLandsat8)
bandnames <-c("aerosol", "blue", "green", "red",
              "nir", "swir1", "swir2",
              "panchromatic",
              "cirrus")
for (i in l8.lst) {
        sceneName  <- i$metadata$landsat_scene_id
        if (!file.exists(file.path(dir.toaRef, sceneName))) {
                dir.create(file.path(dir.toaRef, sceneName), recursive = T)
        }
        for(j in bandnames){
                idx <- seq_along(bandnames)[sapply(bandnames, function(x) j %in% x)] # a number
                bandidx <- paste0("file_name_band_", idx)
                bandName <-  sapply(i, "[[", bandidx)[[1]]
                # fileName <- paste0(tools::file_path_sans_ext(bandName), "_TOARef.tif")
                Ref.rst  <- ToTOAReflectance(i, j)
                writeRaster(Ref.rst, filename = file.path(dir.toaRef, sceneName, bandName), overwrite = T)
                raster::removeTmpFiles(h = 0.5) ## Improtant tips for save hardisk
        }
}
source("./tirSettings.R")
setwd(dir.tif)  ## very important tips for use rLandsat8
l8.lst   <- lapply(dir(dir.tif), ReadLandsat8)
bandnames <-c("aerosol", "blue", "green", "red",
              "nir", "swir1", "swir2",
              "panchromatic",
              "cirrus")
for (i in l8.lst) {
        sceneName  <- i$metadata$landsat_scene_id
        if (!file.exists(file.path(dir.toaRefSun, sceneName))) {
                dir.create(file.path(dir.toaRefSun, sceneName), recursive = T)
        }
        for(j in bandnames){
                idx <- seq_along(bandnames)[sapply(bandnames, function(x) j %in% x)] # a number
                bandidx <- paste0("file_name_band_", idx)
                bandName <-  sapply(i, "[[", bandidx)[[1]]
                # fileName <- paste0(tools::file_path_sans_ext(bandName), "_TOARefSun.tif")
                Ref.rst  <- ToTOAReflectance(i, j, is.suncorrected = TRUE)
                writeRaster(Ref.rst, filename = file.path(dir.toaRefSun, sceneName, bandName), overwrite = T)
                raster::removeTmpFiles(h = 0.5) ## Improtant tips for save hardisk
        }
}
source("./tirSettings.R")
setwd(dir.tif)  ## very important tips for use rLandsat8
l8.lst   <- lapply(dir(dir.tif), ReadLandsat8)
bandnames <-c("tirs1", "tirs2")
for (i in l8.lst) {
        sceneName  <- i$metadata$landsat_scene_id
        if (!file.exists(file.path(dir.toaBT, sceneName))) {
                dir.create(file.path(dir.toaBT, sceneName), recursive = T)
        }
        for(j in bandnames){
                idx <- seq_along(bandnames)[sapply(bandnames, function(x) j %in% x)] + 9 # a number
                bandidx <- paste0("file_name_band_", idx)
                bandName <-  sapply(i, "[[", bandidx)[[1]]
                #fileName <- paste0(tools::file_path_sans_ext(bandName), "_TOABT.tif")
                BT.rst  <- ToAtSatelliteBrightnessTemperature(i, j)
                writeRaster(BT.rst, filename = file.path(dir.toaBT, sceneName, bandName), overwrite = T)
                raster::removeTmpFiles(h = 1) ## Improtant tips for save hardisk
        }
}
source("./tirSettings.R")
setwd(dir.tif)  ## very important tips for use rLandsat8
l8.lst   <- lapply(dir(dir.tif), ReadLandsat8)
bandnames <-c("tirs1", "tirs2")
sceneList <- list.files(dir.toaTbk, full.names = TRUE)
for (i in sceneList) {
        bandList <- list.files(sceneList, full.names = TRUE)
        emiName <- paste0(basename(i), ".tif")
        pngName <- paste0(emiName,".png")
        Tb10 <- raster::raster(bandList[1])
        Tb11 <- raster::raster(bandList[2])
        TbS  <- raster::stack(Tb10, Tb11)
        a <- 1.438*10^-2
        L10 <- 10.9
        L11 <- 12.0
        TbE  <- exp((a*(Tb10 - Tb11))/(Tb10*Tb11*(L10-L11)))
        Ts10  <- Tb10/(1 + (L10*Tb10/a)*log(TbE)
        writeRaster(TbE, filename = file.path(dir.toaTe,emiName), overwrite = T)
        png(file.path(dir.toaTe,pngName))
        plot(TbE)
        dev.off()
        raster::removeTmpFiles(h = 1) ## Improtant tips for save hardisk
        }
}
source("./tirSettings.R"
dataDir  <- dir.toaTbKlcc
setwd(dataDir)
r = raster("LC81050292014153LGN00_B10.tif")
png("test.png")
plot(r)
dev.off()
rs.tif2png()
#' Reproject Raster
#'
#' @author Bingwei Tian
#' @param tif a list of tif files
#' @date 926
library(sp)
library(rgdal)
library(raster)

source("~/SparkleShare/Rprofile/R/sourceDir.R")
sourceDir("~/SparkleShare/Rprofile/R/")
dir.tmp <- "~/Share500sda/Landsat8/raster_tmp"
rasterOptions(tmpdir = dir.tmp)
dir.toaTbK  <- "~/Share500sda/Landsat8/at1_TOA/toaTbK/"
dir.toaTbKlcc  <-  "~/Share500sda/Landsat8/at1_TOA/toaTbKlcc"

toCRS  <- sp::CRS(lccWgs84)
if (!file.exists(dir.toaTbKlcc)){
        dir.create(dir.toaTbKlcc)
}
tif <- list.files(path= dir.toaTbK ,
                  pattern= ".tif$",
                  all.files=TRUE,
                  full.names=TRUE,
                  recursive=TRUE,
                  ignore.case=TRUE)
r.rst  <- lapply(tif, raster)
for (i in r.rst) {
        outName  <- paste0(names(i), ".tif")
        projectRaster(i, crs = toCRS,  method = "ngb",
                      filename =  file.path(dir.toaTbKlcc, outName))
        raster::removeTmpFiles(h = 1) ## Improtant tips for save hardisk
}
test = projectRaster(r.rst[[1]], crs = lccWgs84, method = "ngb")
proj4string(r.rst[[1]])
#' Scale Raster
#'
#' @author Bingwei Tian
#' @param tif a list of tif files
#' @date 926
source("./tirSettings.R")
if (!file.exists(dir.toaTbKlccScale)){
        dir.create(dir.toaTbKlccScale)
}
tif <- list.files(path= dir.toaTbKlcc,
                  pattern= ".tif$",
                  all.files=TRUE,
                  full.names=TRUE,
                  recursive=TRUE,
                  ignore.case=TRUE)
r.rst  <- lapply(tif, raster)
hkdmaskb  <- readRDS("~/SparkleShare/TIR/hkdmskb_grdi2d1h.Rds")
r.rstm  <- lapply(r.rst, function(x) mask(x, hkdmaskb))
for (i in r.rstm) {
        outName  <- paste0(names(i), ".tif")
        #projectRaster(from = i, crs = toCRS,  method = "ngb",
        zscore  <- raster::scale(i)
        writeRaster(zscore,
                    filename = file.path(dir.toaTbKlccScale, outName),
                    overwrite = T)
        raster::removeTmpFiles(h = 1) ## Improtant tips for save hardisk
}
#test = projectRaster(r.rst[[1]], crs = toCRS, method = "ngb")

library(oce)
install.packages("oce")
oceColorsJet
colorRampPalette(c("#00007F", "blue", "#007FFF", "cyan",
                   "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"))(255)
#' Mosaic Rasters
#'
#' @author Bingwei Tian
#' @param tif a list of tif files
#' @date 926
source("./tirSettings.R")
if (!file.exists(dir.toaTbKlccScaleMos)){
        dir.create(dir.toaTbKlccScaleMos)
}
tif10 <- list.files(path= dir.toaTbKlccScale,
                  pattern= "B10.tif$",
                  all.files=TRUE,
                  full.names=TRUE,
                  recursive=TRUE,
                  ignore.case=TRUE)
r10.rst  <- lapply(tif10, raster)
# B10 <- mosaic(r10.rst, fun = mean,
#         filename = file.path(dir.toaTbKlccScaleMos, "B10Mosaic.tif")
# )
## Mosaic a list of raster*
r10.rst$fun <- mean
mos10 <- do.call(mosaic, r10.rst)
writeRaster(mos10,
            filename = file.path(dir.toaTbKlccScaleMos, "B10Mosaic.tif"),
            overwrite = TRUE)
jpeg(filename = file.path(dir.toaTbKlccScaleMos, "B10Mosaic.jpeg"))
plot(mos10)
dev.off()

tif11 <- list.files(path= dir.toaTbKlccScale,
                    pattern= "B11.tif$",
                    all.files=TRUE,
                    full.names=TRUE,
                    recursive=TRUE,
                    ignore.case=TRUE)
###Core Code
r11.rst  <- lapply(tif11, raster)
r11.rst$fun <- mean
mos11 <- do.call(mosaic, r11.rst)
writeRaster(mos11,
            filename = file.path(dir.toaTbKlccScaleMos, "B11Mosaic.tif"),
            overwrite = T)
jpeg(filename = file.path(dir.toaTbKlccScaleMos, "B11Mosaic.jpeg"))
plot(mos11)
dev.off()
#' @title Calculate LST using Arits and Carnathan 1982
#' @param Ts = Tb/(1+(L*Tb/a)lne); a = hc/j
#' @param h is Planck's constant (6.626*10^-34 Js)
#' @param c is the velocity of light (2.998*10^8 m/s)
#' @param j is Bolzman constant (1.38*10^-23 J/K)
#' @param a is h*c/j (1.439*10^-2 m/K)
#' @param e is a Raster* object with Emmissvity Value caluateed by NDVI or LULC
source("./tirSettings.R")
if (!file.exists(dir.sufTsKlcc)){
        dir.create(dir.sufTsKlcc)
}
L10  = 10.9*10^-6
L11  = 12*10^-6
h = 6.626*10^-34
c = 2.998*10^8
j = 1.38*10^-23
p = h*c/j
e  <- raster::raster("~/Share500sda/Landsat8/at9_Database/LULC/hkdEmissivity.tif")
tif10 <- list.files(path= dir.toaTbKlcc,
                            pattern= "B10.tif$",
                            all.files=TRUE,
                            full.names=TRUE,
                            recursive=TRUE,
                            ignore.case=TRUE)
r10.rst  <- lapply(tif10, raster)
hkdmaskb  <- readRDS("~/SparkleShare/TIR/hkdmskb_grdi2d1h.Rds")
r10.msk  <- lapply(r10.rst, function(x) mask(x, hkdmaskb))
for (i in r10.msk) {
        outName  <- paste0(names(i), ".tif")
        #projectRaster(from = i, crs = toCRS,  method = "ngb",
        Tsk  <- i/(1+(L10*i/p)*log(e))
        writeRaster(Tsk,
                    filename = file.path(dir.sufTsKlcc, outName),
                    overwrite = T)
        raster::removeTmpFiles(h = 1) ## Improtant tips for save hardisk
}
tif11 <- list.files(path= dir.toaTbKlcc,
                    pattern= "B11.tif$",
                    all.files=TRUE,
                    full.names=TRUE,
                    recursive=TRUE,
                    ignore.case=TRUE)
r11.rst  <- lapply(tif11, raster)
hkdmaskb  <- readRDS("~/SparkleShare/TIR/hkdmskb_grdi2d1h.Rds")
r11.msk  <- lapply(r11.rst, function(x) mask(x, hkdmaskb))
for (i in r11.msk) {
        outName  <- paste0(names(i), ".tif")
        #projectRaster(from = i, crs = toCRS,  method = "ngb",
        Tsk  <- i/(1+(L11*i/p)*log(e))
        writeRaster(Tsk,
                    filename = file.path(dir.sufTsKlcc, outName),
                    overwrite = T)
        raster::removeTmpFiles(h = 1) ## Improtant tips for save hardisk
}

source("~/SparkleShare/TIR/demo/tirSettings.R")
setwd(dir.toaTbKlccCenterMos)
mos  <- raster("L8B10CenterMos.tif")
volA  <- readRDS("~/Share500sda//2data/dataProduct/hkd/hkdVol20a_140812_175023.Rds")
volAlcc  <- spTransform(volA, CRS(lccWgs84))
proj4string(volA)
plot(volAlcc, pch = 2, size = 6,add =T)
plot(mos, col = oceColorsJet(255))
plot(mos, col = bpy.colors(255))
plot(volAlcc, pch = 2, size = 6,add =T)
levelplot(mos)
levelplot(mos,maxpixels=1e6, par.settings =  BuRdTheme)
source("~/SparkleShare/TIR/demo/tirSettings.R")
# tif <- list.files(path= dir.toaTbK ,
#                   pattern= "B10.tif$",
#                   all.files=TRUE,
#                   full.names=TRUE,
#                   recursive=TRUE,
#                   ignore.case=TRUE)
# r.rst  <- lapply(tif, raster)
# par()
# par(mfcol =  c(4,4))
# lapply(r.rst, plot)
#
# lapply(r.rst, function(x) plot(x, col = terrain.colors(225)))


## lOAD Additional data
hkdshp  <- "~/Share500sda/2data/dataRaw/japan_ver71/HokkaidoUnion_lccWgs84.shp"
hkdshp  <- readShapePoly(hkdshp)
proj4string(hkdshp) <- CRS(lccWgs84)
hkdmaskb  <- readRDS("~/SparkleShare/TIR/hkdmskb_grdi2d1h.Rds")
### Raster Stacked
tif10 <- list.files(path= dir.toaTbKlcc ,
                  pattern= "B10.tif$",
                  all.files=TRUE,
                  full.names=TRUE,
                  recursive=TRUE,
                  ignore.case=TRUE)
r.rst  <- lapply(tif10, raster)
r.stack  <- stack(r.rst)

r.mask  <- mask(r.stack, hkdmaskb)
r.merge  <- merge(r.mask)
plot(r.mask)
plot(r.merge, col = bpy.colors(255))
levelplot(r.merge, par.settings =  BuRdTheme)
## Center values and Merge
r.center  <- scale(r.mask,center=TRUE, scale=FALSE)
r.centerMerge  <- merge(r.center)
plot(r.centerMerge, col = bpy.colors(255))
plot(r.centerMerge, maxpixels= 1e6, col = oceColorsJet(255))
levelplot(r.centerMerge,maxpixels=1e6, par.settings =  BuRdTheme)

## Center valuse and Mosaic
r.center$fun <- mean
r.centerMos10 <- do.call(mosaic, r.center)
writeRaster(r.centerMos10, "L8B10CenterMos.tif")

# levelplot(rprob, contour = TRUE, margin = FALSE, at = miat)
# levelplot(r.stack, col = heat.colors(255), zlim = c(290, 320))
source("~/SparkleShare/TIR/demo/tirSettings.R")
setwd(dir.toaTbKlccCenterMos)
setwd("~/toaTbKlccCenterMos/")
mos  <- raster("L8B10CenterMos.tif")
# cols  <-  bpy.colors(8)
# cols = oceColorsJet(255)
# brks  <- c(-20, -15,-10,-5, seq(0,20,4))
# brks
# plot(mos, maxpixels=1e6, col=cols, lab.breaks=brks)
# cellStats(mos,min)
# #levelplot(mos)
# xlims  <- c(1273000,1283000)
# ylims  <- c(1433000,1443000)
# plot(mos, maxpixels=1e6, col=cols, xlim = xlims, ylim = ylims, lab.breaks=brks, xlab = "Easting", ylab = "Northing")
#p1  <- gplot(mos, maxpixels=100000) + geom_tile(aes(fill = value))
# p11  <- gplot(mos, maxpixels=1e6) + geom_tile(aes(fill = value))
# p1
#ncell(mos)
# p11  <- gplot(mos, maxpixels=24539100) + geom_raster(aes(fill = value))
# p11
#mos.p  <- rasterToPoints(mos)
#mos.df  <- as.data.frame(mos.p)
mos.spdf  <- rasterToPoints(mos, spatial=TRUE)
mos.df  <- as.data.frame(mos.spdf)
names(mos.df)  <- c("x", "y", "tc")
#names(mos.df)  <- c("x", "y", "t")
p12  <- ggplot(mos.df, aes(x,y, fill = t)) + geom_raster()
#p12
#p13  <- ggplot(mos.df, aes(x,y, fill = t)) + geom_point()
# p13
# p14  <- ggplot(mos.df, aes(x,y, fill = t)) + geom_jitter()
# p14
# gc()
p2  <- p12 + scale_x_continuous(label = function(x) x/1000) +
     scale_y_continuous(label = function(x) x/1000) +
     xlab("Easting (km)") +
     ylab("Northing (km)")
#p2
#cols  <-  bpy.colors(8)
cols = oceColorsJet(10)
brks  <- c(-20, -15,-10,-5, 0, 5, 10, 15, 20)
p3  <- p2 + scale_fill_gradientn(colours = cols,
                                 na.value="white",
                          breaks = brks,
                          name = expression(Temperature~(degree*C)))
#p3

#ge.ggsave(p3
### North Arror and scale bar

library(ggplot2)
library(grid)

# north  <- geom_segment(mapping = aes(x=x,y=y, xend=x+dx, yend = y+dy),
#                         arrow = arrow(),
#                         size = 3,
#                         color = "blue") +
#         geom_point(data = north, mapping = aes(x,y),size = 5, shape =21, fill = "white") +
#         geom_text(data = north, x = x+dx, y = y+dy/2, label = "N", size = 12 )
north  <- data.frame(rbind(c(1600000,1400000,0,60000),c(1550000,1400000,100000,0)))
names(north)  <- c("x", "y", "dx", "dy")
p4  <- p3 +

        geom_segment(data = north[1,], aes(x=x,y=y, xend=x+dx, yend = y+dy),
                     arrow = arrow(angle =25),
                     size = 1,
                     color = "black"
                     ) +
        geom_segment(data = north[2,], aes(x=x,y=y, xend=x+dx, yend = y+dy),
                   arrow = arrow(angle =90, ends = "both", length = unit(0.1, "cm")),
                   size = 1
                   ) +
        geom_point(data = north[1,],
                   mapping = aes(x,y),
                   size = 3,
                   shape =21, fill = "white"
        ) +
        geom_text(x = north[1,]$x, y = north[1,]$y+north[1,]$dy/2,
                   label = "N"
                   #size =
                   ) +
        geom_text(x = north[1,]$x+north[1,]$dx/2, y = north[1,]$y -north[1,]$dy/4,
                  label = "100 km"
                  )
sourceDir("~/SparkleShare/geothermaR/R")
# p4
# ge.ggsave(p4)
# jp1  <- raster::getData('GADM', country='JPN', level=1, path = "~/Dropbox/2data//dataRaw/gadm2")
# plot(jp1)
# hkd  <- ge.LargestPolys(jp1, Polygon =T)
# plot(hkd)
# volA  <- readRDS("~/Dropbox/2data/dataProduct/jpVolcanoes/jpVol110_140812_174525.Rds")
# volQ  <- readRDS("~/Dropbox/2data/dataProduct/jpVolcanoes/jpVol455_140812_172148.Rds")
# proj4string(volA)  <- proj4string(hkd)
# proj4string(volQ)  <- proj4string(hkd)
# volQhkd <- volQ[hkd,]
# volAhkd <- volA[hkd,]
# volQhkdlcc  <- spTransform(volQhkd, CRS(lccWgs84))
# volAhkdlcc  <- spTransform(volAhkd, CRS(lccWgs84))
# volQhkdlcc@coords

# plot(volAhkd)
# p5  <- p4 + geom_point(data = as.data.frame(volQhkdlcc@coords),
#                 aes(as.numeric(lon), as.numeric(lat),
#                 color="blue"), shape = 2, alpha = 0.7
#                 ) +
#      geom_point(data = as.data.frame(volAhkdlcc@coords),
#                 aes(as.numeric(lon), as.numeric(lat),
#                 color="red"),  shape = 2, size = 3
#                 ) +
#         scale_color_manual(name =  "Volcanoes", values = c("blue","red"), labels = c("Quaternary Volcanoes","Active Volcanoes"))
# #ge.ggsave(p5)

### focused on rect
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
# ggplot() +
#         geom_rect(data = dlcc,
#                   aes(NULL, NULL, xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = NULL, color = NULL), alpha =0.1, color = "red")

p6  <- p4 + geom_rect(data = dlcc,
               aes(NULL, NULL, xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = NULL, color = NULL), alpha =0.1, color = "red")
#
# p3 +  coord_cartesian(xlim = c(dlcc[1,]$xmin, dlcc[1,]$xmax),                          ylim = c(dlcc[1,]$ymin, dlcc[1,]$ymax))
# p3)
p7  <- p6 + theme_bw(base_size = 12, base_family = "Times") + coord_equal()
#ggsave("p71009.pdf")
ge.ggsave(p7)

# round(dlcc)
# names(d)  <- c("lat","lon")
#
#
# dms2d <- function(d,m=0,s=0){
#     d1  <- d
#     d2  <- m/60
#     d3  <- s/3600
#     return(d1+d2+d3)
# }
#
#
ps1  <- p7 +   coord_cartesian(xlim = c(dlcc[1,]$xmin, dlcc[1,]$xmax),
                               ylim = c(dlcc[1,]$ymin, dlcc[1,]$ymax))
ps2  <- p7 +   coord_cartesian(xlim = c(dlcc[2,]$xmin, dlcc[2,]$xmax),
                               ylim = c(dlcc[2,]$ymin, dlcc[2,]$ymax))
ps3  <- p7 +   coord_cartesian(xlim = c(dlcc[3,]$xmin, dlcc[3,]$xmax),
                               ylim = c(dlcc[3,]$ymin, dlcc[3,]$ymax))
ps4  <- p7 +   coord_cartesian(xlim = c(dlcc[4,]$xmin, dlcc[4,]$xmax),
                               ylim = c(dlcc[4,]$ymin, dlcc[4,]$ymax))
library(gridExtra)
tiff("ps11.tif")
grid.arrange(ps1, ps2, ps3, ps4, ncol=2)
dev.off()
pdf("ps11.pdf")
grid.arrange(ps1, ps2, ps3, ps4, ncol=2)
dev.off()
source("~/SparkleShare/TIR/demo/tirSettings.R")
setwd(dir.toaTbKlccCenterMos)
setwd("~/toaTbKlccCenterMos/")
mos  <- raster("L8B10CenterMos.tif")
proj4string(mos)
## Decide the point
point1  <- c(1200000, 1500000)
point2  <- c(1600000, 1700000)
## Connet to Line
pnts  <- rbind(point1, point2)
line <- SpatialLines(list(Lines(list(Line(pnts)), "1")))
proj4string(line)  <- CRS(lccWgs84)

volA  <- readRDS("~/Share500sda//2data/dataProduct/hkd/hkdVol20a_140812_175023.Rds")
volA  <- readRDS("~/Dropbox/2data/dataProduct/hkd/hkdVol20a_140812_175023.Rds")
volAlcc  <- spTransform(volA, CRS(lccWgs84))

plot(mos)
plot(line, col = "blue", add =T)
plot(volAlcc, pch = 2, size = 6,add =T)
profile  <- extract(mos, )

ids <- init(mos, v='cell')

## Decide the point
point1  <- c(1200000, 1500000)
point2  <- c(1540000, 1680000)
## Connet to Line
pnts  <- rbind(point1, point2)
line <- SpatialLines(list(Lines(list(Line(pnts)), "1")))
proj4string(line)  <- CRS(lccWgs84)
##only value
pf1  <- extract(mos, line)
##
# create raster index raster 1~ncell = cellnumbers=TRUE in extract
idx <- init(mos, v='cell')
plot(idx)
# extract raster idx along line
# cells <- extract(idx, line)
# range(cells)

profile  <-  extract(x=mos, y=line, along=TRUE, cellnumbers=TRUE)[[1]]
idv_df  <- as.data.frame(profile)
cells  <- idv_df[,1]
xy = lapply(cells, function(x) xyFromCell(mos, x))
xy_df  <- as.data.frame(do.call(rbind, xy))
idvxy  <- cbind(idv_df,xy_df)
pairs  <- length(idvxy$x) - 1
dx  <- idvxy$x[2:(pairs+1)] - idvxy$x[1:pairs]
dy  <- idvxy$y[2:(pairs+1)] - idvxy$y[1:pairs]
dx^2
dy^2
dist  <- sqrt(dx^2+dy^2)
cumdist  <- cumsum(dist[1:pairs])
cumdist
### Plot Profile
plot(cumdist, idvxy$L8B10CenterMos[1:pairs], type = "l")
### Fractal dimension
spec  <- spectcrum(idvxy$L8B10CenterMos[1:pairs])
fmodel  <- log(spec$spec) ~ log(1/spec$freq)
lmodel  <- lm(fmodel)
slopes  <- coef(lmodel)[2]
fractal  <- (5 - slopes) /2
plot(fmodel, main = paste0("Fractal Dimension", round(fractal,3)), xlab = "Log(1/freq)", ylab = "Log(Power)")
abline(lmodel)
source("~/SparkleShare/TIR/demo/tirSettings.R")
setwd(dir.AG100B)
### Hokkaido Area
bins.l <- list.files(path=dir.AG100B,
                   pattern="bin$",
                   all.files=TRUE,
                   #full.names=TRUE,
                   recursive=TRUE,
                   ignore.case=TRUE)
bins.df <- as.data.frame(cbind(bins.l, do.call(rbind, strsplit(bins.l, "[.]"))), stringsAsFactors = FALSE)
colnames(bins.df)  <- c("ID", "Product", "Ver", "Lat", "Lon", "Num", "Format")
bins.df[,4]  <- as.numeric(bins.df[,4])
bins.df[,5]  <- as.numeric(bins.df[,5])
hkd  <- subset(bins.df, Lon >= 139 & Lon <=146 & Lat <=46 )
#plot(hkd$Lon, hkd$Lat)
hkd.l  <- hkd[[1]]
hkdAG100B.l   <- readAG100B(hkd.l)
hkdAG100B.df  <- do.call(rbind, hkdAG100B.l)
saveRDS(hkdAG100B.df, file = "hkdAG100B_df.Rds")
source("~/SparkleShare/TIR/demo/tirSettings.R")
mapRaster  <- levelplot(LSTCenter, par.settings = BuRdTheme, FUN.margin=median, axis.margin = TRUE,
                        at = seq(-40, 40,2),
                        xlab='Easting (km)', ylab='Northing (km)',
                        yscale.components=function(...){
                                yc <- yscale.components.default(...)
                                yc$left$labels$labels <- yc$left$labels$at/1000 ## convert to strings as pct
                                return(yc)
                        },
                        xscale.components=function(...){
                                xc <- xscale.components.default(...)
                                xc$bottom$labels$labels <- xc$bottom$labels$at/1000 ## convert to strings as pct
                                return(xc)
                        }
)
volA  <- readRDS("~/Dropbox/2data/dataProduct/hkd//hkdVol20a_140812_175023.Rds")
volQ  <- readRDS("~/Dropbox/2data/dataProduct/hkd//hkdVol_140530_113923.Rds")
plot(volA)
plot(volQ)
mapVector  <-
        + spplot()
layer({SpatialPolygonsRescale(layout.north.arrow(),
                              offset = c(1800000,1600000),
                              scale = 400)
})

levelplot(LSTCenter, at=seq(min(LSTCenter[], na.rm=T), max(LSTCenter[], na.rm=T), len=100),
           col.regions=colorRampPalette(c('#2c7bb6', '#abd9e9', '#ffffbf',
'#fdae61', '#d7191c')))
summary(LSTmask)

show.settings()
source("~/SparkleShare/TIR/demo/tirSettings.R")
### Import df to spdf
hkdAG100df  <- readRDS("~/Share500sda/AG100B/hkdAG100B.Rds")
head(hkdAG100df)
hkdAG100spdf  <- hkdAG100df

coordinates(hkdAG100spdf)  <- ~Lon+Lat
proj4string(hkdAG100spdf)  <- wgs84GRS
setwd(dir.AG100B)
getwd()
### Subset spdf
hkdBoudary  <- readRDS
sub  <- hkdAG100spdf[hkdBoudary]
#phd.saveshp.geo(hkdAG100spdf)  ## This will take a lot of time
####sgdf Crop spdf using Vector
###
hkdAG100sgdfLSTm  <- vect2rast(hkdAG100spdf, fname = "LSTm", cell.size = 100)
summary(hkdAG100sgdfLSTm)
##mask
hkdAG100sgdfLSTmR  <- raster(hkdAG100sgdfLSTm)
hkdmaskb  <- readRDS("~/SparkleShare/TIR/hkdmskb_grdi2d1h.Rds")
proj4string(hkdmaskb)
LSTcrop  <- crop(hkdAG100sgdfLSTmR, hkdmaskb, filename = "hkdAG100sgdfLSTm.tif", overwrite=TRUE)

# plot(LSTcrop)
# proj4string(hkdAG100sgdfLSTmR)
# proj4string(hkdmaskb)
# extent(LSTcrop)
# extent(hkdmaskb)
LSTrsp  <- resample(LSTcrop, hkdmaskb)
LSTrsp2  <- resample(hkdAG100sgdfLSTmR, hkdmaskb)
compareRaster(LSTrsp, LSTrsp2)
extent(LSTrsp2)
extent(hkdmaskb)
plot(LSTrsp)
summary(LSTrsp)
LSTmask  <- mask(LSTrsp2, hkdmaskb, filename = "hkdAG100LSTmask.tif", overwrite=TRUE)
plot(LSTmask, zlim =c(252, 320))

levelplot(LSTmask, par.settings = BuRdTheme)

levelplot(LSTmask, par.settings = BuRdTheme, at  = seq(252,320, 2))
plotKML(LSTmask)
LSTCenter  <- scale(LSTmask, center = TRUE, scale = FALSE)
summary(LSTCenter)


#### rasterlize
plot(hkdmaskb)
proj4string(hkdmaskb)
extent(hkdmaskb)
hkdAG100rb  <- rasterize(hkdAG100spdf, hkdmaskb)

levelplot(hkdAG100rb)
plot(hkdAG100rb)
class(hkdAG100r)
saveRDS(hkdAG100r, file = "hkdAG100Br.Rds")
hkdAG100rb[[1]]
#' @usage source("./tirSettings.R")
#' @author Bingwei Tian <bwtian@gmail.com>
#'
### Load Library
library(sp)
library(rgdal)
library(gdalUtils)
library(maptools)
library(raster)
library(rasterVis)
library(plotKML)
library(gstat)
library(plyr)
library(rgeos)
library(ggplot2)
library(ggmap)

library(lattice)
library(latticeExtra)
#library(oce)
### Source Functions
source("~/SparkleShare/Rprofile/R/RprofilesAuto/sourceDir.R")
sourceDir("~/SparkleShare/Rprofile/R/")
sourceDir("~/SparkleShare/TIR/R/")
sourceDir("~/SparkleShare/phd/R/")
sourceDir("~/SparkleShare/rLandsat8/src/main/R/rLandsat8/R")
sourceDir("~/SparkleShare/geothermaR/R/")
###  Options
if(.Platform$OS.type == "windows"){
        windowsFonts(Times=windowsFont("TT Times New Roman"))
        windowsFonts(times=windowsFont("TT Times New Roman"))
}

driver     <- "~/Share500sda/" # Linux and Windows Symbolink
dir.tmp    <- file.path(driver, "raster_tmp")
rasterOptions(tmpdir = dir.tmp)
raster::removeTmpFiles(h = 24)
gc()

### Colors
rainbow1  <- rainbow(n = 255, start = 2/6)
oceColorsJet  <- function (n)
{
        if (missing(n) || n <= 0)
                colorRampPalette(c("#00007F", "blue", "#007FFF", "cyan",
                                   "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"))
        else {
                colorRampPalette(c("#00007F", "blue", "#007FFF", "cyan",
                                   "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"))(n)
        }
}
### Define Drivers
#dir.tar  <- file.path(driver, "Landsat8/L1T")
dir.sat  <- file.path(driver, "Landsat8")
dir.tif <- file.path(dir.sat, "at0_Sensor")
dir.toa <- file.path(dir.sat, "at1_TOA")
dir.toaRad <- file.path(dir.toa, "toaRad")
dir.toaTbKlccScale  <- file.path(dir.toa,"toaTbKlccScale")
dir.toaTbKlccScaleMos <- file.path(dir.toa,"toaTbKlccScaleMos")
dir.toaRef  <- file.path(dir.toa,"toaRef")
dir.toaRefSun  <- file.path(dir.toa,"toaRefSun")
dir.toaTbK <- file.path(dir.toa, "toaTbK")
dir.toaTbC <- file.path(dir.toa, "toaTbC")
dir.toaTbKE <- file.path(dir.toa, "toaTbKE")
dir.toaEmi  <-   file.path(dir.toa, "toaEmi")
dir.toaTbKlcc  <-  file.path(dir.toa,"toaTbKlcc")
dir.toaTbKlccScale  <-  file.path(dir.toa,"toaTbKlccScale")
dir.toaTbKlccScaleMos <-  file.path(dir.toa,"toaTbKlccScaleMos")
dir.toaTbKlccCenterMos <-  file.path(dir.toa,"toaTbKlccCenterMos")
dir.surface  <- file.path(dir.sat, "at2_Surface")
dir.sufTsKlcc  <-  file.path(dir.surface, "sufTsK")
dir.database  <- file.path(driver, "at9_Database")
dir.lulc  <- file.path(dir.database, "LULC")
dir.AG100B  <- "~/Share500sda/AG100B/"
### Files
#hkdmaskb  <- readRDS("~/SparkleShare/TIR/hkdmskb_grdi2d1h.Rds")

xlimJP <- c(128.5, 146.5)
ylimJP <- c(30.2, 45.8)
certerJp <- c(137.5, 38)
phd.rainbow <- grDevices::colorRampPalette(c("purple","blue","cyan","green","yellow", "orange","red"))
# Coordinate Reference Systems of Japan
## Geographic Coordinate Reference Systems
#tokyoGRS <- "+init=epsg:4301"
tokyoGRS  <- "+proj=longlat +ellps=bessel +no_defs"
#wgs84GRS <- "+init=epsg:4326"
wgs84GRS  <-"+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"
#jgd2000GRS <- "+init=epsg:4612"
jgd2000GRS  <- "+proj=longlat +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +no_defs"

## Projected Coordinate Reference Systems
lccBessel <- "+proj=lcc +lat_1=32.8 +lat_2=43.2 +lat_0=38 +lon_0=137.5 +x_0=1000000 +y_0#=1000000 +ellps=bessel +towgs84=-146.414,507.337,680.507,0,0,0,0 +units=m +no_defs"
## change the datum to WGS84 20140508
lccWgs84 <- "+proj=lcc +lat_1=32.8 +lat_2=43.2 +lat_0=38 +lon_0=137.5 +x_0=1000000 +y_0=1000000 +datum=WGS84 +units=m +no_defs"
#  +ellps=WGS84 +towgs84=0,0,0
codeDir <- "~/Dropbox/1code"
dataDir <- "~/Dropbox/2data"
dataData <- "~/Dropbox/2data/data"
dataRaw <- "~/Dropbox/2data/dataRaw"
dataPro <- "~/Dropbox/2data/dataProduct"
figsDir <- "~/Dropbox/3figs"
