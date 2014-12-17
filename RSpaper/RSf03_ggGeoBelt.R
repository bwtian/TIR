source("~/SparkleShare/Rprofile/R/Rsettings/phdRsettings.R")
setwd("~/D/tian/greenTuff/")
### Make study Boundary

jp1.SPDF  <- getData('GADM', country='JPN', level=1, path = "~/Dropbox/2data//dataRaw/gadm2")
hkdLand.SPDF  <- ge.LargestPolys(jp1.SPDF, Polygon=T)
plot(hkdLand.SPDF)

### Plot GreenTuff
GreenTuff  <- "GreenTuff/doc.kml"
ogrListLayers(GreenTuff)
greenTuff.sldf  <-readOGR(GreenTuff,  "GreenTuff")
greenTuff.sldf
proj4string(greenTuff.sldf)  <- CRS(wgs84GRS)
greenTuff.SP  <- ge.splitPoly(greenTuff.sldf, hkdLand.SPDF )

greenTuff.df  <- fortify(greenTuff.SP)
greenTuff.df   <- greenTuff.df [!greenTuff.df $group == 1.4,] # eliminate
greenTuff.df$gtuff  <- 0
greenTuff.df[greenTuff.df$group == 1.2,]$gtuff  <- 1
greenTuff.df[greenTuff.df$group == 1.3,]$gtuff  <- 1
greenTuff.df  <- greenTuff.df[greenTuff.df$gtuff  == 1,]
levels(factor(greenTuff.df$gtuff))
#ge.sp2shpGeo(hkdLand)
#ggplot(greenTuff.df) + geom_polygon(aes(x = long, y = lat, group=group, fill = group))

### Plot Belt
Belt  <- "Belt/doc.kml"
ogrListLayers(Belt)
belt.sldf  <-readOGR(Belt, "Belt")
belt.sldf
proj4string(belt.sldf)  <- CRS(wgs84GRS)
belt.SP <- ge.splitPoly(belt.sldf, hkdLand.SPDF )
plot(belt.SP)
belt.dfr  <- fortify(belt.SP)
ggplot(belt.df) + geom_polygon(aes(x = long, y = lat, group=group, fill = group))
belt.df  <- belt.dfr[!(belt.dfr$group == "1.7" |
                       belt.dfr$group == "1.8" |
                       belt.dfr$group == "1.9" |
                       belt.dfr$group == "1.10"),]
belt.df[belt.df$group == 1.6,]$group = "1.5"
belt.df$name  <-  "Hidaka & Tokoro belt"
belt.df[belt.df$group == 1.2,]$name  <- "North Kitakami belt"
belt.df[belt.df$group == 1.3,]$name  <- "Nemuro belt"
belt.df[belt.df$group == 1.4,]$name  <- "Kamuikotan belt"
belt.df[belt.df$group == 1.5,]$name  <- "Sorachi-Yezo belt"


levels(factor(belt.df$group))

#ge.sp2shpGeo(hkdLand)

### Make study Boundary


basemap.r  <- readRDS("~/Dropbox/2data/dataProduct/hkd/hkd_google_satellite_142.5_43.5_zoom7_140815_2131.Rds")
jpVolA.spdf  <- readRDS("~/Dropbox/2data/dataProduct/jpVolcanoes/jpVol110_140812_174525.Rds")
xmin <- 139
xmax <- 146
ymin <- 41.4
ymax <- 45.8
bbox.SPDF <- ge.xy2bboxSPDF(xmin,xmax,ymin,ymax,wgs84GRS)
proj4string(jpVolA.spdf) <- proj4string(bbox.SPDF)
volA <- jpVolA.spdf[bbox.SPDF,]
limitsX  <- c(139,146)
breaksX  <- seq(limitsX[1], limitsX[2],1)
labelsX=parse(text=paste(breaksX, "^o ", "*E", sep=""))
limitsY  <- c(41,46)
breaksY  <- seq(limitsY[1],limitsY[2],1)
labelsY=parse(text=paste(breaksY, "^o ", "*N", sep=""))
# limitsX  <- c(138,147)
# breaksX  <- seq(limitsX[1], limitsX[2],1)
# labelsX=parse(text=paste(breaksX, "^o ", "*E", sep=""))
# ##limitsY  <- c(41,47)
# limitsY  <- c(40,47)
# breaksY  <- seq(limitsY[1],limitsY[2],1)
# labelsY=parse(text=paste(breaksY, "^o ", "*N", sep=""))
## Layer0: Base map
ggBase  <-  ggmap(basemap.r, extent = "panel") +
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
                           expand = c(0.01,0.01))
ggBase
cols <- c("Hidaka & Tokoro belt" = "cyan",
          "North Kitakami belt" = "orange",
          "Nemuro belt" = "pink",
          "Kamuikotan belt" = "palegreen",
          "Sorachi-Yezo belt" = "yellow")

ggBelt  <- ggBase +
        geom_polygon(aes(x = long, y = lat, group=group, fill = name),
                               data = belt.df) +
        scale_fill_manual(name =  "Geological belt", values =cols)

ggTuff  <- ggBelt +
        geom_polygon(aes(x = long, y = lat, group=piece, size = factor(gtuff)),
                     data = greenTuff.df,
                     color = "green",
                     alpha  = 0.2) +
        scale_size_manual(name =  "Green tuff", values =1, labels = "Green tuff area" )
ggVol  <- ggTuff  +
        geom_point(data = volA@data,
                   aes(as.numeric(lon), as.numeric(lat), color="red"),
                   shape = 17, size = 3)  +
        scale_color_manual(name =  "Volcanoes",
                           values = c("red"), labels = c("Active volcanoes"))



# ggFont
##ge.ggsave(hkdStudyArea)


jpTlines.sldf  <- readRDS("~/Dropbox/2data/dataProduct/jp/jpTlines_141125_221917.Rds")
hkdTlines.sldf  <- crop(jpTlines.sldf, bbox2.SPDF)
#plot(hkdTlines.sldf)
hkdTlines.df  <- fortify(hkdTlines.sldf)
## regroup
hkdTlines.df$id2 <- 2
hkdTlines.df[hkdTlines.df$id == 1,]$id2 <- 1
hkdTlines.df[hkdTlines.df$id == 3,]$id2 <- 1
ggTlines  <-ggVol + geom_line(aes(long,lat,group=group, linetype=factor(id2)),
                                 color = "red",
                                 #linetype = 2,
                                 size = 1,
                                 hkdTlines.df) +
        scale_linetype_manual(name =  "Tectonic line", values = c(1,3),
                              labels = c("Tectonic line","Volcanic front"))


hkddem  <- raster("hkdDEM1000.tif")
hkdDEM.spdf  <- rasterToContour(hkddem, levels = seq(500,2000,500))
hkdDEM.df  <- fortify(hkdDEM.spdf)

ggContour  <- ggTlines +
        geom_path(aes(long,lat,group=group, alpha=id),hkdDEM.df, color ="brown") +
        scale_alpha_manual(name =  "Elevation contour (m)", values = c(0.3,0.5,0.7,1),
                           labels = c(as.character(seq(500,2000,500))))

ggContour
ggBar  <- ggTlines +scaleBar(lon = 144, lat = 41, distanceLon = 50, distanceLegend = 30,distanceLat = 15, dist.unit = "km", arrow.length = 60, arrow.distance = 450, arrow.North.size = 4,legend.colour = "white", arrow.North.color = "white", arrow.colour = "blue")


ggFont  <- ggBar +
        #coord_equal() +
        theme_bw(base_family = "Times", base_size = 12)

# g  <- guide_legend("Tectonic lines")
# ggGuid  <- ggFont + guides(size = g, linetype=g)
hkdGreenTuff  <-ggFont
# 7*5
#ge.ggsave(hkdGreenTuff)
