## Lastedit: 20141210
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
#plot(hkdLand)
volQ2  <- jpVolQ.spdf[hkdLand,]
#volA@data
A  <- c(1,7,10,12,15,18)
volAA  <- volA[volA$ID %in% A,]
#volAA@data
limitsX  <- c(138,147)
breaksX  <- seq(limitsX[1], limitsX[2],1)
labelsX=parse(text=paste(breaksX, "^o ", "*E", sep=""))
##limitsY  <- c(41,47)
limitsY  <- c(40,47)
breaksY  <- seq(limitsY[1],limitsY[2],1)
labelsY=parse(text=paste(breaksY, "^o ", "*N", sep=""))
## Layer0: Base map
ggBH  <-  ggmap(basemap.r, extent = "panel") +
        ### Layers
        geom_point(data = bh_xy, aes(Lon, Lat,fill = grp, size = grp),
                   shape = 21, alpha = 0.9) +

        scale_x_continuous(name = " Longitude",
                           breaks=breaksX,
                           labels=labelsX,
                           limits=limitsX,
                           expand = c(0.01,0.01)) +

        scale_y_continuous(name = " Latitude",
                           breaks=breaksY,
                           labels=labelsY,
                           limits=limitsY,
                           expand = c(0.01,0.01)) +
       theme(axis.text.y = element_text(angle = 90, hjust = 0.5, vjust = 0),
             axis.title.x = element_text(vjust = 0)) +
       # labs(size = "Borehole depth (m)") +
        scale_size_manual("Borehole depth (m)",values=c(1,1.5,2,3,4)) +
        scale_fill_brewer("Borehole depth (m)", palette="Blues")
ggVol  <- ggBH  +
        geom_point(data = volQ@data,
                   aes(as.numeric(lon), as.numeric(lat),
                       color="blue"), shape = 17, alpha = 0.7) +
        geom_point(data = volA@data,
                   aes(as.numeric(lon), as.numeric(lat),
                      color="red"),  shape = 17, size = 2)  +
        geom_point(data = volAA@data,
             aes(as.numeric(lon), as.numeric(lat)), color="white",
             shape = 3, size = 2) +
        scale_color_manual(name =  "Volcanoes", values = c("orange","red"), labels = c("Quaternary volcanoes","Active volcanoes")) +
        geom_path(data = volQ2@data, aes(as.numeric(lon), as.numeric(lat)),size = 12, alpha = 0.2, colour = "yellow",lineend = "round")

ggSap  <- ggVol + geom_point(data = sap.spdf, aes(x = lon, y = lat), colour = "White")  +
  geom_text(data = sap.spdf, aes(x = lon, y = lat, label = name), hjust = -0.1,family="Times", face="italic", colour="white")


library(wrspathrow)
wrs2.SPDF  <- pathrow_num(x = hkdLand, as_polys = TRUE)
#plot(wrs2.SPDF,col = "red")
#wrs2.SPDF@data
wrs2.df  <- fortify(wrs2.SPDF)

ggWRS  <-ggSap + geom_polygon(aes(long,lat,group=group),
                    color = "grey", alpha = 0.3, fill = NA,
                    linetype = 3,
                    data=wrs2.df) +
        geom_point(data = wrs2.SPDF@data, color = "black",
                   aes(x = centroid_x, y = centroid_y, shape = MODE ))

ggWRS2  <- ggWRS +  geom_text(data = wrs2.SPDF@data,
                  aes(x = centroid_x, y = centroid_y),
                  label = paste(wrs2.SPDF@data$PATH, wrs2.SPDF@data$ROW," "),
                  family="Times", face = "Italic", colour="black", size = 4) +
        scale_shape_manual(name =  " WRS 2", values = 20 , labels = c("Path and Row"))
### Plate Boundaries

jpArc.sldf  <- readRDS("~/Dropbox/2data/dataProduct/jp/jpPlateBoundary_141124_223221.Rds")
#plot(jpArc.sldf)
bbox2.SPDF <- ge.xy2bboxSPDF(138,147,40,47,wgs84GRS)
hkdArc.sldf  <- crop(jpArc.sldf, bbox2.SPDF)
hkdArc.df  <- fortify(hkdArc.sldf )
hkdArc.df  <- hkdArc.df[order(hkdArc.df$lat),]
rownames(hkdArc.df)  <- seq_along(hkdArc.df$lat)
# summary(hkdArc.df)
# ggWRS2 + geom_point(aes(long,lat,group=group),
#                       color = "red",
#                       linetype = 1,
#                       hkdArc.df) +


ggPlate  <- ggWRS2 + geom_path(aes(long,lat,group=piece),
                    color = "red",
                    linetype = 1,
                    size = 1,
                    hkdArc.df) +
  geom_text(aes(x = 144.5, y = 41.4, label = "Kuril Trench"),
            hjust = -0.1, angle = 35, family="Times", colour="white",
            size = 4) +
  geom_text(aes(x = 143.5, y = 40, label = " Japan \n Trench"),
            hjust = -0.1, angle = 90, family="Times", colour="white",
            size = 4) +
  geom_text(aes(x = 139.4, y = 44, label = "Plate Boundary"),
            hjust = -0.1, angle = 78, family="Times", colour="white",
            size = 4)

jpTlines.sldf  <- readRDS("~/Dropbox/2data/dataProduct/jp/jpTlines_141125_221917.Rds")
hkdTlines.sldf  <- crop(jpTlines.sldf, bbox2.SPDF)
#plot(hkdTlines.sldf)
hkdTlines.df  <- fortify(hkdTlines.sldf)
## regroup
hkdTlines.df$id2 <- 2
hkdTlines.df[hkdTlines.df$id == 1,]$id2 <- 1
hkdTlines.df[hkdTlines.df$id == 3,]$id2 <- 1
ggTlines  <- ggPlate + geom_line(aes(long,lat,group=group, linetype=factor(id2)),
  color = "red",
  #linetype = 2,
  size = 1,
  hkdTlines.df) +
  scale_linetype_manual(name =  "Tectonic lines", values = c(1,3),
                                    labels = c("Tectonic lines","Volcanic front"))
ggBar  <- ggTlines  +
  scaleBar(lon = 139, lat = 40, distanceLon = 100,
           distanceLegend = 30, distanceLat = 15,
           dist.unit = "km", arrow.length = 60,
           arrow.distance = 680, arrow.North.size = 4,
           legend.colour = "white", arrow.North.color = "white", arrow.colour = "blue")

ggFont  <- ggBar +
  #coord_equal() +
  theme_bw(base_family = "Times")
hkd  <-ggFont
# ge.ggsave(hkd)
# ge.ggsave
ggsave(plot = hkd, "hkd.pdf", width = 7, height =5.3)
# getwd()
