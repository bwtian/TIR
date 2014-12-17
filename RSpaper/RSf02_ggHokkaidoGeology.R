### p1402fig2 Geology setting 141210
### Core Geology Data
source("~/SparkleShare/Rprofile/R/Rsettings/phdRsettings.R")
setwd(dir.hkd)
hkdFault.sldf  <- readRDS("hkdFault.sldf_141126_221926.Rds")
hkdFault.df  <- fortify(hkdFault.sldf)
#levels(factor(hkdFault.df$piece))

hkdGeoPoly.SPDF  <- readRDS("hkdGeoPoly.SPDF_141126_222720.Rds")
hkdGeoPoly.df  <- fortify(hkdGeoPoly.SPDF)
hkdRocks.SPDF  <- unionSpatialPolygons(hkdGeoPoly.SPDF, hkdGeoPoly.SPDF@data$Division_E)
#hkdRocks.SPDF
#levels(factor(hkdGeoPoly.df$id))
#plot(hkdRocks.SPDF)
hkdRocks.df  <- fortify(hkdRocks.SPDF)
levels(factor(hkdRocks.df$id))

### Make study Boundary


basemap.r  <- readRDS("~/Dropbox/2data/dataProduct/hkd/hkd_google_satellite_142.5_43.5_zoom6_141018_2339.Rds")
jpVolA.spdf  <- readRDS("~/Dropbox/2data/dataProduct/jpVolcanoes/jpVol110_140812_174525.Rds")
xmin <- 139
xmax <- 146
ymin <- 41.4
ymax <- 45.8
bbox.SPDF <- ge.xy2bboxSPDF(xmin,xmax,ymin,ymax,wgs84GRS)
proj4string(jpVolA.spdf) <- proj4string(bbox.SPDF)
volA <- jpVolA.spdf[bbox.SPDF,]
# limitsX  <- c(139,146)
# breaksX  <- seq(limitsX[1], limitsX[2],1)
# labelsX=parse(text=paste(breaksX, "^o ", "*E", sep=""))
# limitsY  <- c(41,46)
# breaksY  <- seq(limitsY[1],limitsY[2],1)
# labelsY=parse(text=paste(breaksY, "^o ", "*N", sep=""))
limitsX  <- c(138,147)
breaksX  <- seq(limitsX[1], limitsX[2],1)
labelsX=parse(text=paste(breaksX, "^o ", "*E", sep=""))
##limitsY  <- c(41,47)
limitsY  <- c(40,47)
breaksY  <- seq(limitsY[1],limitsY[2],1)
labelsY=parse(text=paste(breaksY, "^o ", "*N", sep=""))
## Layer0: Base map
ggBase  <-  ggmap(basemap.r, extent = "panel") +
  xlab("Longitude") +
  scale_x_continuous(breaks=breaksX,
                     labels=labelsX,
                     limits=limitsX,
                     expand = c(0.01,0.01)) +

  ### Y
  ylab("Latitude") +
  scale_y_continuous(breaks=breaksY,
                     labels=labelsY,
                     limits=limitsY,
                     expand = c(0.01,0.01))
cols <- c("accretionary complex" = "blue",
          "metamorphic rock" = "purple",
          "pultonic rock" = "pink",
          "sedimentary rock" = "palegreen",
          "volcanic rock" = "yellow"
          )
#levels(factor(hkdRocks.df$id))
ggRock  <-  ggBase +
  geom_polygon(aes(long,lat,group=group, fill=id), hkdRocks.df[!hkdRocks.df$id =="water",]) +
  scale_fill_manual(name =  "Rock types", values =cols, labels = c(
    "Accretionary complex",
    "Metamorphic rock",
    "Pultonic rock",
    "Sedimentary rock",
    "Volcanic rock"))
ggFault  <- ggRock +
  geom_path(aes(long, lat, group=group, size = factor(0)), hkdFault.df,
            color = "black",
            alpha = 0.7)  +
  scale_size_manual(name =  "Tectonic lines", values = 0.5 ,labels = "Faults")

ggVol  <- ggFault  +
  geom_point(data = volA@data,
             aes(as.numeric(lon), as.numeric(lat), color="red"),
             shape = 17, size = 2)  +
  scale_color_manual(name =  "Volcanoes",
                     values = c("red"), labels = c("Active volcanoes"))



# ggFont
##ge.ggsave(hkdStudyArea)


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


ggPlate  <- ggVol  + geom_path(aes(long,lat,group=piece),
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
  theme_bw(base_family = "Times", base_size = 12) +
  theme(axis.text.y = element_text(angle = 90, hjust = 0.5, vjust = 0),
       axis.title.x = element_text(vjust = 0.25))
# g  <- guide_legend("Tectonic lines")
# ggGuid  <- ggFont + guides(size = g, linetype=g)
hkdGeology  <-ggFont
# hkdGeology
# 7*5
ggsave(plot = hkdGeology, "hkdGeology.pdf", width =7, height = 5.5)
# ge.ggsave(hkdGeology)
