##################################
# Creating Inset Maps in ggplot2 #
##################################


xmin <- 139
xmax <- 146
ymin <- 41.4
ymax <- 45.8
crs  <- wgs84GRS
prs  <- lccWgs84
bbox_SPDF <- phd.bbox(xmin,xmax,ymin,ymax,crs)
jp1  <- raster::getData('GADM', country='JPN', level=1, path = "~/Dropbox/2data//dataRaw/gadm2")
jp2  <- raster::getData('GADM', country='JPN', level=2, path = "~/Dropbox/2data//dataRaw/gadm2")
sub  <- phd.largestPolys(jp1, Polygon =T)

# sp::plot(gadm)
bh  <- readRDS("~/Dropbox/2data/dataProduct/hkd/hkd_profiles_140806_164333.Rds")
bh_xy  <- bh[!duplicated(bh$ID),]
subset()
head(bh_xy)
unique(bh$ID)
row
volA  <- readRDS("~/Dropbox/2data/dataProduct/jpVolcanoes/jpVol110_140812_174525.Rds")
volQ  <- readRDS("~/Dropbox/2data/dataProduct/jpVolcanoes/jpVol455_140812_172148.Rds")
sp::proj4string(volQ) <- sp::proj4string(bbox_SPDF)
volQsub <- volQ[bbox_SPDF,]
sp::proj4string(volQ) <- sp::proj4string(sub)
volQsub2 <- volQ[sub,]
sp::proj4string(volA) <- sp::proj4string(bbox_SPDF)
volAsub <- volA[bbox_SPDF,]
sp::proj4string(volA) <- sp::proj4string(sub)
volAsub2 <- volA[sub,]
plot(bh$Lon, bh$Lat, pch = 20, xlab = "Longitude", ylab = "Latitude", xlim = c(xmin,xmax), ylim = c(ymin,ymax))
sp::plot(volAsub, pch = 17, col = "red", add = TRUE)
sp::plot(volQsub, col = "blue", add = TRUE)
sp::plot(sub, add = TRUE)

hkd<-coordinates(sub) # get center coordinates of municipalities of Marinduque
hkd<-data.frame(hkd) # convert matrix format munnames object to data.frame
hkd$label<-"Hokkaido"
###Map
library(ggplot2)
library(raster)
library(gridExtra)

## Main
hkdBasemap  <- get_googlemap(center = c(lon = 143, lat = 43.5), zoom = 7,
                             maptype = "terrain", filename = "terrain_hkd")
p1 <- ggmap(basemap, extent = "panel") +
        #geom_point(data = Hokkaido, aes(Lon, Lat, size = Depth, colour = Type))
        geom_point(data = Hokkaido_xy, aes(Lon, Lat, color = grp, size = grp), alpha = 0.8) +
        xlab("Lontitude") +
        ylab("Latitude") +
        labs(size = "Depth (m)") +
        labs(color = "Depth (m)")
p <- p + scale_color_brewer(palette="Greys")
p <- p + scale_size_manual(values=c(1,1.5,2,2.5,4))
p
p <- p + geom_point(data = volHokkaido_df, aes(lon, lat), color = "Red", pch = 17)
# Extent rectangle for inset map
pol<-data.frame(xmin=121.7,xmax=122.2 ,ymin=13 ,ymax=13.7)

###
# Main Map
p1<-ggplot()+geom_polygon(data=mrdq, aes(long+0.008,lat-0.005, group=group), fill="#9ecae1")+
        geom_polygon(data=mrdq, aes(long,lat, group=group), colour="grey10",fill="#fff7bc")+
        geom_text(data=munnames, aes(x=X1, y=X2,label=label), size=3, colour="grey20")+
        coord_equal()+theme_bw()+xlab("")+ylab("")+
        scale_x_continuous(breaks=seq(121.8,122.2, 0.1), labels=c(paste(seq(121.8,122.2, 0.1),"??E", sep="")))+
        scale_y_continuous(breaks=seq(13.2,13.6, 0.1), labels=c(paste(seq(13.2,13.6, 0.1),"??N", sep="")))+
        theme(axis.text.y =element_text(angle = 90, hjust=0.5))

#Inset
p2<-ggplot()+geom_polygon(data=ph0, aes(long,lat,group=group),colour="grey10",fill="#fff7bc")+
        coord_equal()+theme_bw()+labs(x=NULL,y=NULL)+
        scale_x_continuous(breaks=seq(117.5,125, 2.5), labels=c(paste(seq(117.5,125, 2.5),"??E", sep="")))+
        scale_y_continuous(breaks=seq(5,20, 5), labels=c(paste(seq(5,20, 5),"??N", sep="")))+
        geom_rect(data = pol, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), alpha=0, colour="red", size = 1, linetype=1)+
        theme(axis.text.x =element_blank(),axis.text.y= element_blank(), axis.ticks=element_blank(),axis.title.x =element_blank(),
              axis.title.y= element_blank())

png(file="mrdq.png",w=1800,h=1800, res=300)
grid.newpage()
v1<-viewport(width = 1, height = 1, x = 0.5, y = 0.5) #plot area for the main map
v2<-viewport(width = 0.3, height = 0.3, x = 0.86, y = 0.28) #plot area for the inset map
print(p1,vp=v1)
print(p2,vp=v2)
dev.off()
