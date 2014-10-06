source("~/SparkleShare/TIR/demo/tirSettings.R")
setwd(dir.toaTbKlccCenterMos)
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
pf1  <- extract(mos, line)
class(pf1)
# create raster with cell numbers
idx <- init(mos, v='cell')
# extract these
cells <- extract(ids, line)
cells
# compute xy
xy = lapply(cells, function(x) xyFromCell(mos, x))
head(xy)
topo_profile = extract(x=mos, y=line, along=TRUE, cellnumbers=TRUE)
class(topo_profile)

summary(topo_profile)
plot(topo_profile, type='l')

