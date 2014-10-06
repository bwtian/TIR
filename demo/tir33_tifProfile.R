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
