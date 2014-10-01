source("./tirSettings.R"
dataDir  <- dir.toaTbKlcc
setwd(dataDir)
r = raster("LC81050292014153LGN00_B10.tif")
png("test.png")
plot(r)
dev.off()
rs.tif2png()
