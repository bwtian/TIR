source("~/SparkleShare/Rprofile/R/Rsettings/phdRsettings.R")
setwd(dataDir)
getwd()
hkdKT  <- readRDS("hkd/hkd_kt3dlcc_140530_114352.Rds")
head(hkdKT)
df  <- subset(hkdKT, Z %% 200 == 0)
g1 <- ggplot(df) +
geom_raster(aes(x = X, y =Y, col = Temperatrue)) +
facet_wrap(~Z)
g1
g2  <- q1 + scale_x_continuous(label = function(x) x/1000) +
scale_y_continuous(label = function(x) x/1000) +
xlab("Easting (km)") +
ylab("Northing (km)")
g2
labelsY = c(min(y), seq(50,500, 50) max(y))
y  <- as.numeric(df$Temperatrue)
labelsY = c(min(y), seq(50,500, 50),max(y))
breaksY = as.numeric(labelsY)
g3  <- g2 +  scale_colour_gradientn(name = expression(Temperature~(degree*C)), colours = rev(rainbow(7)), breaks = breaksY, labels = format(breaksY)) +
theme_bw(base_size = 12, base_family = "Times")
g3


