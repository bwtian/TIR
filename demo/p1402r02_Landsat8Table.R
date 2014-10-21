library("xtable")
scene  <- read.csv("Data//hkdMinCloud.csv")
s  <- scene[,c(2,5:8)]
s  <- s[order(s[,1]),]
colnames(s)  <- c("Landsat 8 Identifier", "Date", "Cloud($\\%$)",
               "Sun Elevation($\\^o$)", "Sun Azimuth($\\^o$)")

print(xtable(s,
             caption = "Landsat 8 data used",
             label ="tbl:l8data",
             align = "cccccc"),
      sanitize.text.function=function(x){x},
      include.rownames=FALSE)


