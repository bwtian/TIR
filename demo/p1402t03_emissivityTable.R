Code  <- c(1,2,3,4,5,6,8,10,11)
LULC  <- c("Water", "Urban", "Paddy", "Crop","Grass", "Deciduous forest",
           "Evergreen forest", "Bare land", "Snow and ice")
Color  <- c("darkblue", "red", "skyblue", "yellow", "yellowgreen", "springgreen", "forestgreen", "saddlebrown", "white")
## emisivity at band 10 averange LST change with season
Emissivity <- c(0.99, 0.93, 0.96, 0.95, 0.96,0.97,0.98,0.92,0.91)
df  <- cbind(Code,LULC,Emissivity,Color)
library(xtable)
print(xtable(df,
             caption = "Emissivity retreived from land use and land cover data",
             label ="tbl:emi",
             align = "c|cccc"),
      sanitize.text.function=function(x){x},
      include.rownames=FALSE)
