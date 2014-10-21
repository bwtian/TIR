Code  <- c(1,2,3,4,5,6,8,10,11)
LULC  <- c("Water", "Urban", "Paddy", "Crop","Grass", "Deciduous forest",
           "Evergreen forest", "Bare land", "Snow and ice")
Color  <- c("darkblue", "red", "skyblue", "yellow", "yellowgreen", "springgreen", "forestgreen", "saddlebrown", "white")
## emisivity at band 10E
Emissivity <- c(0.99, 0.93, 0.96, 0.95, 0.96,0.97,0.98,0.92,0.90)
df  <- cbind(Code,LULC,Emissivity,Color)
