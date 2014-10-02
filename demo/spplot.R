library(lattice)
trellis.par.set(sp.theme()) # sets bpy.colors() ramp
data(meuse)
coordinates(meuse) <- ~xy
l2 = list("SpatialPolygonsRescale", layout.north.arrow(), offset = c(181300,329800),
            scale = 400)
l3 = list("SpatialPolygonsRescale", layout.scale.bar(), offset = c(180500,329800),
             	scale = 500, fill=c("transparent","black"))
l4 = list("sp.text", c(180500,329900), "0")
l5 = list("sp.text", c(181000,329900), "500 m")
spplot(meuse, c("ffreq"), sp.layout=list(l2,l3,l4,l5), col.regions= "black",
                  	pch=c(1,2,3), key.space=list(x=0.1,y=.95,corner=c(0,1)))
spplot(meuse, c("zinc", "lead"), sp.layout=list(l2,l3,l4,l5, which = 2),
          	key.space=list(x=0.1,y=.95,corner=c(0,1)))
 # plotting factors:
         meuse$f = factor(sample(letters[6:10], 155, replace=TRUE),levels=letters[1:10])
 meuse$g = factor(sample(letters[1:5], 155, replace=TRUE),levels=letters[1:10])
 spplot(meuse, c("f","g"), col.regions=bpy.colors(10))

         if (require(RColorBrewer)) {
                 	spplot(meuse, c("ffreq"), sp.layout=list(l2,l3,l4,l5),
                           		col.regions=brewer.pal(3, "Set1"))
                 }
Loading required package: RColorBrewer

         data(meuse.grid)
 gridded(meuse.grid)=~xy
 meuse.grid$g = factor(sample(letters[1:5], 3103, replace=TRUE),levels=letters[1:10])
 meuse.grid$f = factor(sample(letters[6:10], 3103, replace=TRUE),levels=letters[1:10])
 spplot(meuse.grid, c("f","g"))
 spplot(meuse.grid, c("f","g"), col.regions=bpy.colors(10))


f <- system.file("external/test.grd", package="raster")
r <- raster(f)

levelplot(r) +
        layer({
                xs <- seq(181000, 181400, by=100)
                grid.rect(x=xs, y=330500,
                          width=100, height=30,
                          gp=gpar(fill=rep(c('transparent', 'black'), 2)),
                          default.units='native')
                grid.text(x= xs - 50, y=330560, seq(0, 400, by=100),
                          gp=gpar(cex=0.5), rot=30,
                          default.units='native')
        })
levelplot(r, margin=FALSE, auto.key=FALSE, scales=list(draw=FALSE)) +
        layer({
                SpatialPolygonsRescale(layout.north.arrow(),
                                       offset = c(179000,332500),
                                       scale = 400)
        })

