
rs.tif2png <- function(dir = getwd()){
        inRaster <- list.files(path = getwd(), pattern = ".tif$", full.names = TRUE, recursive = TRUE)
        for (i in inRaster) {
                outName <- gsub("\\.tif", "\\.tif.png", i)
                r <- raster(i) *0.02
#                 p <- rasterToPoints(r, fun=function(x){x>250})
#                 ra2  <- cellFromXY(ras, p[,1:2])
#                 m  <- r[]
#                 m[m < 250] <- NA
#                 mr   <- mask[r,m]
                #mr <- mask(r, m)
cols = bpy.colors(30)
#cols = rainbow(200, start = 2/6, end = 1)
# zmin = 250
# zmax = 320


                 m  <- r > 250
                 r  <- mask(r,m)
                png(filename = outName)
                #plot(r)
                pict <- spplot(r, col.regions = cols )
                #pict   <- image(r, zlim=c(zmin,zmax),col=cols)
                #pict  <- image(r, col=cols)
                print(pict)
                dev.off()
        }
}
tir.tif2png  <- function(){
files <- list.files(path=getwd(), pattern="*.tif", all.files=T, full.names=T)
# for loop for processing all the image in tif files
# infile is a file form tif files, outfile is a output file
for (infile in files)
{
        #gsub to rename the infile to outfile
        outfile <- gsub("\\.tif$","-edit\\.tif", infile)
        # rsdata is a complicated slot data format created by readGDAL
        rsdata <- readGDAL(infile)
        # change Backgroud or missed vaule (here 0) to NoData,Deepend on
        rsdata@data[rsdata@data[,1] == 0,1] <- NaN
        # band Math formular, need to change deepend on caculate
        lst <- rsdata@data[,1]*0.02
        rsdata@data[,1] <- data.frame(band1=lst)
        # Export the Bandmath result
        writeGDAL(rsdata, outfile)
        #### Export the bandmath result with colormap file
        outpict <- gsub("-edit\\.tif","-edit\\.tif.png" , outfile)
        pict <- spplot(rsdata,
                       col.regions = rainbow(200, start = 2/6, end = 1))
        png(filename = outpict)
        print(pict)
        dev.off()
        ####
        # delete original files  #unlink(infile)
        file.remove(infile)
}
}
