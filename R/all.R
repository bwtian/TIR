#' Read NASA ASTER Global Emissivity Database (ASTER GED) Binary Datasets
#'
#' @export
#' @details Layers: 19
#'          Resolution: 100 m * 100 m
#'          Dimensions: 1000*1000*19
#'          NA: -9999

#' @author Bingwei Tian [2014-09-30 Tue]
#'
#' @param emiB10m  layer1 is  Emissivity mean of Band10 scaled by 1000
#' @param emiB11m  layer2 is  Emissivity mean of Band11 scaled by 1000
#' @param emiB12m  layer3 is  Emissivity mean of Band12 scaled by 1000
#' @param emiB13m  layer4 is  Emissivity mean of Band13 scaled by 1000
#' @param emiB14m  layer5 is  Emissivity mean of Band14 scaled by 1000
#' @param emiB10sd  layer6 is  Emissivity sdev of Band10 scaled by 10000
#' @param emiB11sd  layer7 is  Emissivity sdev of Band11 scaled by 10000
#' @param emiB12sd  layer8 is  Emissivity sdev of Band12 scaled by 10000
#' @param emiB13sd  layer9 is  Emissivity sdev of Band13 scaled by 10000
#' @param emiB14sd layer10 is  Emissivity sdev of Band14 scaled by 10000
#' @param LSTm    layer11 is  LST mean scaled by 100, Unit Kelvin(K)
#' @param LSTsd    layer12 is  LST sdev scaled by 100, Unit Kelvin(K)
#' @param NDVIm   layer13 is  NDVI mean scaled by 100 at TOA
#' @param NDVIsd   layer14 is  NDVI sdev scaled by 100 at TOA
#' @param Water   layer15 is  Land-Water Map
#' @param Obs     layer16 is  Observations, Images used
#' @param Lat     layer17 is  Latitude scaled by 1000
#' @param Lon     layer18 is  Longitude scaled by 1000
#' @param GDEM    layer19 is  ASTER Global DEM
#' @param bins    A list of Binary files need to read
#' @return A list of dataframes

readAG100B <- function(bins){
        out  <- list()
        for (i in bins) {
                toRead  <- file(i, "rb")
                data.v  <- readBin(toRead, integer(), size = 4, n = 19000000)
                close(toRead)
                layer.l <- split(data.v, ceiling(seq_along(data.v)/1000000))
                layer.d <- as.data.frame(layer.l)
                layer.d[layer.d == -9999]  <- NA
                #layer.m <- as.matrix(layer.d)
#                 scales  <- c(1000, 1000, 1000, 1000, 1000,
#                              10000, 10000, 10000, 10000, 10000,
#                              100, 100, 100, 100,
#                              1, 1, 1000, 1000, 1)
                #layer.ok  <- mapply("/", layer.d, scales)
                #layer.ok  <- layer.d/scales
                #layer.df  <- as.data.frame(layer.ok)
                layer.df  <- as.data.frame(layer.d)
                colnames(layer.df)  <- c("emiB10m", "emiB11m", "emiB12m",
                                         "emiB13m", "emiB14m", "emiB10sd",
                                         "emiB11sd", "emiB12sd", "emiB13sd",
                                         "emiB14sd", "LSTm", "LSTsd", "NDVIm",
                                         "NDVIsd", "Water", "Obs",
                                         "Lat", "Lon", "GDEM")
                idx <- seq_along(bins)[sapply(bins, function(x) i %in% x)]
                out[[idx]]  <- layer.df
        }
        return(out)
}

#' creates a raster with the TOA radiance
#'
#' @description Creates a raster with the TOA radiance
#'
#' @param landsat8 list returned by rLandsat8::ReadLandsat8
#' @param band Landsat 11 bandname (one of "aerosol", "blue", "green", "red", "nir", "swir1", "swir2", "panchromatic", "cirrus", "tirs1", "tirs2"
#' @return TOA Radiance raster
#' @examples \dontrun{
#' ls8 <- ReadLandsat8("LC81880342014174LGN00")
#' r <- ToTOARadiance(ls8, "red")
#' }
#' @note code form rLandsat8
#' @export
#' @import raster

 ToTOARadiance <- function(landsat8, band) {

   bandnames <-c("aerosol", "blue", "green", "red",
   "nir", "swir1", "swir2",
   "panchromatic",
   "cirrus",
   "tirs1", "tirs2")

   allowedbands <- bandnames

   if (!band %in% allowedbands)
   {
        stop(paste(band, "band not allowed"))
   }

   idx <- seq_along(bandnames)[sapply(bandnames, function(x) band %in% x)]

   ml <- as.numeric(landsat8$metadata[[paste0("radiance_mult_band_",idx)]])
   al <- as.numeric(landsat8$metadata[[paste0("radiance_add_band_",idx)]])

   TOArad <- landsat8$band[[band]] * ml + al

   return(TOArad)

 }

 ## Add new
 l8.DNtoTOARadiance <- function(idenfier){
bandnames <-c("aerosol", "blue", "green", "red",
   "nir", "swir1", "swir2",
   "panchromatic",
   "cirrus",
   "tirs1", "tirs2")

   sapply(bandnames, function(i){ToTOARadiance(idenfier, i)})
 }
#' Get Google Man using ggmap function
#'
#' @param lon Longitude
#' @param lat Latitude
#' @param zoom Zoom
#' @param prefix Prefix for the data saved
#' @import ggmap
#' @export
#' @author Bingwei Tian
#' @details
#' This fucntion will get the Google map from the
#'
#' @importFrom ggmap get_googlemap
#' @export
getGoogleMap <- function(lon, lat, zoom, prefix = "google"){
        ### ggmap 4 type of google map and save to Rds 08-15
        require(ggmap)
        x  <- deparse(substitute(lon))
        y  <- deparse(substitute(lat))
        z  <- deparse(substitute(zoom))
        now <- format(Sys.time(), "_%y%m%d_%H%M")
        for (i in c("terrain", "satellite", "roadmap", "hybrid")){
                fileName  <-  paste0(prefix,"_google_", i,"_",x,"_",y,"_zoom", z, now, ".Rds")
                file  <- get_googlemap(center = c(lon = lon, lat = lat), zoom = zoom,
                                       maptype = i, filename = fileName)
                saveRDS(file, file = fileName)
        }
}

#' Reads a Landsat 8 product
#' @description Reads a Landsat 8 product
#'
#' @param product name of the product, e.g. LC80522102014165LGN00. It must be in the working directory
#' @return list with metadata and raster bands
#' @examples \dontrun{
#' ReadLandsat8("LC80522102014165LGN00")
#' }
#'
#' @note  ReadLandsat8 Function orignally from Package rLandsat8
#'
#' @export
#' @import raster

ReadLandsat8 <- function(product) {

  raster.files <- list("aerosol"="file_name_band_1",
    "blue"="file_name_band_2",
    "green"="file_name_band_3",
    "red"="file_name_band_4",
    "nir"="file_name_band_5",
    "swir1"="file_name_band_6",
    "swir2"="file_name_band_7",
    "panchromatic"="file_name_band_8",
    "cirrus"="file_name_band_9",
    "tirs1"="file_name_band_10",
    "tirs2"="file_name_band_11"
    )

  meta.file <- paste0(product, "/", product, "_MTL.txt")

  if (!file.exists(meta.file))
       stop(paste(meta.file, "file not found."))

  textLines <- readLines(meta.file)

  counts <- count.fields(textConnection(textLines), sep="=")

  met <- read.table(text=textLines[counts == 2], as.is=TRUE, header=FALSE, sep="=", strip.white=TRUE, stringsAsFactors=FALSE)

  met <- read.table(text=textLines[counts == 2], as.is=TRUE, header=FALSE, sep="=", strip.white=TRUE, stringsAsFactors=FALSE, row.names = NULL, col.names=c("name", "value"))

  met <- met[!met$name == "GROUP", ]
  met <- met[!met$name == "END_GROUP", ]
  rownames(met) <- tolower(met[, "name"])
  met[, "name"] <- NULL

  met <- as.list(as.data.frame(t(met), stringsAsFactors=FALSE))

  bands=lapply(raster.files, function(x) {
    r <- raster(paste0(product, "/", met[[x]]))
    r@title <- names(raster.files)[seq_along(raster.files)[sapply(raster.files, function(a) x %in% a)]]
    NAvalue(r) <- 0
    return(r)
  })

  return(list(metadata=met,
    band=bands)
  )
}
require(raster)
rs.readL8 <- ReadLandsat8



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
        #file.remove(infile)
}
}
#' @export
tir.todown <- function(list, downeddir, tolistName){
        downedFiles  <- list.files(downeddir)
        todownList   <-  setdiff(list, downedFiles)
        Now <- format(Sys.time(), "_%y%m%d_%H%M%S")
        todownListName <- paste0(todownListName, Now, ".txt")
        write.table(todownList , todownListName, quote = F, row.names = F,col.names = F)
}
#' @export
rs.untar  <- function(list, path = ".", outdir = "../tif", pattern = ".tgz") {
        tarFiles_v <- list.files(path = path, pattern = pattern, all.files = T, full.names = T)
        sapply(tarFiles_v, function(i) {utils::untar(tarfile = i, exdir = file.path(outdir, tools::file_path_sans_ext(basename(i))))})
}
#' @export
tir.untarLandsat  <- function(path = ".", outdir = "../L8_tifs", pattern = ".tgz") {
        tarFiles_v <- list.files(path = path, pattern = pattern, all.files = T, full.names = T)
        sapply(tarFiles_v, function(i) {untar(tarfile = i, exdir = file.path(outdir,tools::file_path_sans_ext(basename(i))))})
}

#' Select and untar files in the list form tar dir to tif dir
#'
#'
#' @param list like LC81050292013262LGN00.tgz or LC81050292013262LGN00.tar.gz
#' @param formdir folders contain the Image usrally the download or database
#' @param todir  Where the files will copy to, usually the workdir
#' @export
rs.untar <- function(list, fromdir, todir){
        if (all(list %in% dir(fromdir))){
                sapply(list, function(i) {
                        sceneDir <-file.path(tools::file_path_as_absolute(todir), gsub(pattern = "(^[^.]+)(.*)", replacement = "\\1", i))
                        utils::untar(tarfile = file.path(tools::file_path_as_absolute(fromdir),i), exdir = sceneDir)
                })
          } else {
                  stop (paste("Not all the files in", fromdir, "Use rs.todown to check it!"))
          }
}
