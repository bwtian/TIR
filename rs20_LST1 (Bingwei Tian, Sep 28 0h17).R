#' @title Calculate LST using Arits and Carnathan 1982
#' @param Ts = Tb/(1+(L*Tb/a)lne); a = hc/j
#' @param h is Planck's constant (6.626*10^-34 Js)
#' @param c is the velocity of light (2.998*10^8 m/s)
#' @param j is Bolzman constant (1.38*10^-23 J/K)
#' @param a is h*c/j (1.439*10^-2 m/K)
#' @param e is a Raster* object with Emmissvity Value caluateed by NDVI or LULC
#' @param
source("~/SparkleShare/Rprofile/R/sourceDir.R")
sourceDir("~/SparkleShare/Rprofile/R/")
dir.tmp <- "~/Share500sda/Landsat8/raster_tmp"
rasterOptions(tmpdir = dir.tmp)
dir.toaTbKlcc  <-  "~/Share500sda/Landsat8/at1_TOA/toaTbKlcc"
dir.sufTsKlcc  <-  "~/Share500sda/Landsat8/at2_Surface/toaTsKlcc"
dir.lulc  <- "~/Share500sda/Landsat8/at9_Database/LULC/hkdEmissivity.tif"
if (!file.exists(dir.sufTsKlcc)){
        dir.create(dir.sufTsKlcc)
}
L10  = 10.9*10^-6
L11  = 12*10^-6
h = 6.626*10^-34
c = 2.998*10^8
j = 1.38*10^-23
p = h*c/j
e  <- raster::raster("~/Share500sda/Landsat8/at9_Database/LULC/")
tif10 <- list.files(path= dir.toaTbKlcc,
                            pattern= "B10.tif$",
                            all.files=TRUE,
                            full.names=TRUE,
                            recursive=TRUE,
                            ignore.case=TRUE)
r10.rst  <- lapply(tif10, raster)
hkdmaskb  <- readRDS("~/SparkleShare/TIR/hkdmskb_grdi2d1h.Rds")
r10.msk  <- lapply(tif10, function(x) mask(x, hkdmaskb))
for (i in r10.msk) {
        outName  <- paste0(names(i), ".tif")
        #projectRaster(from = i, crs = toCRS,  method = "ngb",
        Tsk  <- i/(1+(L10*i/p)*log(e))
        writeRaster(Tsk,
                    filename = file.path(dir.sufTsKlcc, outName),
                    overwrite = T)
        raster::removeTmpFiles(h = 1) ## Improtant tips for save hardisk
}
tif11 <- list.files(path= dir.toaTbKlcc,
                    pattern= "B11.tif$",
                    all.files=TRUE,
                    full.names=TRUE,
                    recursive=TRUE,
                    ignore.case=TRUE)
r11.rst  <- lapply(tif11, raster)
hkdmaskb  <- readRDS("~/SparkleShare/TIR/hkdmskb_grdi2d1h.Rds")
r11.msk  <- lapply(tif11, function(x) mask(x, hkdmaskb))
for (i in r11.msk) {
        outName  <- paste0(names(i), ".tif")
        #projectRaster(from = i, crs = toCRS,  method = "ngb",
        Tsk  <- i/(1+(L11*i/p)*log(e))
        writeRaster(Tsk,
                    filename = file.path(dir.sufTsKlcc, outName),
                    overwrite = T)
        raster::removeTmpFiles(h = 1) ## Improtant tips for save hardisk
}

