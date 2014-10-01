#' Reproject Raster
#'
#' @author Bingwei Tian
#' @param tif a list of tif files
#' @date 926
if (!file.exists(dir.toaTbKlcc)){
        dir.create(dir.toaTbKlcc)
}
tif <- list.files(path= dir.toaTbK ,
                  pattern= ".tif$",
                  all.files=TRUE,
                  full.names=TRUE,
                  recursive=TRUE,
                  ignore.case=TRUE)
r.rst  <- lapply(tif, raster)
hkdmaskb  <- readRDS("~/SparkleShare/TIR/hkdmskb_grdi2d1h.Rds")
for (i in r.rst) {
        outName  <- paste0(names(i), ".tif")
        #projectRaster(from = i, crs = toCRS,  method = "ngb",
        projectRaster(from = i,  to = hkdmaskb,
                      filename =  file.path(dir.toaTbKlcc, outName),
                      overwrite=TRUE)
        raster::removeTmpFiles(h = 1) ## Improtant tips for save hardisk
}
#test = projectRaster(r.rst[[1]], crs = toCRS, method = "ngb")
