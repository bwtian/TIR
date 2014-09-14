library(ggmap)
#' @import ggmap
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
