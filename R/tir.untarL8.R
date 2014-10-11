#' @export
tir.untarLandsat  <- function(path = ".", outdir = "../L8_tifs", pattern = ".tgz") {
        tarFiles_v <- list.files(path = path, pattern = pattern, all.files = T, full.names = T)
        sapply(tarFiles_v, function(i) {untar(tarfile = i, exdir = file.path(outdir,tools::file_path_sans_ext(basename(i))))})
}
