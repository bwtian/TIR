#' @export
rs.untar  <- function(list, path = ".", outdir = "../tif", pattern = ".tgz") {
        tarFiles_v <- list.files(path = path, pattern = pattern, all.files = T, full.names = T)
        sapply(tarFiles_v, function(i) {utils::untar(tarfile = i, exdir = file.path(outdir, tools::file_path_sans_ext(basename(i))))})
}
