
rs.untar  <- function(list, outdir = "../tif", pattern = ".tgz") {
        
        sapply(list, function(i) {untar(tarfile = i, exdir = file.path(outdir, tools::file_path_sans_ext(basename(i))))})
}
