
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
