
#' Copy files in the list from fromdir to todir
#' @param list LC81050292013262LGN00.tgz
#' @param formdir folders contain the Image usrally the download or database
#' @param todir  Where the files will copy to, usually the workdir

    rs.cptar <- function(list, fromdir, todir){
            if (all(list %in% dir(fromdir))){
                    for (i in list){
                            file.copy(file.path(fromdir,i), todir)
                            ifile <- file.path(todir,i)
                            untar(tarfile = ifile, exdir = file.path(todir,tools::file_path_sans_ext(basename(ifile))))
                            file.remove(i)
                    }
            } else {
                    stop (paste("Not all the files in", fromdir))
            }
    }
