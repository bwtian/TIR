
rs.cptar <- function(list, fromdir, todir){

        if (all(list %in% dir(fromdir))){
                for (i in list){
                        file.copy(file.path(fromdir,i), todir)
                }
        } else {
                stop (paste("The file is not in the", fromdir))
        }
}
