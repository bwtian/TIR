
rs.cptar <- function(list, fromdir, todir){
        todir <- dir.create(todir)
        if (match(list, dir(fromdir))){
                for (i in list){
                        file.copy(file.path(fromdir,i), todir)
                }
        } else {
                stop (paste("The file is not in the", fromdir))
        }
}
