
tir.todown <- function(list, downeddir, tolistName){
        downedFiles  <- list.files(downeddir)
        todownList   <-  setdiff(list, downedFiles)
        Now <- format(Sys.time(), "_%y%m%d_%H%M%S")
        todownListName <- paste0(todownListName, Now, ".txt")
        write.table(todownList , todownListName, quote = F, row.names = F,col.names = F)
}
