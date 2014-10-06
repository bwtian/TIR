source("./tirSettings.R")
worklist  <- paste0(hkdMinCloud$ID,".tgz")
rs.untar(worklist, fromdir = dir.tar, todir = dir.tif)
