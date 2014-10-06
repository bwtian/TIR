source("./tirSettings.R")
worklist  <- paste0(hkdMinCloud$ID,".tgz")
rs.todown(worklist, dir.tar, "todown.txt")
