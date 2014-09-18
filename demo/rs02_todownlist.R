
rs.todown()
worklist  <- paste0(hkdMinCloud$ID,".tgz")
dir.tar  <- "~/Share300sdb/Landsat8/N0/LISTEJapan"
rs.todown(worklist, dir.tar, "todown")
