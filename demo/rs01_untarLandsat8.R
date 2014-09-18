
worklist  <- paste0(hkdMinCloud$ID,".tgz")
dir.tar  <- "~/Share300sdb/Landsat8/N0/LISTEJapan"
dir.tif  <- "~/Share500sda/Landsat8/at0_Sensor"
rs.untar(worklist, fromdir = dir.tar, todir = dir.tif)
