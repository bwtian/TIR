library(raster)
library(tools)

library(imad)


data_dir <- 'O:/Data/Landsat/'

sitecode <- 'PSH'

inDataSet1_file <- file.path(data_dir, sitecode, 'PSH_mosaic_2000.tif')
inDataSet2_file <- file.path(data_dir, sitecode, 'PSH_mosaic_2010.tif')

inDataSet1 <- stack(inDataSet1_file)
inDataSet2 <- stack(inDataSet2_file)

mask1 <- stack(paste0(file_path_sans_ext(inDataSet1_file), '_masks', 
                      extension(inDataSet1_file)))
mask2 <- stack(paste0(file_path_sans_ext(inDataSet2_file), '_masks', 
                      extension(inDataSet2_file)))

mask1 <- mask1 != 0
mask2 <- mask1 != 0

output_basename <- paste0(file_path_sans_ext(inDataSet1_file), '_imad')

imad_res <- iMad(inDataSet1, inDataSet2, pos=as.integer(c(1:6)), mask1=mask1, 
                 mask2=mask2, output_basename=output_basename)

RADCAL(inDataSet1, inDataSet2, chisqr_raster)
