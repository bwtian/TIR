source('0_settings.R')

library(stringr)
library(tools)

library(foreach)
library(iterators)
library(doParallel)

registerDoParallel(n_cpus)

input_dir <- file.path(prefix, 'Landsat', 'Cloud_Filled')

image_files <- dir(input_dir, 
                   pattern=paste0('^[a-zA-Z]*_[0-9]{3}-[0-9]{3}_[0-9]{4}-[0-9]{3}_cf.tif$'), 
                   full.names=TRUE)

retvals <- foreach(image_file=iter(image_files),
                   .packages=c('teamlucc'), .inorder=FALSE) %dopar% {
    rasterOptions(tmpdir=paste0(tempdir(), '_raster'))
    jpeg(filename=paste0(file_path_sans_ext(image_file), '_browse.jpg'), 
         height=1000, width=1000, quality=75)
    browse_image(stack(image_file), DN_min=0, DN_max=1000)
    dev.off()
}
