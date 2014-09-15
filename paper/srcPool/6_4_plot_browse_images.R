source('0_settings.R')

library(tools)

library(foreach)
library(iterators)

overwrite <- TRUE

DN_min <- 0
DN_max <- 7000

maxpixels <- 500000
image_width <- 1200
image_height <- 1200

scene_topocorr_key <- read.csv('Scene_topocorr_key.csv')

for (sitecode in sitecodes) {
    message(paste0('Processing browse images for ', sitecode, '...'))
    base_dir <- file.path(prefix, 'Landsat', sitecode)
    image_dirs <- dir(base_dir,
                      pattern='^[0-9]{3}-[0-9]{3}_[0-9]{4}-[0-9]{3}_((LT[45])|(LE7))$', 
                      full.names=TRUE)

    total_dirs <- length(image_dirs)
    preprocessed <- image_dirs[unlist(lapply(image_dirs, is_preprocessed))]

    pathrow_re <- '[0-9]{3}-[0-9]{3}'
    date_re <- '[0-9]{4}-[0-9]{3}'
    sensor_re <- '((L[45]T)|(L[78]E))SR(_tc)?.tif$'
    image_files <- dir(image_dirs, pattern=paste(sitecode, pathrow_re, date_re, 
                                                 sensor_re, sep='_'), 
                       recursive=TRUE, full.name=TRUE)

    mask_files <- paste0(file_path_sans_ext(image_files), '_masks', 
                         extension(image_files))
    browse_files <- file.path(base_dir, 
                           paste0('browse_', file_path_sans_ext(basename(image_files)), 
                                  '.png'))

    image_file <- image_files[1]
    mask_file <- mask_files[1]
    browse_file <- browse_files[1]

    ret <- foreach (image_file=iter(image_files), mask_file=iter(mask_files), 
                    browse_file=iter(browse_files),
                    .packages=c('teamlucc')) %do% {
        rasterOptions(tmpdir=temp)

        if (file_test('-f', browse_file) & !overwrite) {
            return
        }

        image_stack <- stack(image_file, bands=as.integer(c(2:4)))
        mask_stack <- stack(mask_file, bands=as.integer(2))

        browse_image(image_stack, browse_file, mask_stack, DN_min=0, 
                     DN_max=7000, m_fun=function(vals) {vals == 2 | vals == 4})

    }
}
