source('0_settings.R')

library(foreach)
library(iterators)
library(doParallel)

registerDoParallel(n_cpus)

library(rgeos)
library(stringr)
library(tools)

overwrite <- TRUE
reprocess <- TRUE

n_grey <- 64
window_size <- c(5, 5)
shift <- list(c(0,1), c(1,1), c(1,0), c(1,-1))

#imgtype <- 'normalized'
imgtype <- 'raw'

stopifnot(imgtype %in% c('normalized', 'raw'))

image_basedir <- file.path(prefix, 'Landsat', 'Composites', 'Mosaics')
#output_dir <- file.path(prefix, 'Landsat', 'Composites', 'Predictors')
output_dir <- file.path(prefix, 'Landsat', 'Composites', 'Predictors_5x5glcm')

image_files <- c()
dem_files <- c()
slopeaspect_files <- c()
for (sitecode in sitecodes) {
    if (imgtype == 'normalized') {
        pattern <- paste0('^', sitecode, '_mosaic_normalized_[0-9]{4}.tif$')
    } else {
        pattern <- paste0('^', sitecode, '_mosaic_[0-9]{4}.tif$')
    }
    these_image_files <- dir(image_basedir, pattern=pattern, full.names=TRUE)

    output_files <- file.path(output_dir,
                              paste0(file_path_sans_ext(basename(these_image_files)), 
                                     '_predictors.tif'))
    if (length(these_image_files) >= 1 & !reprocess) {
        these_image_files <- these_image_files[!file_test('-f', output_files)]
    }

    if (length(these_image_files) == 0) {
        next
    }

    dem_file <- file.path(image_basedir, paste0(sitecode, '_mosaic_dem.tif'))
    slopeaspect_file <- file.path(image_basedir, paste0(sitecode, 
                                                  '_mosaic_slopeaspect.tif'))

    image_files <- c(image_files, these_image_files)
    dem_files <- c(dem_files, rep(dem_file, length(these_image_files)))
    slopeaspect_files <- c(slopeaspect_files, rep(slopeaspect_file, 
                                                  length(these_image_files)))
}

stopifnot(length(image_files) == length(dem_files))
stopifnot(length(image_files) == length(slopeaspect_files))

notify(paste0('Calculating predictors. ',
              length(image_files), ' images to process.'))
foreach (image_file=iter(image_files), dem_file=iter(dem_files),
         slopeaspect_file=iter(slopeaspect_files),
         .packages=c('teamlucc', 'stringr')) %dopar% {
    raster_tmpdir <- file.path(temp, paste0('raster_',
                               paste(sample(c(letters, 0:9), 15), collapse='')))

    dir.create(raster_tmpdir)
    rasterOptions(tmpdir=raster_tmpdir)
    dem <- raster(dem_file)
    slopeaspect <- stack(slopeaspect_file)
    auto_calc_predictors(image_file, dem, slopeaspect, output_path=output_dir, 
                         overwrite=overwrite, window=window_size, 
                         n_grey=n_grey, shift=shift)

    removeTmpFiles(h=0)
    unlink(raster_tmpdir)
}
notify('Finished calculating predictors.')
