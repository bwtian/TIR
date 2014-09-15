source('0_settings.R')

image_dir <- file.path(prefix, 'Landsat', 'Composites', 'Mosaics')
tiff_files <- dir(image_dir, pattern="*.tif$", full.names=TRUE)

verbose <- TRUE
verbose <- FALSE

pb <- pbCreate(length(tiff_files), 'text')
for (tiff_file in tiff_files) {
    pbStep(pb)
    if (file_test('-f', paste0(tiff_file, '.ovr'))) next
    cmd <- paste('gdaladdo -r AVERAGE -ro', tiff_file, '2 4 8 16 32 64')
    cmd_output <- system(cmd, intern=TRUE)
    if (verbose) print(cmd_output)
}
pbClose(pb)
