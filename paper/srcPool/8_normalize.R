source('0_settings.R')

library(foreach)
library(iterators)
library(doParallel)

registerDoParallel(n_cpus)

library(rgdal)
library(rgeos)
library(stringr)
library(tools)

reprocess <- TRUE
overwrite <- TRUE

input_dir <- file.path(prefix, 'Landsat', 'Cloud_Filled')
output_dir <- file.path(prefix, 'Landsat', 'Cloud_Filled_Normalized')

# Function to auto-normalize an image list, or, if there is only one image in 
# the image list, to copy the image with the base image extension.
auto_normalize_or_copy <- function(image_files) {
    if (length(image_files) > 1) {
        auto_normalize(image_files, overwrite=overwrite)
    } else {
        base_img <- stack(image_files)
        mask_file <- paste0(file_path_sans_ext(image_files), '_masks', 
                            extension(image_files))
        base_mask <- stack(mask_file)

        # Copy the base image to a new file with the _base.tif extension
        base_copy_filename <- file.path(output_dir, 
                                        paste0(file_path_sans_ext(basename(image_files)), 
                                               '_normbase.tif'))
        base_img <- writeRaster(base_img, filename=base_copy_filename, 
                                datatype='INT2S',
                                overwrite=overwrite)
        base_img <- stack(image_files)
        base_mask_copy_filename <- file.path(output_dir,
                                             paste0(file_path_sans_ext(basename(image_files)), 
                                                    '_normbase_masks.tif'))
        base_mask <- writeRaster(base_mask, filename=base_mask_copy_filename, 
                                 datatype='INT2S',
                                 overwrite=overwrite)
    }
}

notify("Starting normalization.")
for (sitecode in sitecodes) {
    message(paste0('Normalizing images for ', sitecode, '...'))

    image_files <- dir(input_dir, 
                       pattern=paste0('^', sitecode, '_[0-9]{3}-[0-9]{3}_[0-9]{4}-[0-9]{3}_cf.tif$'),
                       full.names=TRUE)
    image_stacks <- lapply(image_files, stack)

    mask_files <- paste0(file_path_sans_ext(image_files), '_masks.tif')
    if (any(!file_test('-f', mask_files))) {
        stop('could not locate mask files')
    }
    mask_stacks <- lapply(mask_files, stack)

    output_files <- file.path(output_dir,
                              paste0(file_path_sans_ext(basename(image_files)), '_normalized.tif'))

    if (length(image_files) >= 1 & !reprocess) {
        image_files <- image_files[!file_test('-f', output_files)]
    }

    if (length(image_files) == 0) {
        next
    }

    # First figure out path/rows and dates
    wrspathrows <- gsub('[_]', '', str_extract(basename(image_files), 
                                                '_[0-9]{3}-[0-9]{3}_'))

    image_dates <- as.Date(str_extract(basename(image_files), 
                                       '_[0-9]{4}-[0-9]{3}_'), '_%Y-%j_')

    if (length(unique(wrspathrows)) == 1) {
        message(paste('1 path/row for', sitecode))
        # Simple case - only one path/row for this site
        auto_normalize_or_copy(image_files)
    } else {
        message(paste(length(unique(wrspathrows)), 'path/rows for', sitecode))
        # Handle more complicated case - multiple path/rows per site
        
        ######################################################################
        # First normalize largest path/row
        # Figure out largest path row
        img_sizes <- unlist(lapply(image_stacks, ncell))
        lg_pathrow <- wrspathrows[which(img_sizes == max(img_sizes))[1]]

        # Now normalize this path as normal
        lg_pathrow_imagefiles <- image_files[grepl(lg_pathrow, image_files)]

        auto_normalize_or_copy(lg_pathrow_imagefiles)

        # Determine the base image for lg_pathrow (it was automatically selected by 
        # auto_normalize)
        lg_base_file <- dir(input_dir, pattern=paste0('^', sitecode, '_', lg_pathrow, 
                                                      '_[0-9]{4}-[0-9]{3}_cf_normbase.tif$'),
                            full.names=TRUE)
        lg_mask_file <- paste0(file_path_sans_ext(lg_base_file), '_masks', 
                               extension(lg_base_file))

        remaining_wrspathrows <- wrspathrows[!grepl(lg_pathrow, wrspathrows)]
        remaining_wrspathrows <- unique(remaining_wrspathrows)

        ######################################################################
        # Normalize the remaining pathrows to this base image, using the area 
        # of overlap of each other path/row with this base image
        foreach(wrspathrow=iter(remaining_wrspathrows),
                .packages=c('stringr', 'teamlucc', 'lmodel2')) %do% {
                    
            raster_tmpdir <- file.path(temp, paste0('raster_',
                                                    paste(sample(c(letters, 0:9), 15), collapse='')))
            dir.create(raster_tmpdir)
            rasterOptions(tmpdir=raster_tmpdir)
                    
            these_image_files <- dir(input_dir,
                                     pattern=paste0('^', sitecode, '_', wrspathrow, 
                                                    '_[0-9]{4}-[0-9]{3}_cf.tif$'), 
                                     full.names=TRUE)

            base_extent <- as(extent(raster(lg_base_file)), 'SpatialPolygons')
            this_pathrow_extent <- as(extent(raster(these_image_files[[1]])), 'SpatialPolygons')

            overlap_area <- intersect(base_extent, this_pathrow_extent)

            base_crop <- crop(stack(lg_base_file), overlap_area)
            base_mask_crop <- crop(stack(lg_mask_file), overlap_area)

            ###################################################################
            # Normalize each image file within this path/row
            foreach(image_file=iter(these_image_files),
                    .packages=c('teamlucc', 'lmodel2')) %do% {
                mask_file <- paste0(file_path_sans_ext(image_file), '_masks', 
                                    extension(image_file))
                match_crop <- crop(stack(image_file), overlap_area)
                match_mask_crop <- crop(stack(mask_file), overlap_area)

                # Note that fmask layer is 2nd layer in stack
                crop_msk <- overlay(base_mask_crop[[2]], match_mask_crop[[2]],
                    fun=function(base, this) {
                    # Only use clear pixels when normalizing (0 in fmask)
                    (base != 0) & (this != 0)
                }, datatype=dataType(match_mask_crop))

                x <- base_crop
                y <- match_crop

                # Calculate normalization from extent overlapping base image
                if (500000 < ncell(x)) {
                    # Note that sampleRegular with cells=TRUE returns cell numbers in the 
                    # first column
                    x_vals <- sampleRegular(x, size=500000, cells=TRUE)
                    x_vals <- x_vals[!(crop_msk[x_vals[, 1]]), ]
                    y_vals <- y[x_vals[, 1]]
                    x_vals <- x_vals[, -1]
                } else {
                    x_vals <- getValues(x)
                    y_vals <- getValues(y)
                    x_vals <- x_vals[!getValues(crop_msk), ]
                    y_vals <- y_vals[!getValues(crop_msk), ]
                }

                names(y_vals) <- names(x_vals)

                # Develop model II regression from overlapping extent and normalize 
                # match image using this model
                normed_image <- foreach(unnormed_layer=unstack(stack(image_file)),
                                    x_sample=iter(x_vals, by='column'),
                                    y_sample=iter(y_vals, by='column'),
                                    .combine='addLayer', .multicombine=TRUE, 
                                    .init=raster(),
                                    .packages=c('raster', 'lmodel2', 'rgdal')) %dopar% {
                    model <- suppressMessages(lmodel2(x_sample ~ y_sample, nperm=0))
                    model <- model$regression.results[model$regression.results[, "Method"] == "MA", ]
                    names(model) <- gsub("^ *", "", names(model))
                    normed_layer <- model$Slope * unnormed_layer + model$Intercept
                }

                # Copy masked values back into the output raster. "match_mask" 
                # refers to missing values in the FULL match image, while 
                # crop_msk refers to missing values only in the area of the 
                # match image that overlaps the base image.
                #
                # Remember that fmask layer is 2nd layer in stack
                unnormed_image <- stack(image_file)
                match_mask <- stack(mask_file)
                normed_image[match_mask[[2]] != 0] <- unnormed_image[match_mask[[2]] != 0]

                output_normed_file <- file.path(output_dir, 
                                                paste0(file_path_sans_ext(basename(image_file)), 
                                                       '_normalized.tif'))
                output_normed_masks_file <- file.path(output_dir, 
                                                      paste0(file_path_sans_ext(basename(image_file)), 
                                                             '_normalized_masks.tif'))
                writeRaster(normed_image, filename=output_normed_file, 
                            datatype='INT2S', 
                            overwrite=overwrite)
                writeRaster(match_mask, filename=output_normed_masks_file, 
                            datatype='INT2S', overwrite=overwrite)
            }
            
            removeTmpFiles(h=0)
            unlink(raster_tmpdir)
            
            # # Normalize remainder of this path/row layerstack to this base image
            # base_image <- dir(input_dir, pattern=paste0('^[a-zA-Z]*_', wrspathrow, 
            #                                            '_[0-9]{4}-[0-9]{3}_cf_normbase.tif$'),
            #                   full.names=TRUE)
            # base_image_datestring <- str_extract(basename(base_image), '_[0-9]{4}-[0-9]{3}_')
            #
            # # Ensure the base image itself isn't included in the image_files list
            # image_files <- image_files[!(base_image_datestring %in% image_files)]
            # auto_normalize(image_files, base_image)
        }
    }
}

notify("Normalization completed.")
