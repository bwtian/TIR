source('0_settings.R')

library(foreach)
library(iterators)
library(doParallel)

cl <- makeCluster(n_cpus)
registerDoParallel(cl)

library(rgdal)
library(stringr)
library(lubridate)
library(tools)

redo_classify <- TRUE
overwrite <- TRUE

predictor_names <- c('b1', 'b2', 'b3', 'b4', 'b5', 'b7', 'msavi', 
                     'msavi_glcm_mean', 'msavi_glcm_variance', 
                     'msavi_glcm_dissimilarity', 'elev', 'slope', 'aspect', 
                     'year')

zoi_folder <- file.path(prefix, 'TEAM', 'ZOIs')
image_basedir <- file.path(prefix, 'Landsat', 'Composites', 'Predictors_5x5glcm')
model_dir <- file.path(prefix, 'Landsat', 'Composites', 'Models')
out_dir <- file.path(prefix, 'Landsat', 'Composites', 'Predictions')

image_files <- c()
model_files <- c()
zoi_files <- c()
for (sitecode in sitecodes) {
    these_image_files <- dir(image_basedir,
                             pattern=paste0('^', sitecode, 
                                            '_mosaic_[0-9]{4}_predictors.tif$'))

    if (length(these_image_files) >= 1 & !redo_classify) {
        output_files <- paste0(file_path_sans_ext(these_image_files),
                               '_predclasses', extension(these_image_files))
        these_image_files <- these_image_files[!file_test('-f', output_files)]
    }

    if (length(these_image_files) == 0) {
        next
    }

    this_model_file <- file.path(model_dir,
                                 paste0(sitecode, '_rfmodel.RData'))
    if (!file_test('-f', this_model_file)) {
        next
    }
    
    this_zoi_file <- dir(zoi_folder,
                         pattern=paste0('^ZOI_', sitecode, '_[0-9]{4}.RData'), 
                         full.names=TRUE)
    stopifnot(length(this_zoi_file) == 1)

    image_files <- c(image_files, these_image_files)
    model_files <- c(model_files, rep(this_model_file, length(these_image_files)))
    zoi_files <- c(zoi_files, rep(this_zoi_file, length(these_image_files)))
}

stopifnot(length(image_files) == length(model_files))
stopifnot(length(image_files) == length(zoi_files))

notify(paste0('Starting classification. ',
              length(image_files), ' images to process.'))
num_res <- foreach (image_file=iter(image_files),
                    model_file=iter(model_files),
                    zoi_file=iter(zoi_files),
                    .packages=c('teamlucc', 'tools', 'stringr', 'notifyR', 'rgdal'),
                    .inorder=FALSE, .combine=c) %dopar% {
    raster_tmpdir <- file.path(temp, paste0('raster_',
                               paste(sample(c(letters, 0:9), 15), collapse='')))
    dir.create(raster_tmpdir)
    rasterOptions(tmpdir=raster_tmpdir)
    
    sitecode <- str_extract(basename(image_file), '^[a-zA-Z]{2,3}')
    year <- str_extract(image_file, '[0-9]{4}')

    load(model_file)

    image_stack <- stack(file.path(image_basedir, image_file))
    # Need to add a year layer since year is a predictor in the model
    image_stack$year <- as.numeric(year)
        
    fmask <- raster(file.path(image_basedir, 
                             paste0(file_path_sans_ext(image_file), 
                                    '_masks', extension(image_file))), 
                    layer=2)
    
    # Assign standardized layer names to input image so that different 
    # images can be used with the same model
    names(image_stack) <- predictor_names

    results <- classify(image_stack, model,
                        factors=list(aspect=c(1, 2, 3, 4),
                                     year=c(1990, 1995, 2000, 2005, 2010)))

    # Make a mask of 1s and NAs, with clear areas marked with 1s
    image_mask <- overlay(image_stack[[1]], fmask, fun=function(img, msk) {
        ret <- !is.na(img)
        ret[!ret] <- NA
        ret[(msk == 2) | (msk == 4) | (is.na(msk)) | (msk == 255)] <- NA
        return(ret)
    })
    
    classes <- results$classes * image_mask
    out_base <- file_path_sans_ext(file.path(out_dir, image_file))
    classes_file <- paste0(out_base, '_predclasses', extension(image_file))
    classes <- writeRaster(classes, filename=classes_file, 
                           datatype='INT2S', overwrite=overwrite)
    
    # Copy masks file so it is available with output predclasses images
    orig_masks_file <- file.path(image_basedir, 
                                 paste0(file_path_sans_ext(image_file), 
                                    '_masks', extension(image_file)))
    new_masks_file <- paste0(out_base, '_masks', extension(image_file))
    file.copy(orig_masks_file, new_masks_file)

    probs <- round(results$probs * 100)
    probs <- probs * image_mask
    probs_file <- paste0(out_base, '_predprobs', extension(image_file))
    probs <- writeRaster(probs, filename=probs_file, datatype='INT2S', 
                         overwrite=overwrite)

    key_file <- paste0(out_base, '_classeskey.csv')
    write.csv(results$codes, file=key_file, row.names=FALSE)

    # Calculate class frequencies, masking out area outside of ZOI
    load(zoi_file)
    zoi <- spTransform(zoi, CRS(proj4string(classes)))
    zoi <- rasterize(zoi, classes, 1, silent=TRUE)

    # Set masked areas to 99 so they can be differentiated. Don't use mask as 
    # it has a bug where it doesn't set NA areas in the image to the 
    # updatevalue
    classes_masked <- classes
    classes_masked[is.na(zoi)] <- 99

    class_freqs <- data.frame(freq(classes_masked))
    names(class_freqs) <- c('code', 'freq')
    class_freqs <- cbind(sitecode=sitecode, year=year,
                         name=results$codes$class[match(class_freqs$code, results$codes$code)],
                         class_freqs)
    write.csv(class_freqs, file=paste0(out_base, '_predclasses_classfreqs.csv'), row.names=FALSE)

    removeTmpFiles(h=0)
    unlink(raster_tmpdir)

    return(1)
}

if (length(num_res) == 0) num_res <- 0
notify(paste0('Classification finished. Classified ', sum(num_res), ' images.'))

stopCluster(cl)
