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

redo_extract <- TRUE
redo_training <- TRUE
overwrite <- TRUE

ntree <- 501

predictor_names <- c('b1', 'b2', 'b3', 'b4', 'b5', 'b7', 'msavi', 
                     'msavi_glcm_mean', 'msavi_glcm_variance', 
                     'msavi_glcm_dissimilarity', 'elev', 'slope', 'aspect', 
                     'year')

tr_polys_dir <- file.path(prefix, 'Landsat', 'LCLUC_Training')
image_basedir <- file.path(prefix, 'Landsat', 'Composites', 'Predictors_5x5glcm')
model_outdir <- file.path(prefix, 'Landsat', 'Composites', 'Models')

notify('Starting training.')
for (sitecode in sitecodes) {
    raster_tmpdir <- file.path(temp, paste0('raster_',
                               paste(sample(c(letters, 0:9), 15), collapse='')))
    dir.create(raster_tmpdir)
    rasterOptions(tmpdir=raster_tmpdir)
    message(paste0('Processing ', sitecode, '...'))

    image_files <- dir(image_basedir,
                       pattern=paste0('^', sitecode, 
                                      '_mosaic_[0-9]{4}_predictors.tif$'))

    output_files <- paste0(file_path_sans_ext(image_files), '_classified.tif')

    if (length(image_files) >= 1 & !overwrite) {
        image_files <- image_files[!file_test('-f', output_files)]
    }

    if (length(image_files) == 0) {
        next
    }

    ##########################################################################
    # Read training data
    tr_pixels_file <- file.path(model_outdir, paste0(sitecode, '_trainingpixels.RData'))
    if (file_test('-f', tr_pixels_file) & !redo_extract) {
        load(tr_pixels_file)
    } else {
        tr_polys_file <- paste0(sitecode, '_Landsat_Training_Data.shp')
        if (!file_test('-f', file.path(tr_polys_dir, tr_polys_file))) {
            next
        }
        message('Reading training data...')
        tr_polys <- readOGR(tr_polys_dir, file_path_sans_ext(tr_polys_file))

        # rbind will fail when it tries to rbind a pixel_data object an
        # empty data.frame. And making an empty pixel_data object is difficult.  
        # So use the kludge of creating an rbind function that will ignore a 
        # data.frame and only rbind if y is a pixel_data object
        rbind_if_pixel_data <- function(x, y) {
            if(class(y) == 'pixel_data') {
                return(rbind(x, y))
            } else {
                return(x)
            }
        }
        beginCluster(n_cpus)
        tr_pixels <- foreach(image_file=iter(image_files), .combine=rbind_if_pixel_data,
                             .packages=c('teamlucc', 'rgdal', 'sp', 'maptools')) %do% {
            image_stack <- stack(file.path(image_basedir, image_file))
            image_year <- gsub('_', '', str_extract(image_file, '_[0-9]{4}_'))

            # # Drop the 13th layer (aspect) as it isn't all that useful for 
            # # these predictions.
            # image_stack <- dropLayer(image_stack, 13)

            # Add year as a predictor
            image_stack$year <- as.numeric(image_year)

            # Assign standardized layer names to input image so that different 
            # images can be used with the same model
            names(image_stack) <- predictor_names

            # Find which column has the indicator of whether or not this polygon is 
            # valid for this year.
            year_col_name <- paste0('Class_', image_year)
            year_col_index <- which(names(tr_polys) == year_col_name)
            tr_polys_indices <- which(!is.na(tr_polys@data[year_col_index]) &
                                      (tr_polys@data[year_col_index] != 'Unknown'))
            if (length(tr_polys_indices) == 0) {
                return(data.frame())
            }
            these_polys <- tr_polys[tr_polys_indices, ]
            these_polys <- spTransform(these_polys, CRS(proj4string(image_stack)))
            get_pixels(image_stack, these_polys, class_col=year_col_name, src=image_year)
        }
        endCluster()

        save(tr_pixels, file=tr_pixels_file)
    }

    ##########################################################################
    # Train classifier
    
    # Function to subsample any classes in a pixel_data object that have more 
    # than maxpix pixels
    subsample_classes <- function(x, maxpix=4000) {
        lg_classes <- levels(x)[table(x@y) > maxpix]
        training_flag(x, lg_classes) <- FALSE
        x <- subsample(x, maxpix, strata='classes', classes=lg_classes, 
                       type='testing')
        return(x)
    }

    if (redo_training) {
        model_file <- file.path(model_outdir, paste0(sitecode, '_rfmodel.RData'))
        message('Training classifier...')
        if (length(tr_pixels) > 30000) {
            set.seed(0)
            tr_pixels <- subsample_classes(tr_pixels)
        }
        model <- train_classifier(tr_pixels, ntree=ntree,
                                  factors=list(aspect=c(1, 2, 3, 4),
                                               year=c(1990, 1995, 2000, 2005, 2010)))
        save(model, file=model_file)
    }

    removeTmpFiles(h=0)
    unlink(raster_tmpdir)
}

notify('Training finished.')

stopCluster(cl)
