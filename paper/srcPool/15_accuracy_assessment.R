source('0_settings.R')

library(foreach)
library(doRNG)
library(iterators)
library(doParallel)

cl <- makeCluster(n_cpus)
registerDoParallel(cl)

library(rgdal)
library(stringr)
library(lubridate)
library(tools)

redo_accuracy_sampling <- FALSE
overwrite <- TRUE

zoi_folder <- file.path(prefix, 'TEAM', 'ZOIs')
predictions_dir <- file.path(prefix, 'Landsat', 'Composites', 'Predictions')
chgdetect_dir <- file.path(prefix, 'Landsat', 'Composites', 'Change_Detection')
out_dir <- file.path(prefix, 'Landsat', 'LCLUC_Accuracy_Assessment')

reg_exp <- 5
large_exp <- 1000
large_exp_sites <- c("COU", "CSN", "MAS", "YAS")

chgmag_files <- c()
zoi_files <- c()
classes_1_filenames <- c()
year_1s <- c()
year_2s <- c()
for (sitecode in sitecodes) {
    these_chgmag_files <- dir(chgdetect_dir,
                               pattern=paste0('^', sitecode, '_[0-9]{4}-[0-9]{4}_chgdetect_chgmag.tif$'),
                               full.names=TRUE)

    if (length(these_chgmag_files) == 0) {
        next
    }
    this_zoi_file <- dir(zoi_folder,
                         pattern=paste0('^ZOI_', sitecode, '_[0-9]{4}.RData'), 
                         full.names=TRUE)
    stopifnot(length(this_zoi_file) == 1)
    
    these_year_1s <- as.numeric(gsub('[_-]','', str_extract(these_chgmag_files, '_[0-9]{4}-')))
    these_year_2s <- as.numeric(gsub('[_-]','', str_extract(these_chgmag_files, '-[0-9]{4}_')))
    stopifnot(these_year_1s < these_year_2s)
    year_1s <- c(year_1s, these_year_1s)
    year_2s <- c(year_2s, these_year_2s)
    
    for (this_year_1 in these_year_1s) {
        classes_1_filename <- dir(predictions_dir,
                                  pattern=paste0(sitecode, '_mosaic_', this_year_1,
                                                 '_predictors_predclasses.tif$'),
                                  full.names=TRUE)
        stopifnot(length(classes_1_filename) == 1)
        classes_1_filenames <- c(classes_1_filenames, classes_1_filename)
    }
    
    chgmag_files <- c(chgmag_files, these_chgmag_files)
    
    zoi_files <- c(zoi_files, rep(this_zoi_file, length(these_chgmag_files)))
}

chgmag_files <- chgmag_files[year_2s == 2010]
zoi_files <- zoi_files[year_2s == 2010]
classes_1_filenames <- classes_1_filenames[year_2s == 2010]
year_1s <- year_1s[year_2s == 2010]
year_2s <- year_2s[year_2s == 2010]

stopifnot(length(chgmag_files) == length(zoi_files))
stopifnot(length(chgmag_files) == length(year_1s))
stopifnot(length(chgmag_files) == length(year_2s))
stopifnot(length(chgmag_files) == length(classes_1_filenames))

chgmag_file <- chgmag_files[5]
zoi_file <- zoi_files[5]
classes_1_filename <- classes_1_filenames[5]
year_1 <- year_1s[5]
year_2 <- year_2s[5]

# Set seed for repeatability
set.seed(463)

#Run change detection on each pair
notify(paste0('Calculating accuracy assessment polygons. ', 
              length(chgmag_files), ' image sets to process.'))
num_res <- foreach (chgmag_file=iter(chgmag_files), zoi_file=iter(zoi_files),
                    classes_1_filename=iter(classes_1_filenames),
                    year_1=iter(year_1s), year_2=iter(year_2s),
                    .packages=c('teamlucc', 'notifyR', 'stringr', 'rgdal'),
                    .combine=c, .inorder=FALSE) %dorng% {
    raster_tmpdir <- file.path(temp, paste0('raster_',
                               paste(sample(c(letters, 0:9), 15), collapse='')))
    dir.create(raster_tmpdir)
    rasterOptions(tmpdir=raster_tmpdir)

    sitecode <- str_extract(basename(chgmag_file), '^[a-zA-Z]*')

    out_basename <- paste0(sitecode, '_', year_1, '-', year_2, '_accpolys')
    output_files <- dir(out_dir, pattern=paste0(out_basename, '.shp'), 
                        full.names=TRUE)
    if (length(output_files) >= 1 & !redo_accuracy_sampling) {
        return()
    }

    chgmag_image <- raster(chgmag_file)
    classes_1_image <- raster(classes_1_filename)

    # Load ZOI for use in masking image
    load(zoi_file)
    zoi <- spTransform(zoi, CRS(proj4string(chgmag_image)))
    zoi_rast <- rasterize(zoi, chgmag_image, 1, silent=TRUE)

    # Mask out areas outside ZOI - we only care about accuracy w/in the ZOI.  
    chgmag_image[is.na(zoi_rast)] <- NA

    # And we can only evaluate accuracy where we have data for both years, so 
    # use the NAs in the chgmag_image to mask the classes_1_image.
    classes_1_image[is.na(chgmag_image)] <- NA

    key_file_1 <- gsub('predclasses.tif', 'classeskey.csv', classes_1_filename)
    class_key <- read.csv(key_file_1)

    if (sitecode %in% large_exp_sites) {
        expansion <- large_exp
    } else {
        expansion <- reg_exp
    }

    # Draw sample for assessing change/no-change
    training_polys <- sample_raster(classes_1_image, 30, 
                                    strata=classes_1_image, 
                                    exp=expansion,
                                    side=xres(classes_1_image))
    training_polys$year_1 <- ''
    training_polys$year_2 <- ''
    names(training_polys)[names(training_polys) == "year_1"] <- paste0("class_", year_1)
    names(training_polys)[names(training_polys) == "year_2"] <- paste0("class_", year_2)
    writeOGR(training_polys, out_dir, out_basename, driver="ESRI Shapefile")

    # Draw sample for assessing accuracy of last date classification
    
    # Draw sample for assessing accuracy of trajectories
}

notify(paste0('Finished calculating accuracy assessment polygons.'))
