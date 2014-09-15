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

redo_chg_detection <- TRUE
overwrite <- TRUE

chg_threshold_min <- 100

zoi_folder <- file.path(prefix, 'TEAM', 'ZOIs')
predictions_dir <- file.path(prefix, 'Landsat', 'Composites', 'Predictions')
chgdetect_dir <- file.path(prefix, 'Landsat', 'Composites', 'Change_Detection')
out_dir <- file.path(prefix, 'Landsat', 'Composites', 'Change_Detection')

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
stopifnot(length(chgmag_files) == length(zoi_files))
stopifnot(length(chgmag_files) == length(year_1s))
stopifnot(length(chgmag_files) == length(year_2s))
stopifnot(length(chgmag_files) == length(classes_1_filenames))


#zoi_file <- zoi_files[1]
#chgmag_file <- chgmag_files[1]
#classes_1_filename <- classes_1_filenames[1]
#year_1 <- year_1s[1]
#year_2 <- year_2s[1]

#Run change detection on each pair
notify(paste0('Calculating change trajectories. ', length(chgmag_files), ' image sets to process.'))
num_res <- foreach (chgmag_file=iter(chgmag_files), zoi_file=iter(zoi_files),
                    classes_1_filename=iter(classes_1_filenames),
                    year_1=iter(year_1s), year_2=iter(year_2s),
                    .packages=c('teamlucc', 'notifyR', 'stringr', 'rgdal'),
                    .combine=c, .inorder=FALSE) %dopar% {
    raster_tmpdir <- file.path(temp, paste0('raster_',
                               paste(sample(c(letters, 0:9), 15), collapse='')))
    dir.create(raster_tmpdir)
    rasterOptions(tmpdir=raster_tmpdir)

    sitecode <- str_extract(basename(chgmag_file), '^[a-zA-Z]*')

    out_basename <- paste0(sitecode, '_', year_1, '-', year_2, '_chgdetect')

    output_files <- dir(out_dir, pattern=paste0(out_basename, '_chgtraj.tif'), 
                        full.names=TRUE)
    if (length(output_files) >= 1 & !redo_chg_detection) {
        return()
    }

    chgmag_image <- raster(chgmag_file)

    chgdir_filename <- file.path(out_dir,
                                  paste(out_basename, 'chgdir.tif', 
                                        sep='_'))
    chgdir_image <- raster(chgdir_filename)

    # Load ZOI for use in masking images
    load(zoi_file)
    zoi <- spTransform(zoi, CRS(proj4string(chgmag_image)))
    zoi_rast <- rasterize(zoi, chgmag_image, 1, silent=TRUE)

    chgmag_image_crop <- crop(chgmag_image, zoi)
    zoi_rast_crop <- crop(zoi_rast, zoi)
    chgmag_image_crop[is.na(zoi_rast_crop)] <- NA
    chg_threshold_auto <- threshold(chgmag_image_crop)

    if (chg_threshold_auto < chg_threshold_min) {
        chg_threshold <- chg_threshold_min
    } else {
        chg_threshold <- chg_threshold_auto
    }
    
    chg_threshold_filename <- file.path(out_dir,
                                        paste(out_basename, 'threshold.txt', 
                                              sep='_'))
    write.csv(data.frame(sitecode=sitecode, year_1=year_1, year_2=year_2, 
                         threshold=chg_threshold,
                         threshold_auto=chg_threshold_auto),
              file=chg_threshold_filename, 
              row.names=FALSE)

    key_file_1 <- gsub('predclasses.tif', 'classeskey.csv', classes_1_filename)
    class_key <- read.csv(key_file_1)

    chg_traj_out <- chg_traj(chgmag_image, chgdir_image, 
                             chg_threshold=chg_threshold)

    chg_traj_filename <- file.path(out_dir, paste(out_basename, 'chgtraj.tif', 
                                                  sep='_'))
    # Recheck - but below masking line should no longer be needed
    #chg_traj_image <- chg_traj_out$traj * image_mask
    chg_traj_image <- writeRaster(chg_traj_out, 
                                  filename=chg_traj_filename, 
                                  overwrite=overwrite, datatype='INT2S')

    chg_traj_lut_filename <- file.path(out_dir,
                                       paste(out_basename, 'chgtraj_lut.csv', 
                                             sep='_'))
    lut <- traj_lut(class_key$code, class_key$class)
    write.csv(lut, file=chg_traj_lut_filename, row.names=FALSE)

    # Set masked areas to 99 so they can be differentiated. Don't use mask as 
    # it has a bug where it doesn't set NA areas in the image to the 
    # updatevalue
    chg_traj_image_masked <- chg_traj_image
    chg_traj_image_masked[is.na(zoi_rast)] <- 99

    traj_freqs <- data.frame(freq(chg_traj_image_masked))
    chg_freqs <- lut
    chg_freqs$freq <- traj_freqs$count[match(chg_freqs$Code, traj_freqs$value)]
    chg_freqs <- chg_freqs[order(chg_freqs$t0_name, chg_freqs$t1_name),]
    freqs_filename <- file.path(out_dir, paste(out_basename, 
                                               'chgtraj_freqs.csv', sep='_'))
    write.csv(chg_freqs, file=freqs_filename, row.names=FALSE)

    removeTmpFiles(h=0)
    unlink(raster_tmpdir)

    return(1)
}

if (length(num_res) == 0) num_res <- 0
notify(paste0('Finished calculating change trajectories. Processed ', sum(num_res), ' images.'))

stopCluster(cl)
