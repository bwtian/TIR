#!/bin/bash

cd /localdisk/home/azvoleff/Code/teamlucc

git pull
sed -i 's/dplyr/plyr/' ./DESCRIPTION
sed -i '/dplyr/d' ./NAMESPACE
R -e 'library(devtools);install(".")'
###############################################################################
# Load packages (do not modify these lines)
library(teamlucc)
library(notifyR)

source('notify.R')

###############################################################################
# General settings (update as necessary)

sites <- read.csv('Site_Code_Key.csv')
sitecodes <- sites$Site.Name.Code
sitecodes <- c("BIF", "CAX", "COU", "CSN",
               "MAS", "PSH", "RNF", "VB",
               "YAN", "YAS", "BCI", "BBS",
               "UDZ", "NAK")

PLOT_WIDTH <- 6.5
PLOT_HEIGHT <- 6.5
PLOT_DPI <- 300

prefixes <- c('D:/azvoleff/Data', # CI-TEAM
              'H:/Data', # Buffalo drive
              'O:/Data', # Blue drive
              '/localdisk/home/azvoleff/Data') # vertica1
prefix <- prefixes[match(TRUE, unlist(lapply(prefixes, function(x) file_test('-d', x))))]

temps <- c('H:/Temp', # Buffalo drive
           'O:/Temp', # Blue drive (HP or thinkpad)
           '/localdisk/home/azvoleff/Temp', # vertica1
           'D:/Temp') # CI-TEAM
temp <- temps[match(TRUE, unlist(lapply(temps, function(x) file_test('-d', x))))]
rasterOptions(tmpdir=temp)

# Specify how many processors to use for parallel processing. On CI-TEAM, this 
# should be set to 6. On your laptop, set it somewhere between 2 and 4.
if (Sys.info()[4] == 'CI-TEAM') {
    n_cpus <- 8
} else if (Sys.info()[4] == 'vertica1.team.sdsc.edu') {
    n_cpus <- 16
} else {
    n_cpus <- 3
}

# Should any existing output files be overwritten as the script runs? If set to 
# FALSE, and there ARE existing files for earlier runs of the script, the 
# script will raise an error and stop running.
overwrite <- TRUE

espa_email <- 'azvoleff@gmail.com'

crop_to_aoi <- TRUE
verbose <- TRUE

# Load the DEM extents needed for the auto_setup_dem function
load('dem_extents.RData')
dem_path <- file.path(prefix, 'CGIAR_SRTM', 'Tiles')
dem_extents$filename <- gsub('H:\\\\Data\\\\CGIAR_SRTM', dem_path, dem_extents$filename)
dem_extents$filename <- gsub('\\\\', '/', dem_extents$filename)

###############################################################################
# Function to check if there are preprocessed files in an image folder
is_preprocessed <- function(image_dir) {
    pathrow_re <- '[0-9]{3}-[0-9]{3}'
    date_re <- '[0-9]{4}-[0-9]{3}'
    sensor_re <- '((L[45]T)|(L[78]E))SR(_tc)?.tif$'
    preprocessed_files <- dir(image_dir,
                              pattern=paste(sitecode, pathrow_re, date_re, 
                                            sensor_re, sep='_'))
    if (length(preprocessed_files) >= 1) {
        return(TRUE)
    } else {
        return(FALSE)
    }
}

class_names_pretty <- c('Urban/built',
                        'Agriculture',
                        'Plantation forest',
                        'Natural forest',
                        'Other vegetation',
                        'Bare',
                        'Water',
                        'Unknown')

class_names_R <- c('Urban.built',
                   'Agriculture',
                   'Plantation.forest',
                   'Natural.forest',
                   'Other.vegetation',
                   'Bare',
                   'Water',
                   'Unknown')

class_names_abbrev <- c('Urban',
                        'Ag',
                        'PlanFor',
                        'NatFor',
                        'OthVeg',
                        'Bare',
                        'Water',
                        'Unk')

class_colors <- c('#CC0000',
                  '#F3F781',
                  '#3366FF',
                  '#088A08',
                  '#82FA58',
                  '#DBA901',
                  '#58D3F7',
                  '#A4A4A4')

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
library(stringr)

new_sitecodes <- c('BBS',
                   'KRP',
                   'MAS',
                   'NNN',
                   'YAN',
                   'YAS')
base_dir <- "H:/Data/Landsat/LCLUC_Training"
base_qgs_file <- file.path(base_dir, "BCI_Landsat_Training_Map.qgs")
base_shp_files <- dir(base_dir, pattern="BCI_Landsat_Training_Data")

for (new_sitecode in new_sitecodes) {
    new_shp_files <- gsub('BCI', new_sitecode, base_shp_files)
    new_qgs_file <- gsub('BCI', new_sitecode, base_qgs_file)

    stopifnot(all(!file_test('-f', c(new_shp_files, new_qgs_file))))

    file.copy(file.path(base_dir, base_shp_files),
              file.path(base_dir, new_shp_files))

    f_in <- file(base_qgs_file, 'r')
    base_qgs_file_text <- readLines(f_in)
    close(f_in)

    new_qgs_file_text <- gsub('BCI', new_sitecode, base_qgs_file_text)

    f_out <- file(new_qgs_file, 'w')
    writeLines(new_qgs_file_text, f_out)
    close(f_out)
}
source('0_settings.R')

library(stringr)
library(tools)
library(foreach)
library(iterators)
library(dplyr)
library(reshape2)

overwrite <- TRUE

###############################################################################
# Summarize cloud fills and normalized data
get_status <- function(pattern, name) {
    statuses <- foreach(sitecode=iter(sitecodes), .combine=rbind) %do% {
        base_dir <- file.path(prefix, 'Landsat', sitecode)
        these_files <- dir(base_dir, pattern=pattern)
        if (length(these_files) >= 1) {
            these_paths <- gsub('[_-]', '', str_extract(these_files, '_[0-9]{3}-'))
            these_rows <- gsub('[_-]', '', str_extract(these_files, '-[0-9]{3}_'))
            these_dates <- gsub('[_]', '', str_extract(these_files, '_[0-9]{4}-[0-9]{3}'))
            statuses <- data.frame(site=sitecode, pathrow=these_paths, 
                                   path=these_paths, row=these_rows, 
                                   date=these_dates, this_status=TRUE)
        } else {
            statuses <- data.frame()
        }
        return(statuses)
    }
    names(statuses)[names(statuses) == 'this_status'] <- name
    return(statuses)
}

cf_status <- get_status('^[a-zA-Z]*_[0-9]{3}-[0-9]{3}_[0-9]{4}-[0-9]{3}_cf.tif$', 'cf')
cf_mask_status <- get_status('^[a-zA-Z]*_[0-9]{3}-[0-9]{3}_[0-9]{4}-[0-9]{3}_cf_masks.tif$', 'cf_mask')
norm_status <- get_status('^[a-zA-Z]*_[0-9]{3}-[0-9]{3}_[0-9]{4}-[0-9]{3}_cf(_((normbase)|(normalized))).tif$', 'normed')
norm_mask_status <- get_status('^[a-zA-Z]*_[0-9]{3}-[0-9]{3}_[0-9]{4}-[0-9]{3}_cf(_((normbase)|(normalized)))_masks.tif$', 'normed_mask')
statuses <- merge(cf_status, cf_mask_status, all=TRUE)
statuses <- merge(statuses, norm_status, all=TRUE)
statuses <- merge(statuses, norm_mask_status, all=TRUE)

statuses <- statuses[with(statuses, order(site, path, row, date)), ]
statuses

imgs_by_pathrow <- summarize(group_by(statuses, site,
                                      pathrow=paste(path, row, sep='-')),
                             num_imgs=length(!is.na(cf)))

imgs_by_site <- summarize(group_by(imgs_by_pathrow, site),
                          num_pathrows=length(num_imgs),
                          mean_num_imgs=mean(num_imgs))
imgs_by_site[order(imgs_by_site$mean_num_imgs), ]

summarize(group_by(statuses, site, pathrow=paste(path, row, sep='-')), 
          num_imgs=length(!is.na(cf)))

statuses[statuses$site == 'UDZ', ]
statuses[statuses$site == 'BBS', ]

###############################################################################
# Summarize mosaics
get_mosaic_status <- function(pattern, name) {
    statuses <- foreach(sitecode=iter(sitecodes), .combine=rbind) %do% {
        base_dir <- file.path(prefix, 'Landsat', sitecode)
        these_files <- dir(base_dir, pattern=pattern)
        if (length(these_files) >= 1) {
            these_dates <- gsub('[_]', '', str_extract(these_files, '_[0-9]{4}'))
            statuses <- data.frame(site=sitecode, date=these_dates, 
                                   this_status=TRUE)
        } else {
            statuses <- data.frame()
        }
        return(statuses)
    }
    names(statuses)[names(statuses) == 'this_status'] <- name
    return(statuses)
}

mosaic_raw_status <- get_mosaic_status('^[a-zA-Z]*_mosaic_[0-9]{4}.tif$', 'mosaic_raw')
mosaic_norm_status <- get_mosaic_status('^[a-zA-Z]*_mosaic_normalized_[0-9]{4}.tif$', 'mosaic_norm')
mosaic_statuses <- merge(mosaic_raw_status, mosaic_norm_status, all=TRUE)
mosaic_statuses

summarize(group_by(mosaic_statuses, site), 
          num_raw_imgs=length(!is.na(mosaic_raw)), 
          num_norm_imgs=length(!is.na(mosaic_norm)))

dcast(mosaic_statuses, site ~ date, value.var='mosaic_raw')
dcast(mosaic_statuses, site ~ date, value.var='mosaic_norm')

###############################################################################
# Summarize DEM mosaics
get_dem_status <- function(pattern, name) {
    statuses <- foreach(sitecode=iter(sitecodes), .combine=rbind) %do% {
        base_dir <- file.path(prefix, 'Landsat', sitecode)
        these_files <- dir(base_dir, pattern=pattern)
        if (length(these_files) >= 1) {
            statuses <- data.frame(site=sitecode, this_status=TRUE)
        } else {
            statuses <- data.frame()
        }
        return(statuses)
    }
    names(statuses)[names(statuses) == 'this_status'] <- name
    return(statuses)
}

mosaic_dem_status <- get_dem_status('^[a-zA-Z]*_mosaic_dem.tif$', 'mosaic_dem')
mosaic_slopeaspect_status <- get_dem_status('^[a-zA-Z]*_mosaic_slopeaspect.tif$', 'mosaic_slpasp')
mosaic_dem_statuses <- merge(mosaic_dem_status, mosaic_slopeaspect_status, all=TRUE)
mosaic_dem_statuses 

###############################################################################
# Summarize training data
get_train_status <- function(pattern, name) {
    base_dir <- file.path(prefix, 'TEAM', 'LCLUC_Training')
    these_files <- dir(base_dir, pattern=pattern)
    if (length(these_files) >= 1) {
        sitecodes <- gsub('[_]', '', str_extract(these_files, '^[a-zA-Z]*_'))
        statuses <- data.frame(site=sitecodes, this_status=TRUE)
    } else {
        statuses <- data.frame()
    }
    names(statuses)[names(statuses) == 'this_status'] <- name
    return(statuses)
}
train_shp_status <- get_train_status('^[a-zA-Z]*_Landsat_Training_Data.shp$', 'shp')
train_map_status <- get_train_status('^[a-zA-Z]*_Landsat_Training_Map.qgs$', 'map')
train_statuses <- merge(train_shp_status, train_map_status, all=TRUE)
train_statuses

        
dir(file.path(prefix, 'Landsat'), pattern='^[a-zA-Z]*_training_pixels.RData$', 
    recursive=TRUE)

dir(file.path(prefix, 'Landsat'), pattern='^[a-zA-Z]*_rfmodel.RData$',
    recursive=TRUE)
source('0_settings.R')

library(stringr)
library(gridExtra) # for unit
library(tools)
library(dplyr)
library(reshape2)

library(ggplot2)

library(foreach)
library(iterators)
library(doParallel)

library(scales) # for percent format

registerDoParallel(n_cpus)

overwrite <- TRUE

zoi_folder <- file.path(prefix, 'TEAM', 'ZOIs')
image_basedir <- file.path(prefix, 'Landsat', 'Composites', 'Mosaics')

reprocess <- FALSE
imgtype <- 'raw'

###############################################################################
# Summarize mosaics
mosaic_stats_RData_file <- 'mosaic_pixel_stats.RData'
if (reprocess) {
    stopifnot(imgtype %in% c('normalized', 'raw'))
    if (imgtype == 'normalized') {
        pattern <- '^[a-zA-Z]*_mosaic_normalized_[0-9]{4}.tif$'
    } else {
        pattern <- '^[a-zA-Z]*_mosaic_[0-9]{4}.tif$'
    }
    mosaic_files <- dir(image_basedir, pattern=pattern, full.names=TRUE, recursive=TRUE)

    sitecodes <- str_extract(basename(mosaic_files), '^[a-zA-Z]*')

    mosaic_stats <- foreach(mosaic_file=iter(mosaic_files),
                            .packages=c('raster', 'tools', 'stringr'),
                            .combine=rbind) %dopar% {
        fmask <- raster(paste0(file_path_sans_ext(mosaic_file), '_masks', 
                                   extension(mosaic_file)),
                        band=2)
        sitecode <- str_extract(basename(mosaic_file), '^[a-zA-Z]*')
        year <- as.numeric(str_extract(basename(mosaic_file), '[0-9]{4}'))

        # Mask out area outside of ZOI
        zoi_file <- dir(zoi_folder, pattern=paste0('^ZOI_', sitecode, '_[0-9]{4}.RData'), 
                        full.names=TRUE)
        stopifnot(length(zoi_file) == 1)
        load(zoi_file)
        zoi <- spTransform(zoi, CRS(proj4string(fmask)))

        # Set masked areas to 99 so they can be differentiated from other NAs 
        # in fmask.  Don't use mask as it has a bug where it doesn't set NA 
        # areas in the image to the updatevalue
        zoi <- rasterize(zoi, fmask, 1, silent=TRUE)
        fmask[is.na(zoi)] <- 99

        bs <- blockSize(fmask)
        num_fill <- 0
        num_cloud <- 0
        num_clear <- 0
        num_water <- 0
        num_snow <- 0
        num_missing <- 0
        for (block_num in 1:bs$n) {
            fmask_bl <- getValuesBlock(fmask, row=bs$row[block_num], 
                                       nrows=bs$nrows[block_num])
            num_missing <- num_missing + sum(is.na(fmask_bl))
            num_fill <- num_fill + sum(fmask_bl == 255, na.rm=TRUE)
            num_cloud <- num_cloud + sum(fmask_bl == 2, na.rm=TRUE) + sum(fmask_bl == 4, na.rm=TRUE)
            num_clear <- num_clear + sum(fmask_bl == 0, na.rm=TRUE)
            num_water <- num_water + sum(fmask_bl == 1, na.rm=TRUE)
            num_snow <- num_snow + sum(fmask_bl == 3, na.rm=TRUE)
        }
        num_pixels <- num_fill + num_cloud + num_clear + num_water + num_snow + num_missing
        return(data.frame(site=sitecode, date=year, num_fill=num_fill, 
                          num_missing=num_missing, num_cloud=num_cloud, 
                          num_clear=num_clear, num_water=num_water, 
                          num_snow=num_snow, total_pixels=num_pixels))
    }

    # Add rows with num_fill equal to the number of pixels in the whole image for 
    # sites without any data for a particular year.  Do this with a merge.
    miss_data <- data.frame(site=rep(unique(mosaic_stats$site), each=5))
    miss_data$date <- rep(unique(mosaic_stats$date), length.out=nrow(miss_data))
    miss_data$num_fill <- mosaic_stats$total_pixels[match(miss_data$site, 
                                                          mosaic_stats$site)]
    miss_data$total_pixels <- miss_data$num_fill
    miss_data <- miss_data[!(paste(miss_data$site, miss_data$date) %in% 
                             paste(mosaic_stats$site, mosaic_stats$date)), ]

    stopifnot(sum(is.na(mosaic_stats)) == 0)
    mosaic_stats <- merge(mosaic_stats, miss_data, all=TRUE)
    mosaic_stats[is.na(mosaic_stats)] <- 0
    save(mosaic_stats, file=mosaic_stats_RData_file)
}

load(mosaic_stats_RData_file)

clmiss_summary <- summarize(group_by(mosaic_stats, site, date),
                             pct_cloud=num_cloud / (num_fill + num_missing + num_clear + num_water + num_cloud + num_snow),
                             pct_missing=(num_fill + num_missing) / (num_fill + num_missing + num_clear + num_water + num_cloud + num_snow))
#NaN results from mosaicks with ALL data missing
clmiss_summary$pct_cloud[is.nan(clmiss_summary$pct_cloud)] <- 0
filter(clmiss_summary, pct_cloud > 1)
filter(clmiss_summary, pct_cloud > 5)
filter(clmiss_summary, pct_missing > 5)

ggplot(clmiss_summary) +
    geom_line(aes(date, pct_cloud, colour=site, linetype=site)) +
    geom_point(aes(date, pct_cloud, colour=site, shape=site)) +
    scale_colour_manual("Site", values=rep(1:4, each=4, length.out=16)) +
    scale_shape_manual("Site", values=rep(1:4, length.out=16)) +
    scale_linetype_manual("Site", values=rep(1:4, length.out=16)) +
    xlab("Epoch") + ylab("Percent clouded (of ZOI)")

ggplot(clmiss_summary) +
    geom_line(aes(date, pct_missing, colour=site, linetype=site)) +
    geom_point(aes(date, pct_missing, colour=site, shape=site)) +
    scale_colour_manual("Site", values=rep(1:4, each=4, length.out=16)) +
    scale_shape_manual("Site", values=rep(1:4, length.out=16)) +
    scale_linetype_manual("Site", values=rep(1:4, length.out=16)) +
    xlab("Epoch") + ylab("Percent missing (of ZOI)")

ggplot(clmiss_summary) +
    geom_line(aes(date, pct_cloud)) +
    geom_point(aes(date, pct_cloud)) +
    facet_wrap(~site) +
    xlab("Epoch") + ylab("Percent clouded (of ZOI)")

ggplot(clmiss_summary) +
    geom_line(aes(date, pct_missing)) +
    geom_point(aes(date, pct_missing)) +
    facet_wrap(~site) +
    xlab("Epoch") + ylab("Percent missing (of ZOI)")

missing_melt <- melt(clmiss_summary, id.vars=c('site', 'date'))
missing_melt$variable <- factor(missing_melt$variable, levels=c('pct_cloud', 'pct_missing'), labels=c('Clouded', 'Missing'))
ggplot(missing_melt) +
    geom_line(aes(date, value, colour=variable, linetype=variable)) +
    geom_point(aes(date, value, colour=variable, linetype=variable, shape=variable)) +
    labs(colour="Type of data",
         shape="Type of data",
         linetype="Type of data") +
    facet_wrap(~site) +
    theme(legend.key.size=unit(1.5, "line"),
          panel.grid.major=element_blank()) +
    scale_y_continuous(labels=percent_format()) +
    xlab("Epoch") + ylab("Percent of ZOI") +
    theme(axis.text.x = element_text(angle=45, hjust=1))
ggsave('missing_data_summary_lines.png', width=10, height=7.5, dpi=300)

ggplot(missing_melt) +
    geom_bar(aes(date, value, fill=variable), stat="identity") +
    labs(colour="Type of data",
         shape="Type of data",
         linetype="Type of data") +
    facet_wrap(~site) +
    scale_y_continuous(labels=percent_format()) +
    theme(legend.key.size=unit(1.5, "line"),
          panel.grid.major=element_blank()) +
    xlab("Epoch") + ylab("Percent of ZOI") +
    theme(axis.text.x = element_text(angle=45, hjust=1))
ggsave('missing_data_summary_bars.png', width=10, height=7.5, dpi=300)

miss_summary <- summarize(group_by(mosaic_stats, site, date),
                             pct_missing=(num_cloud + num_fill + num_missing) / (num_fill + num_missing + num_clear + num_water + num_cloud + num_snow))
cloud_wide_table <- dcast(miss_summary, site ~ date)
cloud_wide_table[2:ncol(cloud_wide_table)] <- round(cloud_wide_table[2:ncol(cloud_wide_table)], 2)
write.csv(cloud_wide_table, file='mosaic_pixel_miss_summary.csv', row.names=FALSE)

###############################################################################
# Summarize DEM mosaics
#
# - Min elevation
# - Max elevation
# - Mean elevation
# - Mean slope
# - Max slope
get_dem_status <- function(pattern, name) {
    statuses <- foreach(sitecode=iter(sitecodes), .combine=rbind) %do% {
        these_files <- dir(image_basedir, pattern=pattern)
        if (length(these_files) >= 1) {
            statuses <- data.frame(site=sitecode, this_status=TRUE)
        } else {
            statuses <- data.frame()
        }
        return(statuses)
    }
    names(statuses)[names(statuses) == 'this_status'] <- name
    return(statuses)
}

mosaic_dem_status <- get_dem_status('^[a-zA-Z]*_mosaic_dem.tif$', 'mosaic_dem')
mosaic_slopeaspect_status <- get_dem_status('^[a-zA-Z]*_mosaic_slopeaspect.tif$', 'mosaic_slpasp')
mosaic_dem_statuses <- merge(mosaic_dem_status, mosaic_slopeaspect_status, all=TRUE)
mosaic_dem_statuses 
source('0_settings.R')

library(stringr)
library(tools)
library(dplyr)
library(reshape2)

training_pixel_files <- dir(file.path(prefix, 'Landsat', 'Composites', 'Models'),
                            pattern='trainingpixels.RData$', full.names=TRUE)

out_file <- 'training_pixel_summary.txt'

sink(out_file)
for (training_pixel_file in training_pixel_files) {
    cat('*******************************************************************************\n')
    cat(training_pixel_file)
    cat('\n')
    load(training_pixel_file)
    print(summary(tr_pixels))
    cat('\n')
}
sink()
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

zoi_folder <- file.path(prefix, 'TEAM', 'ZOIs')
image_basedir <- file.path(prefix, 'Landsat', 'Composites', 'Predictions')
out_dir <- file.path(prefix, 'Landsat', 'Composites', 'Change_Detection')

classes_file_1s <- c()
classes_file_2s <- c()
for (sitecode in sitecodes) {
    these_classes_files <- dir(image_basedir,
                               pattern=paste0('^', sitecode, 
                                              '_mosaic_[0-9]{4}_predictors_predclasses.tif$'),
                               full.names=TRUE)

    if (length(these_classes_files) == 0) {
        next
    }
    if (length(these_classes_files) == 1) {
        message(paste("cannot process", sitecode, "- only 1 classes file found"))
        next
    }

    # Setup pairs of image files
    these_classes_file_1s <- these_classes_files[1:(length(these_classes_files) - 1)]
    these_classes_file_2s <- these_classes_files[2:length(these_classes_files)]

    classes_file_1s <- c(classes_file_1s, these_classes_file_1s)
    classes_file_2s <- c(classes_file_2s, these_classes_file_2s)
}
stopifnot(length(classes_file_1s) == length(classes_file_2s))

# Run chg magnitude/direction calculation on each pair
notify(paste0('Starting chg magnitude/direction calculation. ',
              length(classes_file_1s), ' image sets to process.'))
num_res <- foreach (classes_file_1=iter(classes_file_1s), 
                    classes_file_2=iter(classes_file_2s),
                    .packages=c('teamlucc', 'notifyR', 'stringr', 'rgdal'),
                    .combine=c, .inorder=FALSE) %dopar% {
    raster_tmpdir <- file.path(temp, paste0('raster_',
                               paste(sample(c(letters, 0:9), 15), collapse='')))
    dir.create(raster_tmpdir)
    rasterOptions(tmpdir=raster_tmpdir)

    sitecode <- str_extract(basename(classes_file_1), '^[a-zA-Z]*')

    year_1 <- as.numeric(gsub('_','', str_extract(classes_file_1,
                                                  '_[0-9]{4}_')))
    year_2 <- as.numeric(gsub('_','', str_extract(classes_file_2,
                                                  '_[0-9]{4}_')))
    stopifnot(year_1 < year_2)

    out_basename <- paste0(sitecode, '_', year_1, '-', year_2, 
                           '_chgdetect')

    output_files <- dir(out_dir, pattern=paste0(out_basename, '_chgdir.tif'), 
                        full.names=TRUE)
    if (length(output_files) >= 1 & !redo_chg_detection) {
        return()
    }

    mask_file_1 <- gsub('predclasses', 'masks', classes_file_1)
    mask_file_2 <- gsub('predclasses', 'masks', classes_file_2)
    probs_file_1 <- gsub('predclasses', 'predprobs', classes_file_1)
    probs_file_2 <- gsub('predclasses', 'predprobs', classes_file_2)

    key_file_1 <- gsub('predclasses.tif', 'classeskey.csv', classes_file_1)
    key_file_2 <- gsub('predclasses.tif', 'classeskey.csv', classes_file_2)
    stopifnot(read.csv(key_file_1) == read.csv(key_file_2))
    class_key <- read.csv(key_file_1)
    classnames <- class_key$class

    # Make a mask of 1s and NAs, with clear areas marked with 1s. This needs to 
    # take into account BOTH images.
    mask_1 <- raster(mask_file_1, layer=2)
    mask_2 <- raster(mask_file_2, layer=2)
    image_mask <- overlay(mask_1, mask_2, fun=function(msk1, msk2) {
        ret <- !is.na(msk1) & !is.na(msk2)
        ret[(msk1 == 2) | (msk1 == 4) | (is.na(msk1)) | (msk1 == 255)] <- NA
        ret[(msk2 == 2) | (msk2 == 4) | (is.na(msk2)) | (msk2 == 255)] <- NA
        return(ret)
    })

    t1_classes <- stack(classes_file_1)
    t1_probs <- stack(probs_file_1)
    t2_probs <- stack(probs_file_2)

    chg_dir_filename <- file.path(out_dir,
                                  paste(out_basename, 'chgdir.tif', 
                                        sep='_'))
    chg_dir_image <- chg_dir(t1_probs, t2_probs)
    chg_dir_image <- chg_dir_image * image_mask
    writeRaster(chg_dir_image, filename=chg_dir_filename, 
                overwrite=overwrite, datatype=dataType(chg_dir_image))

    chg_mag_filename <- file.path(out_dir,
                                  paste(out_basename, 'chgmag.tif', 
                                        sep='_'))
    chg_mag_image <- chg_mag(t1_probs, t2_probs)
    chg_mag_image <- chg_mag_image * image_mask
    writeRaster(chg_mag_image, filename=chg_mag_filename, 
                overwrite=overwrite, datatype=dataType(chg_mag_image))

    removeTmpFiles(h=0)
    unlink(raster_tmpdir)

    return(1)
}

if (length(num_res) == 0) num_res <- 0
notify(paste0('Finished chg magnitude/direction calculation. Processed ', sum(num_res), ' images.'))

stopCluster(cl)
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
source('0_settings.R')

library(ggplot2)

img_width <- 10
img_height <- 7.5
img_dpi <- 300

library(foreach)
library(iterators)
library(doParallel)

cl <- makeCluster(n_cpus)
registerDoParallel(cl)

library(rgdal)
library(stringr)

zoi_folder <- file.path(prefix, 'TEAM', 'ZOIs')
chgdetect_dir <- file.path(prefix, 'Landsat', 'Composites', 'Change_Detection')
out_dir <- "."

chgmag_files <- c()
zoi_files <- c()
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
    
    chgmag_files <- c(chgmag_files, these_chgmag_files)
    
    zoi_files <- c(zoi_files, rep(this_zoi_file, length(these_chgmag_files)))
}
stopifnot(length(chgmag_files) == length(zoi_files))
stopifnot(length(chgmag_files) == length(year_1s))
stopifnot(length(chgmag_files) == length(year_2s))

# Count the number of pixels in the ZOI that have good data and that are 
# missing
notify(paste0('Counting ZOI cells. ', length(chgmag_files), ' images to process.'))
zoi_pix <- foreach (chgmag_file=iter(chgmag_files), zoi_file=iter(zoi_files),
                    year_1=iter(year_1s), year_2=iter(year_2s),
                    .packages=c('teamlucc', 'stringr', 'rgdal'),
                    .combine=rbind, .inorder=FALSE) %dopar% {
    raster_tmpdir <- file.path(temp, paste0('raster_',
                               paste(sample(c(letters, 0:9), 15), collapse='')))
    dir.create(raster_tmpdir)
    rasterOptions(tmpdir=raster_tmpdir)

    sitecode <- str_extract(basename(chgmag_file), '^[a-zA-Z]*')

    chgmag <- raster(chgmag_file)

    # Load ZOI for use in masking images
    load(zoi_file)
    zoi <- spTransform(zoi, CRS(proj4string(chgmag)))
    chgmag <- crop(chgmag, zoi)

    zoi_rast <- rasterize(zoi, chgmag, 1, silent=TRUE)

    # Recode areas outside ZOI as -1 so NAs outside ZOI are not counted towards 
    # the missing data count. After the below code, chgmag is coded as:
    #   -1 : outside ZOI
    #    0 : good data inside ZOI
    #    1 : bad data inside ZOI
    chgmag[!is.na(chgmag)] <- 0
    chgmag[is.na(chgmag)] <- 1
    chgmag[is.na(zoi_rast)] <- -1

    n_pix <- cellStats(chgmag != -1, 'sum')
    n_good <- cellStats(chgmag == 0, 'sum')
    n_NA <- cellStats(chgmag == 1, 'sum')
    return(data.frame(sitecode=sitecode, t0=year_1, t1=year_2,
                      pixtype=c("Total", "Invalid", "Valid"),
                      n=c(n_pix, n_NA, n_good)))
}

# Ensure sitecodes are in alphabetical order prior to ordering zoi_pix
zoi_pix$sitecode <- factor(as.character(zoi_pix$sitecode))
zoi_pix <- zoi_pix[order(zoi_pix$sitecode, zoi_pix$t0), ]

save(zoi_pix, file="zoi_n_pix_change.RData")

zoi_pix$period <- paste(zoi_pix$t0, zoi_pix$t1, sep=" -> ")
ggplot(zoi_pix[zoi_pix$pixtype!="Total", ], aes(period, n, fill=pixtype)) +
    geom_bar(stat="identity", position="dodge") +
    facet_wrap(~sitecode, scales="free_y") + scale_fill_discrete("Type") +
    xlab("Period") + ylab("Number of pixels")
ggsave('zoi_n_pix_change.png', height=img_height, width=img_width, dpi=img_dpi)

if (length(zoi_pix) == 0) {
    num_res <- 0
} else {
    num_res <- nrow(zoi_pix)
}
notify(paste0('Finished counting ZOI cells. ', num_res, ' images processed.'))
stopCluster(cl)
source('0_settings.R')

library(foreach)
library(itertools)

library(ggplot2)
library(gridExtra) # for unit
library(dplyr)
library(reshape2)
library(stringr)

library(scales) # for percent format

img_width <- 10
img_height <- 7.5
img_dpi <- 300

load("zoi_n_pix_change.RData")

traj_freqs_dir <- file.path(prefix, 'Landsat', 'Composites', 'Change_Detection')

traj_freqs_files <- dir(traj_freqs_dir,
                        pattern='^[a-zA-Z]*_[0-9]{4}-[0-9]{4}_chgdetect_chgtraj_freqs.csv$')
traj_freqs <- foreach(traj_freqs_file=iter(traj_freqs_files),
                 .packages=c('ggplot2'),
                 .combine=rbind, .inorder=FALSE) %do% {
    sitecode <- str_extract(basename(traj_freqs_file), '^[a-zA-Z]*')
    time_string <- str_extract(traj_freqs_file, '[0-9]{4}-[0-9]{4}')
    t0 <- as.numeric(gsub('-', '', str_extract(time_string, '[0-9]{4}-')))
    t1 <- as.numeric(gsub('-', '', str_extract(time_string, '-[0-9]{4}')))
    traj_freqs <- read.csv(file.path(traj_freqs_dir, traj_freqs_file))
    traj_freqs$t0_name <- ordered(traj_freqs$t0_name, levels=class_names_R, 
                                  labels=class_names_pretty)
    traj_freqs$t1_name <- ordered(traj_freqs$t1_name, levels=class_names_R, 
                                  labels=class_names_pretty)
    traj_freqs$t0_name_abbrev <- ordered(class_names_abbrev[match(traj_freqs$t0_name, 
                                                             class_names_pretty)], 
                                         levels=class_names_abbrev)
    traj_freqs$t1_name_abbrev <- ordered(class_names_abbrev[match(traj_freqs$t1_name, 
                                                             class_names_pretty)], 
                                         levels=class_names_abbrev)
    traj_freqs$Transition <- paste(traj_freqs$t0_name_abbrev, 
                                   traj_freqs$t1_name_abbrev, sep=' -> ')
    traj_freqs <- cbind(sitecode=sitecode, t0=t0, t1=t1, nyears=t1-t0, 
                        traj_freqs)
    return(traj_freqs)
}

traj_freqs <- tbl_df(traj_freqs)
traj_freqs$period <- paste(traj_freqs$t0, traj_freqs$t1, sep=" -> ")

traj_freqs_site_period <- paste(traj_freqs$sitecode, traj_freqs$period)
valid_zoi_pix <- zoi_pix[zoi_pix$pixtype == "Valid", ]
valid_zoi_pix_site_period <- paste(valid_zoi_pix$sitecode, valid_zoi_pix$period)
traj_freqs$freq_as_frac <- traj_freqs$freq/valid_zoi_pix$n[match(traj_freqs_site_period, valid_zoi_pix_site_period)]

write.csv(traj_freqs, file="traj_freqs.csv", row.names=FALSE)
save(traj_freqs, file="traj_freqs.RData")

preds_basedir <- file.path(prefix, 'Landsat', 'Composites', 'Predictions')
class_freqs_files <- dir(preds_basedir,
                         pattern='^[a-zA-Z]*_mosaic_[0-9]{4}_predictors_predclasses_classfreqs.csv$')
class_freqs <- foreach(class_freqs_file=iter(class_freqs_files),
                       .packages=c('ggplot2'),
                       .combine=rbind, .inorder=FALSE) %do% {
    class_freqs <- read.csv(file.path(preds_basedir, class_freqs_file), stringsAsFactors=FALSE)
    class_freqs$name[is.na(class_freqs$code)] <- 'Unknown'
    class_freqs$code[class_freqs$name == 'Unknown'] <- '-1'
    # Ignore areas outside ZOI (areas coded 99)
    class_freqs <- class_freqs[!(class_freqs$code == 99), ]
    class_freqs$name <- ordered(class_freqs$name, levels=class_names_R, 
                                labels=class_names_pretty)
    class_freqs$name_abbrev <- ordered(class_names_abbrev[match(class_freqs$name, 
                                                             class_names_pretty)], 
                                       levels=class_names_abbrev)

    return(class_freqs)
}

write.csv(class_freqs, file="class_freqs.csv", row.names=FALSE)
save(class_freqs, file="class_freqs.RData")

# TODO: integrate number of cells in ZOI per image into calculation so that 
# results are normalized.

for (sitecode in unique(traj_freqs$sitecode)) {
    site_traj_freqs <- traj_freqs[traj_freqs$sitecode == sitecode, ]
    # persist <- summarize(group_by(site_traj_freqs, t0, t0_name),
    #                      pct=paste0(round(freq[t0_name == t1_name] / sum(freq), 2)),
    #                      t0_name_abbrev=t0_name_abbrev[1])
    # freqs_zeroed_persist <- site_traj_freqs
    # freqs_zeroed_persist$freq[freqs_zeroed_persist$t0_name == freqs_zeroed_persist$t1_name] <- 0

    ggplot(site_traj_freqs) +
        theme_bw() +
        geom_tile(aes(x=t1_name_abbrev, y=t0_name_abbrev, fill=freq/sum(freq)), colour='black') +
        #geom_text(aes(x=t0_name_abbrev, y=t0_name_abbrev, label=pct), 
        #data=persist) +
        scale_fill_gradientn('Relative\nFrequency', limits=c(0, .25),
                             colours=c('white', 'orange', 'red')) +
        xlab('Class in time 1') + ylab('Class in time 0') +
        theme(axis.text.y=element_text(angle=90, hjust=.5),
              legend.key.size=unit(1.5, "line"),
              panel.grid.major=element_blank()) +
        facet_wrap(~ t0)
    ggsave(file.path(traj_freqs_dir, paste('transitions', sitecode, 
                                           'colorplot.png', sep='_')),
           height=img_height, width=img_width, dpi=img_dpi)
}

classes <- data.frame(label=class_names_pretty,
                      color=class_colors,
                      stringsAsFactors=FALSE)

# Plot trajectory frequencies by site
ggplot(traj_freqs) +
    geom_bar(aes(t0, freq_as_frac, fill=t0_name), stat="identity", position="dodge") +
    facet_wrap(~sitecode) + 
    scale_fill_manual("Time 0 Cover", values=classes$color, breaks=classes$label,
                      labels=classes$label, drop=FALSE) +
    xlab("Start of period") +
    ylab("Fraction of all pixels") +
    ggtitle("Time 0")
ggsave(file.path(traj_freqs_dir, 
                 'transition_frequencies_all_sites_normalized_time0.png'),
       height=img_height, width=img_width, dpi=img_dpi)

# Plot trajectory frequencies by site
ggplot(traj_freqs) +
    geom_bar(aes(t1, freq_as_frac, fill=t1_name), stat="identity", position="dodge") +
    facet_wrap(~sitecode) + 
    scale_fill_manual("Time 1 Cover", values=classes$color, breaks=classes$label,
                      labels=classes$label, drop=FALSE) +
    xlab("End of period") +
    ylab("Fraction of all pixels") +
    ggtitle("Time 1")
ggsave(file.path(traj_freqs_dir, 
                 'transition_frequencies_all_sites_normalized_time1.png'),
       height=img_height, width=img_width, dpi=img_dpi)

# Plot percentage of pixels changing over time
class_freqs <- group_by(class_freqs, sitecode, year)
class_freqs <- mutate(class_freqs, pct=freq/sum(freq[!is.na(name)]))
ggplot(class_freqs) +
    geom_line(aes(year, pct, colour=name, linetype=name)) +
    geom_point(aes(year, pct, colour=name, shape=name)) + 
    scale_colour_manual("Site", values=rep(1:4, length.out=16)) +
    scale_shape_manual("Site", values=rep(1:4, length.out=16)) +
    scale_linetype_manual("Site", values=rep(1:4, each=4, length.out=16)) +
    theme_grey(base_size=18) +
    facet_wrap(~ sitecode) +
    labs(linetype="Class", colour="Class", shape="Class") +
    xlab('Year') + ylab('Percent of Landscape') +
    ylim(c(0, 100)) +
    theme(axis.text.y=element_text(angle=90, hjust=.5),
          legend.key.size=unit(1.5, "line"),
          panel.grid.major=element_blank()) +
    scale_y_continuous(labels=percent_format())
ggsave(file.path(preds_basedir, 'class_frequencies_all_sites.png'),
       height=img_height, width=img_width, dpi=img_dpi)
source('0_settings.R')

library(foreach)
library(itertools)
library(doParallel)

cl <- makeCluster(n_cpus)
registerDoParallel(cl)

library(rgdal)
library(raster)
library(ggplot2)
library(gridExtra)
library(plyr)
library(grid)
library(stringr)
library(tools)

img_width <- 10
img_height <- 7.5
img_dpi <- 300
type <- "cairo"

overwrite <- TRUE

zoi_folder <- file.path(prefix, 'TEAM', 'ZOIs')

image_basedir <- file.path(prefix, 'Landsat', 'Composites', 'Change_Detection')
out_dir <- image_basedir

stopifnot(file_test('-d', out_dir))

# chgtraj_file <- chgtraj_files[1]
# chgtraj_lut_file <- chgtraj_lut_files[1]

#' Function to plot classified image for a given year
#'
#' @export
#' @import rgdal
#' @import ggplot2
#' @importFrom plyr join
#' @importFrom grid unit
#' @importFrom sp spTransform CRS proj4string
#' @param x a forest change raster layer (a single layer of the layer 
#' stack output by \code{\link{annual_stack}}
#' @param aoi one or more AOI polygons as a \code{SpatialPolygonsDataFrame} 
#' object.  If there is a 'label' field  in the dataframe, it will be used to 
#' label the polygons in the plots. If the AOI is not in WGS 1984 (EPSG:4326), 
#' it will be reprojected to WGS84.
#' @param classes a \code{data.frame} with "code", "label", and (optionally) 
#' "color" columns. The "code" column indicates the numeric code in the image 
#' referring to a particular category or cover type, "label" indicates the 
#' label to use for each code, and "color" indicates the color to use on the 
#' image. "color" must be specified in a format recognized by \code{ggplot2}.
#' @param title_string the plot title
#' @param size_scale a number used to scale the size of the plot text
#' @param maxpixels the maximum number of pixels from x to use in plotting
plot_trajs <- function(x, aoi, classes, title_string='', size_scale=1, 
                       maxpixels=2e6, legend_title="Change type") {
    aoi_tr <- spTransform(aoi, CRS(proj4string(x)))
    aoi_tr$ID <- row.names(aoi_tr)
    if (!('label' %in% names(aoi_tr))) {
        aoi_tr$label <- paste('AOI', seq(1:nrow(aoi_tr@data)))
    }
    aoi_tr@data$id <- rownames(aoi_tr@data)
    aoi_points <- fortify(aoi_tr, region="id")
    aoi_df <- join(aoi_points, aoi_tr@data, by="id")

    if (ncell(x) > maxpixels) {
        x <- sampleRegular(x, maxpixels, asRaster=TRUE, useGDAL=TRUE)
    }
    dat <- as.data.frame(x, xy=TRUE)
    names(dat)[3] <- 'value'
    dat$value <- ordered(dat$value, levels=classes$code)

    if (!"color" %in% names(classes)) {
        gg_color_hue <- function(n) {
            # From: http://bit.ly/1yXZOlv
            hues = seq(15, 375, length=n+1)
            hcl(h=hues, l=65, c=100)[1:n]
        }
        classes$color <- gg_color_hue(nrow(classes))
    }

    long=lat=value=label=ID=NULL # For R CMD CHECK
    ggplot(dat) +
        geom_raster(aes(x, y, fill=value)) + coord_fixed() + 
        scale_fill_manual(legend_title, values=classes$color, breaks=classes$code,
                          labels=classes$label, drop=FALSE) +
        theme(axis.text.x=element_blank(), axis.text.y=element_blank(),
              axis.title.x=element_blank(), axis.title.y=element_blank(),
              panel.background=element_blank(), panel.border=element_blank(),
              panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
              plot.background=element_blank(), axis.ticks=element_blank(),
              plot.margin=unit(c(.1, .1, .1, .1), 'cm')) +
        geom_path(aes(long, lat, group=ID), color="black", data=aoi_df, size=.7, alpha=.7) +
        ggtitle(title_string)
}

notify(paste0('Plotting chgdetect browse images. ', length(sitecodes), ' sites to process.'))
for (sitecode in sitecodes) {
    chgtraj_files <- dir(image_basedir,
                         pattern=paste0(sitecode, '_[0-9]{4}-[0-9]{4}_chgdetect_chgtraj.tif$'),
                         full.names=TRUE)
    chgtraj_lut_files <- dir(image_basedir,
                             pattern=paste0(sitecode, '_[0-9]{4}-[0-9]{4}_chgdetect_chgtraj_lut.csv$'),
                             full.names=TRUE)
    stopifnot(length(chgtraj_files) == length(chgtraj_lut_files))

    plot_titles <- str_extract(chgtraj_files, "[0-9]{4}-[0-9]{4}")

    zoi_file <- dir(zoi_folder, pattern=paste0('^ZOI_', sitecode, '_[0-9]{4}.RData'), 
                    full.names=TRUE)
    stopifnot(length(zoi_file) == 1)
    load(zoi_file)

    chgtraj_rasts <- foreach (chgtraj_file=iter(chgtraj_files), 
                        chgtraj_lut_file=iter(chgtraj_lut_files),
                        .packages=c('stringr', 'tools', 'raster', 'plyr', 'grid', 
                                    'rgdal', 'ggplot2')) %dopar% {
        sitecode <- str_extract(basename(chgtraj_file), '^[a-zA-Z]*')
        year <- str_extract(chgtraj_file, '[0-9]{4}')
        chgtraj_rast <- raster(chgtraj_file)
        chgtraj_rast <- sampleRegular(chgtraj_rast, 1e6, asRaster=TRUE, 
                                      useGDAL=TRUE)
        # Mask out area outside of ZOI
        zoi <- spTransform(zoi, CRS(proj4string(chgtraj_rast)))
        chgtraj_rast <- crop(chgtraj_rast, zoi)
        zoi_rast <- rasterize(zoi, chgtraj_rast, 1, silent=TRUE)
        chgtraj_rast[is.na(zoi_rast)] <- NA
        chgtraj_rast
    }

    traj_code_dfs <- foreach (chgtraj_lut_file=iter(chgtraj_lut_files)) %dopar% {
        traj_codes <- read.csv(chgtraj_lut_file)
        traj_codes$t0_name_abbrev <- class_names_abbrev[match(traj_codes$t0_name, class_names_R)]
        traj_codes$t1_name_abbrev <- class_names_abbrev[match(traj_codes$t1_name, class_names_R)]
        traj_codes$trans_name <- with(traj_codes, paste(t0_name_abbrev, t1_name_abbrev, sep=' -> '))
        traj_codes
    }

    make_traj_plot <- function(chg_rast, traj_codes, classes, 
                               gain_legend, loss_legend,
                               plot_titles, plot_file) {
        plots <- foreach (chgtraj_rast=iter(chgtraj_rasts),
                          traj_codes=iter(traj_code_dfs),
                          plot_title=iter(plot_titles),
                          .export=c("plot_trajs", "zoi")) %dopar% {
            # Make forest/non-forest transition image
            traj_codes$Change <- NA
            traj_codes$Change[with(traj_codes, (t0_name != classes) & (t1_name == classes))] <- 1
            traj_codes$Change[with(traj_codes, (t0_name == classes) & (t1_name != classes))] <- 2
            classes_subs <- data.frame(from=traj_codes$Code[!is.na(traj_codes$Change)],
                                       to=traj_codes$Change[!is.na(traj_codes$Change)])
            Change <- subs(chgtraj_rast, classes_subs)
            if (cellStats(is.na(Change), "sum") != ncell(Change)) {
                this_plot <- plot_trajs(Change, zoi,
                                        classes=data.frame(code=c(1, 2),
                                                           label=c(gain_legend, loss_legend),
                                                           color=c("#33FF33", "#FF3333"),
                                                           stringsAsFactors=FALSE),
                                        title_string=plot_title)
            } else {
                this_plot <- c()
            }
            this_plot
        }

        plots <- plots[unlist(lapply(plots, function(x) !is.null(x)))]

        if (length(plots) > 0) {
            ggsave(file.path(out_dir, plot_file),
                   plot=do.call(arrangeGrob, plots),
                   width=img_width, height=img_height, dpi=img_dpi, type=type)
        }

    }

    # classes <- c("Natural.forest")
    # gain_legend <- "Forest gain"
    # loss_legend <- "Forest loss"
    # plot_title <- plot_titles
    # plot_file <- paste0(sitecode, "_natforest_change.png")

    make_traj_plot(chg_rast, traj_codes, c("Natural.forest"),
                   "Forest gain", "Forest loss", plot_titles,
                    paste0(sitecode, "_natforest_change.png"))

    #
    #classes <- c("Plantation.forest")
    # "To plantation"
    # "From plantation"
    # plot_titles
    # paste0(sitecode, "_planforest_change.png")

    make_traj_plot(chg_rast, traj_codes, c("Plantation.forest"),
                   "To plantation", "From plantation", plot_titles, 
                    paste0(sitecode, "_planforest_change.png"))

    make_traj_plot(chg_rast, traj_codes, c("Agriculture", "Plantation.forest"),
                   "To agriculture", "From agriculture", plot_titles,
                   paste0(sitecode, "_ag_change.png"))
}

stopCluster(cl)

notify(paste0('Finished plotting chgdetect browse images.'))
source('0_settings.R')

library(foreach)
library(itertools)
library(doParallel)

cl <- makeCluster(n_cpus)
registerDoParallel(cl)

library(rgdal)
library(raster)
library(ggplot2)
library(gridExtra)
library(plyr)
library(grid)
library(stringr)
library(tools)

img_width <- 10
img_height <- 7.5
img_dpi <- 300
type <- "cairo"

overwrite <- TRUE

zoi_folder <- file.path(prefix, 'TEAM', 'ZOIs')

image_basedir <- file.path(prefix, 'Landsat', 'Composites', 'Predictions')
out_dir <- image_basedir

stopifnot(file_test('-d', out_dir))

#' Function to plot classified image for a given year
#'
#' @export
#' @import rgdal
#' @import ggplot2
#' @importFrom plyr join
#' @importFrom grid unit
#' @importFrom sp spTransform CRS proj4string
#' @param x a forest change raster layer (a single layer of the layer 
#' stack output by \code{\link{annual_stack}}
#' @param aoi one or more AOI polygons as a \code{SpatialPolygonsDataFrame} 
#' object.  If there is a 'label' field  in the dataframe, it will be used to 
#' label the polygons in the plots. If the AOI is not in WGS 1984 (EPSG:4326), 
#' it will be reprojected to WGS84.
#' @param classes a \code{data.frame} with "code", "label", and (optionally) 
#' "color" columns. The "code" column indicates the numeric code in the image 
#' referring to a particular category or cover type, "label" indicates the 
#' label to use for each code, and "color" indicates the color to use on the 
#' image. "color" must be specified in a format recognized by \code{ggplot2}.
#' @param title_string the plot title
#' @param size_scale a number used to scale the size of the plot text
#' @param maxpixels the maximum number of pixels from x to use in plotting
plot_classes <- function(x, aoi, classes, title_string='', size_scale=1, 
                         maxpixels=1e6) {
    aoi_tr <- spTransform(aoi, CRS(proj4string(x)))
    aoi_tr$ID <- row.names(aoi_tr)
    if (!('label' %in% names(aoi_tr))) {
        aoi_tr$label <- paste('AOI', seq(1:nrow(aoi_tr@data)))
    }
    aoi_tr@data$id <- rownames(aoi_tr@data)
    aoi_points <- fortify(aoi_tr, region="id")
    aoi_df <- join(aoi_points, aoi_tr@data, by="id")

    if (ncell(x) > maxpixels) {
        x <- sampleRegular(x, maxpixels, asRaster=TRUE, useGDAL=TRUE)
    }
    dat <- as.data.frame(x, xy=TRUE)
    names(dat)[3] <- 'value'
    dat$value <- ordered(dat$value, levels=classes$code)

    long=lat=value=label=ID=NULL # For R CMD CHECK
    ggplot(dat) +
        geom_raster(aes(x, y, fill=value)) + coord_fixed() + 
        scale_fill_manual("Cover", values=classes$color, breaks=classes$code,
                          labels=classes$label, drop=FALSE) +
        theme(axis.text.x=element_blank(), axis.text.y=element_blank(),
              axis.title.x=element_blank(), axis.title.y=element_blank(),
              panel.background=element_blank(), panel.border=element_blank(),
              panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
              plot.background=element_blank(), axis.ticks=element_blank(),
              plot.margin=unit(c(.1, .1, .1, .1), 'cm')) +
        geom_path(aes(long, lat, group=ID), color="black", data=aoi_df, size=.7, alpha=.7) +
        ggtitle(title_string)
}

notify(paste0('Plotting classified browse images. ', length(sitecodes), ' sites to process.'))
for (sitecode in sitecodes) {
    predclasses_files <- dir(image_basedir,
                             pattern=paste0(sitecode, '_mosaic_[0-9]{4}_predictors_predclasses.tif$'),
                             full.names=TRUE)
    classeskey_files <- dir(image_basedir,
                            pattern=paste0(sitecode, '_mosaic_[0-9]{4}_predictors_classeskey.csv$'),
                            full.names=TRUE)

    stopifnot(length(classeskey_files) == length(predclasses_files))

    # Mask out area outside of ZOI
    zoi_file <- dir(zoi_folder, pattern=paste0('^ZOI_', sitecode, 
                                               '_[0-9]{4}.RData'), 
                    full.names=TRUE)
    stopifnot(length(zoi_file) == 1)
    load(zoi_file)
    zoi <- spTransform(zoi, CRS(proj4string(stack(predclasses_files[1]))))

    plots <- foreach (predclasses_file=iter(predclasses_files),
                      classeskey_file=iter(classeskey_files),
                      .packages=c('stringr', 'tools', 'raster', 'plyr', 'grid', 
                                  'rgdal', 'ggplot2')) %dopar% {
            year <- str_extract(predclasses_file, '[0-9]{4}')

            classeskey <- read.csv(classeskey_file)
            classes <- data.frame(code=NA,
                                  label=class_names_abbrev,
                                  color=class_colors,
                                  stringsAsFactors=FALSE)
            # Assign codes to classes that are in this image. Assign codes to the 
            # others just so the plot code doesn't choke, and so these classes remain 
            # in the legend, even if they do not appear in the image.
            #
            # First fill in proper codes for classes that do appear in image
            classes_rows_match <- match(classeskey$class, class_names_R)
            classes[classes_rows_match, ]$code <- classeskey$code
            # Now fill in random codes for classes that don't appear in the image
            classes_rows_nas <- is.na(classes$code)
            classes[classes_rows_nas, ]$code <- seq(100, length.out=sum(classes_rows_nas))


            classes_rast <- stack(predclasses_file)
            classes_rast <- sampleRegular(classes_rast, 1e6, asRaster=TRUE, 
                                          useGDAL=TRUE)
            classes_rast <- crop(classes_rast, zoi)
            zoi_rast <- rasterize(zoi, classes_rast, 1, silent=TRUE)
            classes_rast[is.na(zoi_rast)] <- NA


            title_string <- paste(sitecode, year, sep=' - ')
            plot_classes(classes_rast, zoi, classes, title_string)
    }

    # Below is from: http://bit.ly/1qhIgOh
    
    # For all one page:
    ggsave(file.path(out_dir, paste0(sitecode, "_classified_browse.png")), 
           plot=do.call(arrangeGrob, plots),
           width=img_width, height=img_height, dpi=img_dpi, type=type)

    # # For gridded 2x2
    # ggsave(file.path(out_dir, paste0(sitecode, "_classified_browse.png")), 
    #        do.call(marrangeGrob, c(l, list(nrow=2, ncol=2))),
    #        width=img_width, height=img_height, dpi=img_dpi, type=type)
}

stopCluster(cl)

notify(paste0('Finished plotting classified browse images.'))
source('0_settings.R')

library(foreach)
library(itertools)

library(raster)

library(ggplot2)
library(dplyr)

chgdetect_dir <- file.path(prefix, 'Landsat', 'Composites', 'Change_Detection')
out_dir <- chgdetect_dir

thresholds_files <- dir(chgdetect_dir,
                        pattern='^[a-zA-Z]*_[0-9]{4}-[0-9]{4}_chgdetect_threshold.txt$',
                        full.names=TRUE)
thresholds <- foreach(thresholds_file=iter(thresholds_files),
                      .combine=rbind, .inorder=FALSE) %do% {
    read.csv(thresholds_file)
}

summarize(group_by(thresholds, sitecode), 
          min=min(threshold),
          max=max(threshold),
          mean=mean(threshold),
          range=diff(range(threshold)))

ggplot(thresholds, aes(year_1, threshold, colour=sitecode)) + geom_line()

write.csv(thresholds, file='thresholds.csv', row.names=FALSE)

# chgmag_files <- dir(chgdetect_dir,
#                     pattern='^[a-zA-Z]*_[0-9]{4}-[0-9]{4}_chgdetect_chgmag.tif$',
#                     full.names=TRUE)
# chgmag_hists <- foreach(chgmag_file=iter(chgmag_files),
#                         .packages=c('ggplot2', 'dplyr'),
#                         .combine=rbind, .inorder=FALSE) %do% {
#
#     chgmag <- raster(chgmag_file)
#     
#     vals <- sampleRegular(chgmag, 100000, useGDAL=TRUE)
#     hist(vals)
#     hiu
#     sitecode <- str_extract(basename(chgmag_file), '^[a-zA-Z]*')
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
###############################################################################
# This script calculates total forest loss, in hectares for the Zone of 
# Interaction of each TEAM site, using data from the University of Maryland 
# Global Forest Change dataset.
#
# After running this script, you can run the '2_animate_loss.R' script to 
# produce a series of animations of forest change at each TEAM site.
#
# Contact Alex Zvoleff (azvoleff@conservation.org) for the datafiles needed to 
# produce these animations.
###############################################################################

library(stringr)
library(plyr)
library(reshape2)
library(ggplot2)
library(scales) # for percent format

###############################################################################
# Setup parameters
###############################################################################

forest_threshold <- 75
to_utm <- FALSE
prefixes <- c('D:/azvoleff/Data', # CI-TEAM
              'H:/Data', # Buffalo drive
              'D:/Data/CI_H_Data/TEAM', # Orange drive
              '/home/azvoleff/Data') # vertica1
# Specify output_folder and data_folder relative to above prefixes
output_folder <- 'GFC_TEAM_Analysis'

###############################################################################
# Script begins below
###############################################################################

prefix <- prefixes[match(TRUE, unlist(lapply(prefixes, function(x) file_test('-d', x))))]
if (to_utm) {
    utm_string <- '_utm'
} else {
    utm_string <- '_wgs84'
}

###############################################################################
# Loss

this_output_folder <- file.path(output_folder, paste0(gsub('_', '', utm_string), '_', forest_threshold, 'pct'))
this_output_folder <- file.path(prefix, this_output_folder)

if (!file_test('-d', this_output_folder)) {
    stop(paste(this_output_folder, 'does not exist'))
}

loss_stats_files <- dir(this_output_folder, pattern='_stats_loss.csv')
site_codes <- str_extract(loss_stats_files, '_[A-Z]{2,3}_')
site_codes <- gsub('_', '', site_codes)

loss_dfs <- lapply(file.path(this_output_folder, loss_stats_files), read.csv)
loss <- ldply(loss_dfs, data.frame)
loss$site_code <- rep(site_codes, each=nrow(loss)/length(site_codes))
loss$threshold <- forest_threshold
loss$threshold <- factor(loss$threshold)

# Add some more data fields needed for plotting
loss$year <- as.Date(paste0(loss$year, '-1-1'))
site_codes_key <- read.csv('sitecode_key.csv')
loss$site_name <- site_codes_key$sitename_short[match(loss$site_code, 
                                                      site_codes_key$sitecode)]

# Add cumulative and percent loss fields
loss <- ddply(loss, .(threshold, site_code, aoi), transform,
                   loss_cum=c(NA, cumsum(loss[2:length(loss)])),
                   loss_pct=(loss/cover[1])*100)
loss <- ddply(loss, .(threshold, site_code, aoi), transform,
                   loss_pct_cum=c(NA, cumsum(loss_pct[2:length(loss_pct)])))
save(loss, file="loss.RData")

###############################################################################
# Gain

this_output_folder <- file.path(output_folder, paste0(gsub('_', '', utm_string), '_', forest_threshold, 'pct'))
this_output_folder <- file.path(prefix, this_output_folder)

if (!file_test('-d', this_output_folder)) {
    stop(paste(this_output_folder, 'does not exist'))
}

fgain_stats_files <- dir(this_output_folder, pattern='_stats_gain.csv')
site_codes <- str_extract(fgain_stats_files, '_[A-Z]{2,3}_')
site_codes <- gsub('_', '', site_codes)

these_fgain_dfs <- lapply(file.path(this_output_folder, fgain_stats_files), read.csv)
fgain <- ldply(these_fgain_dfs, data.frame)
fgain$site_code <- rep(site_codes, each=nrow(fgain)/length(site_codes))
fgain$threshold <- forest_threshold
fgain$threshold <- factor(fgain$threshold)

# Calculate total loss per site
summ_stats <- melt(fgain, id.vars=c('site_code', 'aoi', 'threshold'), 
                   measure.vars=c('gain', 'lossgain'))
summ_stats <- ddply(summ_stats, .(threshold, site_code, aoi, variable), summarize,
                    value=sum(value, na.rm=TRUE))
total_loss <- ddply(loss, .(threshold, site_code, aoi), summarize,
                    variable='loss', 
                    value=sum(loss, na.rm=TRUE))
summ_stats <- rbind(summ_stats, total_loss)
summ_stats <- summ_stats[order(summ_stats$threshold, summ_stats$site_code, 
                               summ_stats$variable, summ_stats$aoi), ]
summ_stats$variable <- factor(summ_stats$variable,
                              levels=c('loss', 'lossgain', 'gain'), 
                              labels=c('Loss', 'Loss & gain', 'Gain'))

# Normalize by initial cover, then multiply by 100, to get percentage of 
# initial forest cover
initial_cover <- loss[loss$year == as.Date('2000-1-1'), ]
initial_cover_row <- match(paste(summ_stats$site_code, summ_stats$aoi),
                           paste(initial_cover$site_code, initial_cover$aoi))
summ_stats$value_pct <- with(summ_stats, (value / 
                                          initial_cover$cover[initial_cover_row])*100)

save(summ_stats, file="summ_stats.RData")
source('0_settings.R')

library(stringr)

order_folder <- 'ESPA_orders'
scenelist_dir <- file.path(prefix, 'Landsat_scenelists')

###############################################################################
# Load scene lists as output by Earth Explorer

scenelist_regex <- '[A-Z]{2,3}_L((4-5)|7)_[0-9]{8}_scenelist.csv$'
scenelist_files <- dir(scenelist_dir, pattern=scenelist_regex, full.names=TRUE)
codes <- str_extract(basename(scenelist_files), '[A-Z]{2,3}')
sats <- str_extract(basename(scenelist_files), '(L4-5)|(L7)')

n <- 1
for (scenelist_file in scenelist_files) {
    message(paste0('Progress: ', round(n/length(scenelist_files)*100), '%'))
    these_scenes <- ee_read(scenelist_file)
    these_scenes$scenelist_file <- scenelist_files[n]
    these_scenes$sitecode <- codes[n]
    these_scenes$sats <- sats[n]
    if (n == 1) {
        scenes <- these_scenes
    } else {
        scenes <- merge(scenes, these_scenes, all=TRUE)
    }
    n <- n + 1
}

# # Exclude tiles that are not specifically included
# include <- read.csv('Included_tiles.csv')
# scenes <- scenes[(paste(scenes$WRS.Path, scenes$WRS.Row) %in%
#                   paste(include$path, include$row)), ]
#
# is_included <- paste(include$path, include$row) %in% paste(scenes$WRS.Path, scenes$WRS.Row)
# if (!all(is_included)) {
#     stop('missing some included scenes')
# }

save(scenes, file='1_all_scenes_noexclusions.RData')
source('0_settings.R')

library(plyr)
library(lubridate)

load('1_all_scenes.RData')

# Count scenes
lt20 <- scenes[(scenes$Cloud.Cover <= 20) &
               (scenes$Date.Acquired >= as.Date('1995/1/1')) &
               (scenes$Date.Acquired <= as.Date('2015/12/31')),]
lt20$year <- year(lt20$Date.Acquired)
lt20$year_cat <- cut(lt20$year, c(1995, 2000, 2005, 2010, 2015), include.lowest=TRUE)
lt20$pathrow <- paste(lt20$WRS.Path, lt20$WRS.Row, sep='-')

lt20bysite <- ddply(lt20, .(sitecode, year_cat), summarize,
                    n=length(sitecode),
                    mean_cloud_cover=mean(Cloud.Cover, na.rm=TRUE))

lt20bypathrow <- ddply(lt20, .(year_cat, pathrow), summarize,
                    n=length(sitecode),
                    mean_cloud_cover=mean(Cloud.Cover, na.rm=TRUE), .drop=FALSE)
nrow(lt20)
nrow(lt20bypathrow)
median(lt20bypathrow$n)
table(lt20bypathrow$n)
max(lt20bypathrow$n)
mean(lt20bypathrow$n)
hist(lt20bypathrow$n)


lt20bypathrow[lt20bypathrow$n < 4, ]
library(teamlucc)
library(ggplot2)

load('1_all_scenes.RData')

plot_folder <- 'Scene_plots'

###############################################################################
# Plot available scenes

for (sitecode in unique(sitecodes)) {
    message(sitecode)
    these_scenes <- scenes[scenes$sitecode == sitecode, ]
    these_scenes$Path_Row <- factor(these_scenes$Path_Row)
    # Make plots to assist in selecting Landsat images
    endpts <- seq(as.Date('1980/1/1'), as.Date('2020/1/1'), '10 years')
    for (i in 1:(length(endpts) - 1)) {
        start_date <- endpts[i]
        end_date <- endpts[i + 1]
        plot_title <- paste0(sitecode, ' (', start_date, ' - ', end_date, ')')
        ee_plot(these_scenes, start_date, end_date, title=plot_title)
        ggsave(file.path(plot_folder, paste0(sitecode, '_scenes_', start_date, 
                                             '--', end_date, '_bar.png')), 
                         width=PLOT_WIDTH, height=PLOT_HEIGHT, 
               dpi=PLOT_DPI)
        ee_plot(these_scenes, start_date, end_date, normalize=TRUE, title=plot_title)
        ggsave(file.path(plot_folder, paste0(sitecode, '_scenes_', start_date, 
                                             '--', end_date, '_line.png')), 
                         width=PLOT_WIDTH, height=PLOT_HEIGHT, 
               dpi=PLOT_DPI)
    }
}
###############################################################################
# This script produces animations of forest loss within the Zone of Interaction 
# of each TEAM site, using data from the University of Maryland Global Forest 
# Change dataset.
#
# Before running this script, you must first run the '1_calculate_loss.R' 
# script to produce the "loss.RData" file this script requires.
#
# Contact Alex Zvoleff (azvoleff@conservation.org) for the datafiles needed to 
# produce these animations.
###############################################################################

library(stringr)
library(ggplot2)
library(scales) # for percent format
library(gridExtra) # for grid.arrange 
library(raster)
library(gfcanalysis)
library(animation)

###############################################################################
# Setup parameters
###############################################################################

forest_threshold <- 75
to_utm <- TRUE
prefixes <- c('D:/azvoleff/Data', # CI-TEAM
              'H:/Data', # Buffalo drive
              'D:/Data/CI_H_Data/TEAM', # Orange drive
              '/home/azvoleff/Data') # vertica1
# Specify data_folder and data_folder relative to above prefixes
data_folder <- 'GFC_TEAM_Analysis'
zoi_folder <- 'TEAM/ZOIs'

height <- 4
width <- 4
dpi <- 200

sites_to_plot <- c('PSH', 'NAK', 'RNF')

###############################################################################
# Script begins below
###############################################################################

prefix <- prefixes[match(TRUE, unlist(lapply(prefixes, function(x) file_test('-d', x))))]
if (to_utm) {
    utm_string <- '_utm'
} else {
    utm_string <- '_wgs84'
}

###############################################################################
# Loss

this_data_folder <- file.path(data_folder, paste0(gsub('_', '', utm_string), '_', forest_threshold, 'pct'))
this_data_folder <- file.path(prefix, this_data_folder)
this_zoi_folder <- file.path(prefix, zoi_folder)

if (!file_test('-d', this_data_folder)) {
    stop(paste(this_data_folder, 'does not exist'))
}

gfc_extract_files <- dir(this_data_folder, pattern='_annual.envi$')
site_codes <- str_extract(gfc_extract_files, '_[A-Z]{2,3}_')
site_codes <- gsub('_', '', site_codes)
gfc_extract_files <- file.path(this_data_folder, gfc_extract_files)

site_codes_key <- read.csv('Site_Code_Key.csv')

# Load saved loss data
load('loss.RData')

years <- seq(as.Date('2000-1-1'), by='1 year', length=13)

###############################################################################
# Make plots
for (n in which(site_codes %in% sites_to_plot)) {
    site_code <- site_codes[n]
    print(site_code)

    out_dir <- paste0('anim_', site_code)
    out_basename <- paste0('anim_', site_code)

    maxpixels <- ceiling((width * height * dpi^2)/1000) * 1000

    site_name <- site_codes_key$sitename_short[match(site_code, 
                                                     site_codes_key$sitecode)]
    plot_title <- paste(site_name, 'forest change')

    gfc_annual <- brick(gfc_extract_files[n])

    zoi_file <- dir(this_zoi_folder, pattern=paste0('^ZOI_', site_code, 
                                                    '_[0-9]{4}.RData$'))
    load(file.path(this_zoi_folder, zoi_file))
    zoi$label <- 'ZOI'

    site_loss <- loss[loss$site_code == site_code & loss$aoi == 'ZOI', ]
    max_loss_pct_cum <- max(site_loss$loss_pct_cum, na.rm=TRUE)
    # Convert cover and loss values to square kilometers (they were in ha)
    site_loss$cover <- site_loss$cover / 100
    site_loss$loss <- site_loss$loss / 100
    max_cover <- max(site_loss$cover, na.rm=TRUE)

    size_scale <- 2
    make_frame <- function(year_num) {
         p1 <- plot_gfc(gfc_annual[[year_num]], zoi, 
                        maxpixels=maxpixels, size_scale=size_scale)
         #               title_string=plot_title)
         p1 <- p1 + theme(legend.key.height=unit(1.5, "line"))

         frame_loss <- site_loss[site_loss$year <= years[year_num], ]

         # Below is for the histogram animation
         total_loss <- sum(frame_loss$loss, na.rm=TRUE)
         current_cover <- frame_loss$cover[frame_loss$year == years[year_num]]
         hist_data <- data.frame(type='Forest loss', value=total_loss)
         hist_data <- rbind(hist_data, 
                            data.frame(type='Forest', value=current_cover))
         hist_data$type <- relevel(hist_data$type, 'Forest')

         # Below is for the line plot animation
         frame_loss$size <- 1
         frame_loss$size[frame_loss$year == years[year_num]] <- 2

         timeline <- data.frame(year=years, y=1, point_size=1, label='', stringsAsFactors=FALSE)
         timeline$point_size[timeline$year == years[year_num]] <- 2
         timeline$label[timeline$year == years[year_num]] <- format(years[year_num], '%Y')

         # Make an animated timeline
         p2 <- ggplot(timeline, aes(x=year, y=y, label=label)) +
             geom_line() + geom_point(aes(size=point_size)) +
             #geom_text(vjust=-.7, size=4) +
             theme_bw(base_size=8*size_scale) +
             scale_size(range=c(2, 4), guide=FALSE) +
             theme(axis.ticks=element_blank(),
                   axis.ticks.length=unit(0, "lines"),
                   axis.ticks.margin=unit(0, "lines"),
                   axis.title=element_blank(),
                   axis.text.y=element_blank(),
                   #axis.text.x=element_blank(),
                   panel.grid=element_blank(),
                   panel.border=element_blank(),
                   plot.margin=unit(c(0, 0, 0, 0), "lines"),
                   panel.margin=unit(0, "lines"))
         p2
         
         # Uncomment below for lineplot in bottom of frame
         # p3 <- ggplot(frame_loss, aes(year, loss_pct_cum/100)) +
         #     geom_line() + geom_point(aes(size=size)) +
         #     scale_y_continuous(labels=percent_format(), 
         #                        limits=c(0, max_loss_pct_cum/100)) +
         #     scale_size(range=c(2, 4), guide=FALSE) +
         #     xlim(min(years), max(years)) +
         #     xlab('Year') +
         #     #ylab('Forest loss (cumulative fraction of 2000 forest cover)') +
         #     ylab('Forest loss') +
         #     theme_bw(base_size=8*size_scale)

         # Uncomment below for histogram in bottom of frame
         p3 <- ggplot(hist_data, aes(type, value)) +
             geom_bar(stat='identity') +
             scale_y_continuous(limits=c(0, max_cover)) +
             ylab(expression(paste('Area (', km^2, ')'))) + 
             theme_bw(base_size=8*size_scale) + 
             theme(axis.ticks.x=element_blank(),
                   axis.title.x=element_blank(),
                   panel.grid.major.x=element_blank(),
                   panel.grid.minor.x=element_blank())

         grid.arrange(p1, p2, p3, nrow=3, heights=c(.6, .1, .3))
    }

    # # Uncomment below for HTML output
    # out_dir <- normalizePath(out_dir)
    # if (!file_test('-d', out_dir)) {
    #     dir.create(out_dir)
    # }
    # ani.options(outdir=out_dir, ani.width=width*dpi, ani.height=height*dpi, 
    #             verbose=FALSE)
    # saveHTML({
    #               for (year_num in 1:13) {
    #                   make_frame(year_num)
    #               }
    #          },
    #          img.name=out_basename,
    #          imgdir=paste0(out_basename, '_imgs'),
    #          outdir=out_dir,
    #          htmlfile=paste0(out_basename, ".html"),
    #          autobrowse=FALSE,
    #          title=paste(site_name, 'forest change'))
 
    # Uncomment below for movie output
    ani.options(ffmpeg="C:/Program Files/ffmpeg/bin/ffmpeg.exe",
                ani.width=width*dpi, ani.height=height*dpi, verbose=TRUE,
                outdir=getwd())
    saveVideo({
                   for (year_num in 1:13) {
                       make_frame(year_num)
                   }
              },
             video.name = paste0(out_basename, '.mov'),
             # For Mac:
             other.opts = '-preset slow -r 25 -pix_fmt yuv420p')
             # For PC:
             #other.opts = '-f mp4 -preset slow -r 25')

}
library(teamlucc)

# load('1_all_scenes.RData')
load('1_all_scenes_noexclusions.RData')

###############################################################################
# Output ESPA order files

# # Order all scenes with less than 10% cloud cover
# espa_scenelist(scenes, as.Date('1980/1/1'), as.Date('2015/1/1'), min_clear=.9,
#                file.path(order_folder, 'scenes_allsites_lt10cloud.txt'))
# # Order all scenes with less than 5% cloud cover
# espa_scenelist(scenes, as.Date('1980/1/1'), as.Date('2015/1/1'), min_clear=.95,
#                file.path(order_folder, 'scenes_allsites_lt5cloud.txt'))
# # Order all scenes with less than 2% cloud cover
# espa_scenelist(scenes, as.Date('1980/1/1'), as.Date('2015/1/1'), min_clear=.98,
#                file.path(order_folder, 'scenes_allsites_lt2cloud.txt'))
#
# scenes_lt20gt10 <- scenes[scenes$Frac_Clear >= .8 & scenes$Frac_Clear <= .9, ]
# Order all scenes with less than 20% cloud cover
# espa_scenelist(scenes_lt20gt10, as.Date('1980/1/1'), as.Date('2015/1/1'), min_clear=0,
#                file.path(order_folder, 'scenes_allsites_lt20cloud.txt'))

# Order all scenes with less than 20% cloud cover (inclusive of 0-10%
scenes_lt20 <- scenes[scenes$Frac_Clear >= .8 & scenes$Frac_Clear <= .9, ]
espa_scenelist(scenes_lt20, as.Date('1980/1/1'), as.Date('2015/1/1'), 
               min_clear=0, file.path(order_folder, 
                                      'scenes_allsites_lt20cloud_noexclusions.txt'))

source('0_settings.R')

espa_email <- 'azvoleff@conservation.org'

###############################################################################
# All Sites
download_folder <- 'I:/Landsat_Originals'

stopifnot(file_test('-d', download_folder))

# espa_download(espa_email, '5142014-155630', download_folder) # 0-2% cover
# espa_download(espa_email, '5142014-155558', download_folder) # 2-5% cover
# espa_download(espa_email, '5142014-15558', download_folder) # 5-10% cover
# espa_download(espa_email, '5142014-153954', download_folder) # 10-20% cover
# espa_download(espa_email, '6162014-8585', download_folder) # 10-20% cover
library(stringr)
options(RCurlOptions=list(cainfo=system.file("CurlSSL", "cacert.pem", 
                                             package="RCurl")))
curl=getCurlHandle()
login_page <- unlist(strsplit(getURL('https://espa.cr.usgs.gov/login/', curl=curl), '\n'))
csrfmiddlewaretoken <- login_page[grepl("csrfmiddlewaretoken", login_page)]
csrfmiddlewaretoken <- gsub("(value=)|(')", '',
                            str_extract(csrfmiddlewaretoken, 
                                        "value='[a-zA-Z0-9]*'"))
params <- list('username'="azvoleff",
               'password'="0ZmNcTDR1ZtSs85",
               'submit'="Log In",
               'next'="",
               'csrfmiddlewaretoken'=csrfmiddlewaretoken)
post_res <- postForm('https://espa.cr.usgs.gov/login',
                     .params=params, style="POST", curl=curl)
espa_page <- getURL("http://espa.cr.usgs.gov/ordering/status/azvoleff%40conservation.org-782014-164815", curl=curl)

espa_download(espa_email, '782014-164815', download_folder) # 10-20% cover

library(stringr)
library(plyr)

#order_files <- dir('ESPA_orders', pattern='.txt', full.names=TRUE)
order_files <- dir('ESPA_orders', pattern='noexclusions.txt', full.names=TRUE)
read_order_file <- function(order_file) {
    ret <- read.table(order_file, col.names='scene_id')
    ret$scene_id_full <- ret$scene_id
    ret$scene_id <- str_extract(ret$scene_id, '((LE[78])|(LT[45]))[0-9]{13}')
    ret <- cbind(order_file=basename(order_file), ret)
}
ordered_scenes <- ldply(order_files, read_order_file)

downloaded_landsats <- dir('I:/Landsat_Originals', pattern='.tar.gz')
downloaded_scenes <- str_extract(downloaded_landsats, '((LE[78])|(LT[45]))[0-9]{13}')

missing_scenes <- ordered_scenes[!(ordered_scenes$scene_id %in% downloaded_scenes), ]

# write.table(missing_scenes$scene_id_full, 'missing_scenes_order.txt', row.names=FALSE, 
#             col.names=FALSE, quote=FALSE, sep='\n')
write.table(missing_scenes$scene_id_full, 'noexclusions_order.txt', row.names=FALSE, 
            col.names=FALSE, quote=FALSE, sep='\n')

table(missing_scenes$order_file)
library(dplyr)
library(stringr)

base_dir <- 'I:/Landsat_Originals'

dl_landsats <- dir(base_dir, pattern='.tar.gz')
dl_scenes <- str_extract(dl_landsats, '((LE[78])|(LT[45]))[0-9]{13}')
dl_scenes_process_times <- gsub('SC', '', str_extract(dl_landsats, 'SC[0-9]{14}'))
dl_scenes_process_times <- strptime(dl_scenes_process_times, format='%Y%m%d%H%M%S', tz='GMT')
scene_date <- as.Date(substr(dl_scenes, 10, 16), '%Y%j')

dl_scenes <- data.frame(filename=dl_landsats,
                        scene_id=dl_scenes, 
                        process_time=dl_scenes_process_times,
                        date=scene_date)

dim(dl_scenes)

rm_duplicate_scenes <- function(scene_group) {
    if (nrow(scene_group) > 1) {
        scene_group <- scene_group[order(scene_group$scene_id), ]
        del_scenes <- data.frame(path=file.path(base_dir, scene_group$filename[1:(nrow(scene_group) - 1)]))
        unlink(del_scenes$path)
    } else {
        del_scenes <- data.frame()
    }
    return(del_scenes)
}

# Note that below line gives an error if no scenes are deleted
del_scenes <- do(group_by(dl_scenes, scene_id), rm_duplicate_scenes(.))
library(teamlucc)

library(foreach)
library(iterators)
library(doParallel)

registerDoParallel(4)

start_dates <- as.Date(c('1988/1/1',
                         '1993/1/1',
                         '1998/1/1',
                         '2003/1/1',
                         '2008/1/1'))
end_dates <- as.Date(c('1992/12/31',
                       '1997/12/31',
                       '2002/12/31',
                       '2007/12/31', 
                       '2012/12/31'))
# start_dates <- as.Date(c('1998/1/1', '2008/1/1'))
# end_dates <- as.Date(c('2002/12/31', '2012/12/31'))
stopifnot(length(start_dates) == length(end_dates))

in_folder <-'I:/Landsat_Originals'
out_base <-'H:/Data/Landsat'
#out_base <-'Z:/Data/Landsat'

included_tiles <- read.csv('Included_tiles.csv')

foreach (sitecode=iter(included_tiles$sitecode),
         wrspath=iter(included_tiles$path),
         wrsrow=iter(included_tiles$row),
         .packages='iterators') %:%
    foreach (start_date=iter(start_dates),
             end_date=iter(end_dates),
             .packages=c('teamlucc')) %dopar% {
        this_pathrow <- sprintf('%03i%03i', wrspath, wrsrow)
        output_folder <- file.path(out_base, sitecode)
        if (!file_test('-d', output_folder)) {
            stop(paste(output_folder, 'does not exist'))
        }
        message(paste0(sitecode, ': ',
                       format(start_date, '%Y/%m/%d'), '-',
                       format(end_date, '%Y/%m/%d'),
                       ', pathrow ', this_pathrow))
        tryCatch(espa_extract(in_folder, output_folder, pathrows=this_pathrow, 
                              start_date=start_date, end_date=end_date),
                 error=function(e) {
                     print(paste0('extract failed for ', sitecode, ' ', 
                                  start_date, '-', end_date))
                 })
}
source('0_settings.R')

library(rgeos)

# NOTE IF SMOOTHING IS CHANGED NEED TO USE THE HACKED RASTER CODE AND UPDATE 
# THE CALL TO AUTO_SETUO_DEM BELOW.
smoothing <- 1
out_dir <- 'CGIAR_SRTM_TEAM_nosmoothing'

for (sitecode in sitecodes) {
    message(paste0('Processing DEMs for ', sitecode, '...'))

    output_path <- file.path(prefix, out_dir, sitecode)
    if (!file_test('-d', output_path)) {
        dir.create(output_path)
    }

    load(file.path(prefix, 'TEAM', 'ZOI_CSA_PAs',
                   paste0(sitecode, '_ZOI_CSA_PA.RData')))
    aoi <- gConvexHull(aois)
    aoi <- spTransform(aoi, CRS(utm_zone(aoi, proj4string=TRUE)))
    aoi <- gBuffer(aoi, width=5000)

    auto_setup_dem(aoi, output_path=output_path, dem_extents=dem_extents, 
                   n_cpus=n_cpus, overwrite=overwrite, crop_to_aoi=TRUE, 
                   verbose=verbose)
    # auto_setup_dem(aoi, output_path=output_path, dem_extents=dem_extents, 
    #                smoothing=smoothing, n_cpus=n_cpus, overwrite=overwrite, 
    #                crop_to_aoi=TRUE, verbose=verbose)
}
load('6_z_evaluate_DEMS.RData')

tc_key <- dem_freqs[1:3]
tc_key$do_tc <- (dem_freqs$pct95 > 10)

table(tc_key$do_tc)

write.csv(tc_key, file='Scene_topocorr_key.csv', row.names=FALSE)
source('0_settings.R')

library(rgeos)
library(stringr)

library(foreach)
library(doParallel)
library(iterators)

registerDoParallel(10)

dem_freqs <- foreach(sitecode=iter(sitecodes), .combine=rbind, .inorder=FALSE,
         .packages=c('teamlucc', 'stringr', 'rgeos', 'sp', 'rgdal', 'foreach', 
                     'iterators')) %dopar% {
    dem_path <- file.path(prefix, 'CGIAR_SRTM_TEAM_nosmoothing', sitecode)
    slopeaspect_files <- dir(dem_path,
                             pattern='^slopeaspect_[0-9]{3}-[0-9]{3}.envi$',
                             full.names=TRUE)

    wrspathrow <- str_extract(slopeaspect_files, '_[0-9]{3}-[0-9]{3}[.]')
    wrspath <- gsub('[_-]', '', str_extract(wrspathrow, '^_[0-9]{3}-'))
    wrsrow <- gsub('[-.]', '', str_extract(wrspathrow, '-[0-9]{3}[.]$'))

    load(file.path(prefix, 'TEAM', 'ZOI_CSA_PAs',
                   paste0(sitecode, '_ZOI_CSA_PA.RData')))
    aoi <- gConvexHull(aois)
    aoi <- spTransform(aoi, CRS(utm_zone(aoi, proj4string=TRUE)))
    aoi <- gBuffer(aoi, width=5000)

    # Mask DEM
    site_dem_freqs <- foreach(slopeaspect_file=iter(slopeaspect_files), 
                              wrspath=iter(wrspath), wrsrow=iter(wrsrow), 
                              .combine=rbind, .packages=c('raster', 'sp'), 
            .inorder=FALSE) %do%  {
        slope <- (brick(slopeaspect_file)[[1]] / 10000) * (180 / pi)
        slope <- mask(slope, aoi)
        quants <- quantile(slope, probs=seq(0, 1, .05), na.rm=TRUE)
        ret <- data.frame(sitecode=sitecode, wrspath=wrspath, wrsrow=wrsrow, 
                          mean=cellStats(slope, mean, na.rm=TRUE),
                          sd=cellStats(slope, sd, na.rm=TRUE))
        ret <- cbind(ret, matrix(quants, nrow=1))
        names(ret)[6:ncol(ret)] <- gsub('%', '', gsub('^', 'pct', names(quants)))
        ret
    }

    site_dem_freqs
}

dem_freqs <- dem_freqs[order(dem_freqs$sitecode, dem_freqs$wrspath, dem_freqs$wrsrow), ]

dem_freqs_readable <- dem_freqs
dem_freqs_readable[4:ncol(dem_freqs_readable)] <- round(dem_freqs_readable[4:ncol(dem_freqs_readable)], 1)
dem_freqs_readable[4:ncol(dem_freqs_readable)][dem_freqs_readable[4:ncol(dem_freqs_readable)] < 1] <- ''

save(dem_freqs, file='5_z_evaluate_DEMS.RData')
source('0_settings.R')

dem_path <- file.path(prefix, 'CGIAR_SRTM')

dems <- lapply(dir(dem_path, pattern='.tif$', full.names=TRUE), raster)
dem_extents <- get_extent_polys(dem_path)

save(dem_extents, file='dem_extents.RData')
source('0_settings.R')

library(stringr)
library(rgeos)

library(foreach)
library(iterators)
library(doParallel)

overwrite <- TRUE
reprocess <- FALSE

registerDoParallel(n_cpus)

scene_topocorr_key <- read.csv('Scene_topocorr_key.csv')

sitecodes_rep <- c()
dem_paths <- c()
image_dirs <- c()
do_tcs <- c()
for (sitecode in sitecodes) {
    message(paste0('Preprocessing imagery for ', sitecode, '...'))
    base_dir <- file.path(prefix, 'Landsat', sitecode)
    new_image_dirs <- dir(base_dir,
                          pattern='^[0-9]{3}-[0-9]{3}_[0-9]{4}-[0-9]{3}_((LT[45])|(LE7))$')

    if ((length(new_image_dirs) > 1) & (!reprocess)) {
        # Do nothing if preprocessed files already exist
        new_image_dirs <- new_image_dirs[!unlist(lapply(file.path(base_dir, 
                                                                  new_image_dirs), 
                                                        is_preprocessed))]
    } 

    if (length(new_image_dirs) < 1) {
        next
    }

    wrspathrows <- str_extract(new_image_dirs, '^[0-9]{3}-[0-9]{3}_')
    wrspaths <- as.numeric(gsub('[-]', '', str_extract(wrspathrows, '^[0-9]{3}-')))
    wrsrows <- as.numeric(gsub('[_-]', '', str_extract(wrspathrows, '-[0-9]{3}_')))
    tc_key_rows <- match(paste(sitecode, wrspaths, wrsrows),
                         with(scene_topocorr_key, paste(sitecode, wrspath, wrsrow)))
    new_do_tcs <- scene_topocorr_key$do_tc[tc_key_rows]
    do_tcs <- c(do_tcs, new_do_tcs)

    dem_path <- file.path(prefix, 'CGIAR_SRTM_TEAM_4smoothing', sitecode)
    dem_paths <- c(dem_paths, rep(dem_path, length(new_image_dirs)))

    sitecodes_rep <- c(sitecodes_rep, rep(sitecode, length(new_image_dirs)))

    new_image_dirs <- file.path(base_dir, new_image_dirs)
    image_dirs <- c(image_dirs, new_image_dirs)
}

stopifnot(length(sitecodes_rep) == length(dem_paths))
stopifnot(length(sitecodes_rep) == length(image_dirs))
stopifnot(length(sitecodes_rep) == length(do_tcs))

print(paste("Images to process:", length(image_dirs)))
foreach(sitecode=iter(sitecodes_rep), dem_path=iter(dem_paths), 
        image_dir=iter(image_dirs), do_tc=iter(do_tcs), 
        .packages=c('teamlucc', 'rgeos', 'sp', 'rgdal'), 
        .inorder=FALSE) %dopar% {
    message(paste0('Processing ', image_dir, '...'))
    # Set a separate raster temp dir for each worker, so that temp files can be 
    # cleared after each iteration
    rasterOptions(tmpdir=paste0(tempdir(), '_raster'))

    load(file.path(prefix, 'TEAM', 'ZOI_CSA_PAs',
                   paste0(sitecode, '_ZOI_CSA_PA.RData')))
    aoi <- gConvexHull(aois)
    aoi <- spTransform(aoi, CRS(utm_zone(aoi, proj4string=TRUE)))
    aoi <- gBuffer(aoi, width=5000)
    auto_preprocess_landsat(image_dir, prefix=sitecode, tc=do_tc,
                            dem_path=dem_path, aoi=aoi, n_cpus=1, 
                            cleartmp=FALSE, overwrite=overwrite, verbose=TRUE)
    removeTmpFiles(h=0)
}
source('0_settings.R')

scene_topocorr_key <- read.csv('Scene_topocorr_key.csv')

total_preprocessed <- 0
total_not_preprocessed <- 0
writeLines('Site\tDone\tRemaining')
writeLines('-------------------------')
for (sitecode in sitecodes) {
    base_dir <- file.path(prefix, 'Landsat', sitecode)
    image_dirs <- dir(base_dir,
                      pattern='^[0-9]{3}-[0-9]{3}_[0-9]{4}-[0-9]{3}_((LT[45])|(LE7))$', 
                      full.names=TRUE)

    total_dirs <- length(image_dirs)

    if (length(image_dirs) >= 1) {
        not_preprocessed <- image_dirs[!unlist(lapply(image_dirs, is_preprocessed))]
        preprocessed <- image_dirs[unlist(lapply(image_dirs, is_preprocessed))]
    } else {
        preprocessed <- c()
        not_preprocessed <- c()
    }
    total_preprocessed <- total_preprocessed + length(preprocessed)
    total_not_preprocessed <- total_not_preprocessed + length(not_preprocessed)

    # print(not_preprocessed)

    writeLines(paste0(sitecode, '\t', length(preprocessed), '\t', 
                      length(not_preprocessed)))

}
writeLines('-------------------------')
writeLines(paste0('Total:\t', total_preprocessed, '\t', 
                  total_not_preprocessed))
source("0_settings.R")

in_folder <- 'H:/Data/Landsat'
out_folder <- 'D:/Landsat_HDFs'

for (sitecode in sitecodes) {
    print(paste0('*** Processing ', sitecode, ' ***'))
    image_dirs <- dir(file.path(in_folder, sitecode),
                      pattern='^[0-9]{3}-[0-9]{3}_[0-9]{4}-[0-9]{3}_((LT[45])|(LE7))$', 
                      full.names=TRUE)

    if (length(image_dirs) >= 1) {
        # Do nothing if not preprocessed
        image_dirs <- image_dirs[unlist(lapply(image_dirs, is_preprocessed))]
        total_dirs <- length(image_dirs)
    }

    print(paste(total_dirs, 'images are preprocessed'))
    for (image_dir in image_dirs) {
        lndsr_regex <- '^lndsr.((LT4)|(LT5)|(LE7)|(LE8))[0-9]{6}[12][0-9]{6}[a-zA-Z]{3}[0-9]{2}((.hdf(.hdr)?)|(.txt))'
        lndsr_files <- dir(image_dir, pattern=lndsr_regex)
        if (length(lndsr_files) > 1) {
            print(paste('Moving files from', image_dir))
            new_dir <- file.path(out_folder, sitecode, basename(image_dir))
            if (!file_test('-d', new_dir))dir.create(new_dir)
            ret <- file.rename(file.path(image_dir, lndsr_files), 
                               file.path(new_dir, lndsr_files))
            if (sum(ret) != length(lndsr_files))
                stop(paste('failed to move', sum(!ret), 'files'))
        }
    }
}
source("0_settings.R")

#in_folder <- 'H:/Data/Landsat'
#in_folder <- 'Z:/Data/Landsat'
in_folder <- 'O:/Data/CI_H_Data/Landsat'

for (sitecode in sitecodes) {
    print(paste0('*** Processing ', sitecode, ' ***'))
    image_dirs <- dir(file.path(in_folder, sitecode),
                      pattern='^[0-9]{3}-[0-9]{3}_[0-9]{4}-[0-9]{3}_((LT[45])|(LE7))$', 
                      full.names=TRUE)

    if (length(image_dirs) >= 1) {
        # Do nothing if not preprocessed
        image_dirs <- image_dirs[unlist(lapply(image_dirs, is_preprocessed))]
        total_dirs <- length(image_dirs)
    }

    print(paste(total_dirs, 'images are preprocessed'))
    for (image_dir in image_dirs) {
        lndsr_regex <- '^lndsr.((LT4)|(LT5)|(LE7)|(LE8))[0-9]{6}[12][0-9]{6}[a-zA-Z]{3}[0-9]{2}((.hdf(.hdr)?)|(.txt))'
        lndsr_files <- dir(image_dir, pattern=lndsr_regex)
        if (length(lndsr_files) > 1) {
            print(paste('Deleting files from', image_dir))
            ret <- unlink(file.path(image_dir, lndsr_files))
            if (ret != 0) {
                stop(paste('failed to delete files in', image_dir))
            }
        }
    }
}
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
source('0_settings.R')

library(stringr)

input_dir <- file.path(prefix, 'Landsat', 'Cloud_Filled')

image_files <- dir(input_dir, 
                   pattern=paste0('^[a-zA-Z]*_[0-9]{3}-[0-9]{3}_[0-9]{4}-[0-9]{3}_cf.tif$'))

# First figure out path/rows and dates
wrspathrows <- gsub('[_]', '', str_extract(basename(image_files), 
                                            '_[0-9]{3}-[0-9]{3}_'))
wrspath <- substr(wrspathrows, 1, 3)
wrsrow <- substr(wrspathrows, 5, 7)

image_dates <- as.Date(str_extract(basename(image_files), 
                                   '_[0-9]{4}-[0-9]{3}_'), '_%Y-%j_')
site <- str_extract(image_files, '^[a-zA-Z]*')

cf_file_list <- data.frame(site=site, path=wrspath, row=wrsrow, 
                           date=image_dates, rating='', notes='', 
                           file=image_files)

cf_file_list <- cf_file_list[order(cf_file_list$site,
                                   cf_file_list$path,
                                   cf_file_list$row,
                                   cf_file_list$date), ]

# Ensure existing file is not overwritten
csv_output_file <- '7_0_cf_image_list.csv'
stopifnot(!(file_test('-f', csv_output_file)))

write.csv(cf_file_list, file=csv_output_file, row.names=FALSE)
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
library(teamlucc)

# library(tools)
# library(lubridate)
# library(stringr)
# library(SDMTools)

prefix <- 'D:/azvoleff/Data'
sitecode <- 'BBS'
data_dir <- file.path(prefix, 'Landsat', sitecode)
wrspath <- 124
wrsrow <- 64
start_date <- as.Date('1988/1/1')
end_date <- as.Date('1992/12/31')
base_date <- NULL
mid_date <- (end_date - start_date)/2 + start_date
out_base <- file.path(data_dir,
                      paste0(sitecode,
                             sprintf('_%03i-%03i_', wrspath, wrsrow),
                             format(mid_date, '%Y-%j'), '_cf'))
out_name <- paste0(out_base, '.tif')
tc <- TRUE
sensors <- list('L4T', 'L5T', 'L7E', 'L8E')
overwrite <- TRUE
verbose <- 2
byblock <- FALSE
notify <- print
overwrite <- TRUE
threshold <- 1
ext <- 'tif'
max_iter <- 5
DN_min <- -100
DN_max <- 16000
algorithm <- 'CLOUD_REMOVE_FAST'

options(error=recover)

auto_cloud_fill(data_dir, wrspath, wrsrow, start_date, end_date, 
                out_name=out_name, tc=tc, sensors=sensors,
                overwrite=TRUE, verbose=2, DN_min=-100, 
                DN_max=16000, algorithm='CLOUD_REMOVE_FAST', byblock=FALSE)
source('0_settings.R')

library(stringr)

library(foreach)
library(doParallel)

registerDoParallel(n_cpus)

scene_topocorr_key <- read.csv('Scene_topocorr_key.csv')

overwrite <- TRUE
reprocess <- TRUE

verbose <- TRUE
algorithm <- 'CLOUD_REMOVE_FAST'
#algorithm <- 'simple'

start_dates <- as.Date(c('1988/1/1',
                         '1993/1/1',
                         '1998/1/1',
                         '2003/1/1',
                         '2008/1/1'))
end_dates <- as.Date(c('1992/12/31',
                       '1997/12/31',
                       '2002/12/31',
                       '2007/12/31', 
                       '2012/12/31'))
sensors_bydate <- list(c('L4T', 'L5T', 'L7E', 'L8E'),
                       c('L4T', 'L5T', 'L7E', 'L8E'),
                       c('L4T', 'L5T', 'L7E', 'L8E'),
                       c('L4T', 'L5T', 'L7E', 'L8E'),
                       c('L4T', 'L5T', 'L8E'))
# start_dates <- as.Date(c('1988/1/1', '1998/1/1', '2008/1/1'))
# end_dates <- as.Date(c('1992/12/31', '2002/12/31', '2012/12/31'))
# sensors_bydate <- list(c('L4T', 'L5T', 'L7E', 'L8E'),
#                        c('L4T', 'L5T', 'L7E', 'L8E'),
#                        c('L4T', 'L5T', 'L8E'))
# start_dates <- as.Date(c('1998/1/1', '2008/1/1'))
# end_dates <- as.Date(c('2002/12/31', '2012/12/31'))
# sensors_bydate <- list(c('L4T', 'L5T', 'L7E', 'L8E'),
#                        c('L4T', 'L5T', 'L8E'))
stopifnot(length(start_dates) == length(end_dates))
stopifnot(length(start_dates) == length(sensors_bydate))

output_dir <- file.path(prefix, 'Landsat', 'Cloud_Filled')

sitecodes_rep <- c()
base_dirs <- c()
wrspaths <- c()
wrsrows <- c()
tcs <- c()
for (sitecode in sitecodes) {
    this_base_dir <- file.path(prefix, 'Landsat', sitecode)
    image_dirs <- dir(this_base_dir, pattern='^[0-9]{3}-[0-9]{3}_[0-9]{4}-[0-9]{3}_((LE)|(LT))[4578]$')
    wrspathrows <- unique(str_extract(image_dirs, '^[0-9]{3}-[0-9]{3}_'))
    these_wrspaths <- as.numeric(gsub('[-]', '', str_extract(wrspathrows, '^[0-9]{3}-')))
    these_wrsrows <- as.numeric(gsub('[_-]', '', str_extract(wrspathrows, '-[0-9]{3}_')))

    tc_key_rows <- match(paste(sitecode, these_wrspaths, these_wrsrows),
                         with(scene_topocorr_key, paste(sitecode, wrspath, wrsrow)))
    new_tcs <- scene_topocorr_key$do_tc[tc_key_rows]
    tcs <- c(tcs, new_tcs)

    wrspaths <- c(wrspaths, these_wrspaths)
    wrsrows <- c(wrsrows, these_wrsrows)
    base_dirs <- c(base_dirs, rep(this_base_dir, length(these_wrspaths)))
    sitecodes_rep <- c(sitecodes_rep, rep(sitecode, length(these_wrspaths)))
}

# sitecode <- sitecodes_rep[1]
# base_dir <- base_dirs[1]
# wrspath <- wrspaths[1]
# wrsrow <- wrsrows[1]
#
# start_date <- start_dates[1]
# end_date <- end_dates[1]

stopifnot(length(sitecodes_rep) == length(base_dirs))
stopifnot(length(sitecodes_rep) == length(wrspaths))
stopifnot(length(sitecodes_rep) == length(wrsrows))
stopifnot(length(sitecodes_rep) == length(tcs))

foreach (sitecode=iter(sitecodes_rep), base_dir=iter(base_dirs), 
         wrspath=iter(wrspaths), wrsrow=iter(wrsrows), tc=iter(tcs),
         .inorder=FALSE)  %:% 
    foreach (start_date=iter(start_dates), end_date=(end_dates), 
             sensors=iter(sensors_bydate),
             .packages=c('teamlucc', 'raster', 'sp'),
             .inorder=FALSE) %dopar% {
        mid_date <- (end_date - start_date)/2 + start_date
        out_base <- file.path(output_dir,
                              paste0(sitecode,
                                     sprintf('_%03i-%03i_', wrspath, wrsrow),
                                     format(mid_date, '%Y-%j'), '_cf'))

        status_line <- paste0(sitecode, ' ', wrspath, '/', wrsrow, ' (', 
                              format(start_date, '%Y/%d/%m'), ' - ', 
                              format(end_date, '%Y/%d/%m'), ')')

        output_file <- paste0(out_base, ext)
        if (file_test('-f', output_file)) {
            if (!reprocess) return()
            if (!overwrite) stop(paste(output_file, 'already exists'))
        }

        # Set a separate raster temp dir for each worker, so that temp 
        # files can be cleared after each iteration
        rasterOptions(tmpdir=paste0(tempdir(), '_raster'))

        tryCatch(cf <- auto_cloud_fill(base_dir, wrspath, wrsrow, start_date, 
                                       end_date, out_name=out_base, tc=tc, 
                                       sensors=sensors, n_cpus=1, 
                                       overwrite=overwrite, verbose=verbose, 
                                       DN_min=-100, DN_max=16000, 
                                       algorithm=algorithm, byblock=FALSE),
                 error=function(e) {
                     print(paste(status_line, 'FAILED'))
                 })

        removeTmpFiles(h=0)
    }
library(teamlucc)

# library(tools)
# library(lubridate)
# library(stringr)
# library(SDMTools)

prefix <- 'D:/azvoleff/Data'
sitecode <- 'RNF'
data_dir <- file.path(prefix, 'Landsat', sitecode)
wrspath <- 158
wrsrow <- 75
start_date <- as.Date('1993/1/1')
end_date <- as.Date('1997/12/31')
base_date <- NULL
mid_date <- (end_date - start_date)/2 + start_date
out_base <- file.path(data_dir,
                      paste0(sitecode,
                             sprintf('_%03i-%03i_', wrspath, wrsrow),
                             format(mid_date, '%Y-%j'), '_cf'))
out_name <- paste0(out_base, '.tif')
tc <- TRUE
sensors <- list('L4T', 'L5T', 'L7E', 'L8E')
overwrite <- TRUE
verbose <- 2
byblock <- FALSE
notify <- print
overwrite <- TRUE
threshold <- 1
ext <- 'tif'
max_iter <- 5
DN_min <- -100
DN_max <- 16000
algorithm <- 'CLOUD_REMOVE_FAST'

options(error=recover)

auto_cloud_fill(data_dir, wrspath, wrsrow, start_date, end_date, 
                out_name=out_name, tc=tc, sensors=sensors,
                overwrite=TRUE, verbose=2, DN_min=-100, 
                DN_max=16000, algorithm='CLOUD_REMOVE_FAST', byblock=FALSE)
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
library(raster)

sitecode <- 'BCI'
base_dir <- file.path(prefix, 'Landsat', sitecode)
image_files <- dir(base_dir, pattern='^[a-zA-Z]*_mosaic_[0-9]{4}.tif$', full.names=TRUE)
mask_files <- dir(base_dir, pattern='^[a-zA-Z]*_mosaic_[0-9]{4}_masks.tif$', full.names=TRUE)

for (n in 1:length(image_files)) {
    pdf(extension(image_files[n], '.pdf'))
    plot(stack(image_files[n]))
    dev.off()

    pdf(extension(mask_files[n], '.pdf'))
    plot(stack(mask_files[n]))
    dev.off()
}
source('0_settings.R')

library(foreach)
library(iterators)
library(doParallel)

registerDoParallel(n_cpus)

library(inline)
library(RcppArmadillo)
library(abind)
library(gdalUtils)
library(rgdal)
library(rgeos)
library(stringr)
library(tools)
library(lubridate)

reprocess <- TRUE
overwrite <- TRUE
builddem <- TRUE
#imgtype <- 'normalized'
imgtype <- 'raw'

stopifnot(imgtype %in% c('normalized', 'raw'))

input_dir <- file.path(prefix, 'Landsat', 'Cloud_Filled')
output_dir <- file.path(prefix, 'Landsat', 'Composites', 'Mosaics')

notify('Starting mosaicking.')
for (sitecode in sitecodes) {
    raster_tmpdir <- file.path(temp, paste0('raster_',
                            paste(sample(c(letters, 0:9), 15), collapse='')))
    dir.create(raster_tmpdir)
    rasterOptions(tmpdir=raster_tmpdir)

    message(paste0('Mosaicking images for ', sitecode, '...'))

    if (imgtype == 'normalized') {
        pattern=paste0('^', sitecode, '_[0-9]{3}-[0-9]{3}_[0-9]{4}-[0-9]{3}_cf(_((normbase)|(normalized))).tif$')
    } else {
        pattern=paste0('^', sitecode, '_[0-9]{3}-[0-9]{3}_[0-9]{4}-[0-9]{3}_cf.tif$')
    }
    image_files <- dir(input_dir, pattern=pattern, full.names=TRUE)
    image_stacks <- lapply(image_files, stack)

    mask_files <- paste0(file_path_sans_ext(image_files), '_masks.tif')
    if (any(!file_test('-f', mask_files))) {
        stop('could not locate mask files')
    }
    mask_stacks <- lapply(mask_files, stack)

    image_date_strings <- unique(str_extract(basename(image_files), 
                                             '_[0-9]{4}-[0-9]{3}_'))

    # Function to get maximal extent from a list of extents
    get_total_extent <- function(extents) {
        full_ext <- extents[[1]]
        extents <- extents[-1]
        while (length(extents) > 0) {
            full_ext <- merge(full_ext, extents[[1]])
            extents <- extents[-1]
        }
        return(full_ext)
    }
    # First calculate maximal extent of each stack of images, considering all 
    # the dates to ensure that the mosaic extents match for each epoch even if 
    # some epochs are missing path/rows
    mos_exts <- foreach(image_date_string=iter(image_date_strings),
                        .packages=c('raster', 'rgdal', 'lubridate', 'tools', 
                                    'foreach', 'iterators')) %dopar% {
        epoch_image_files <- image_files[grepl(image_date_string, image_files)]
        # Calculate full extent of mosaic
        extents <- lapply(epoch_image_files, function(x) {
            x <- stack(x)
            extent(x)
        })
        get_total_extent(extents)
    }
    mos_ext <- get_total_extent(mos_exts)

    # Calculate extents, with origins at 0,0
    mosaic_te <- as.numeric(bbox(mos_ext))
    tr <- c(30, 30)
    # Setup xmin
    mosaic_te[1] <- round(mosaic_te[1] - mosaic_te[1] %% tr[1])
    # Setup ymin
    mosaic_te[2] <- round(mosaic_te[2] - mosaic_te[2] %% tr[2])
    # Setup xmax
    mosaic_te[3] <- round(mosaic_te[3] + tr[1] - mosaic_te[3] %% tr[1])
    # Setup ymax
    mosaic_te[4] <- round(mosaic_te[4] + tr[2] - mosaic_te[4] %% tr[2])
    stopifnot(all(round(mosaic_te / 30) == (mosaic_te / 30)))

    ###########################################################################
    # Iterate over the images to mosaick
    mosaic_stacks <- foreach(image_date_string=iter(image_date_strings),
                             .packages=c('raster', 'rgdal', 'lubridate', 
                                         'tools', 'foreach', 'iterators',
                                         'gdalUtils', 'RcppArmadillo', 
                                         'inline', 'abind'),
                             .combine=c) %dopar% {
        raster_tmpdir <- file.path(temp, paste0('raster_',
                                   paste(sample(c(letters, 0:9), 15), collapse='')))
        dir.create(raster_tmpdir)
        rasterOptions(tmpdir=raster_tmpdir)

        image_date_object <- as.Date(image_date_string, '_%Y-%j_')

        epoch_image_files <- image_files[grepl(image_date_string, image_files)]
        epoch_mask_files <- paste0(file_path_sans_ext(epoch_image_files), '_masks.tif')
        stopifnot(all(file_test('-f', epoch_image_files)))
        stopifnot(all(file_test('-f', epoch_mask_files)))

        if (imgtype == 'normalized') {
            mosaic_out_file <- file.path(output_dir,
                                         paste0(sitecode, '_mosaic_normalized_', 
                                                year(image_date_object),  
                                            extension(epoch_image_files[1])))
        } else {
            mosaic_out_file <- file.path(output_dir,
                                         paste0(sitecode, '_mosaic_', 
                                                year(image_date_object),  
                                            extension(epoch_image_files[1])))
        }
        if (file_test('-f', mosaic_out_file) & !reprocess) {
            return()
        }

        mask_out_file <- paste0(file_path_sans_ext(mosaic_out_file), '_masks', 
                                extension(mosaic_out_file))

        #######################################################################
        # Need to align images prior to mosaicking if origins are not identical
        image_origins <- lapply(epoch_mask_files, function(x) 
                                origin(raster(x)))
        image_origins_eq  <- lapply(image_origins, function(x) {
            identical(x, c(0, 0))
        })

        if (all(unlist(image_origins_eq))) {
            epoch_image_files_aligned <- epoch_image_files
            epoch_mask_files_aligned <- epoch_mask_files
        } else {
            aligned_files <- foreach (epoch_image_file=iter(epoch_image_files),
                                      epoch_mask_file=iter(epoch_mask_files),
                                      .combine=rbind) %do% {
                mask_vrtfile <- extension(rasterTmpFile(), '.vrt')
                # The hidenodata and vrtnodata lines below ensure that no data 
                # areas are coded 255 in the output mosaic (consistent with the 
                # Landsat CDR mask coding for fill area).
                gdalbuildvrt(epoch_mask_file, mask_vrtfile, vrtnodata=255, 
                             hidenodata=FALSE, te=mosaic_te, tr=c(30, 30))
                mask_tiffile <- extension(rasterTmpFile(), '.tif')
                gdalwarp(mask_vrtfile, dstfile=mask_tiffile, r='near', 
                         of='GTiff', overwrite=overwrite, ot='Byte', 
                         co="COMPRESS=LZW")

                image_tiffile <- extension(rasterTmpFile(), '.tif')
                gdalwarp(epoch_image_file, dstfile=image_tiffile, 
                         r='cubicspline', of='GTiff', overwrite=overwrite, 
                         te=mosaic_te, tr=c(30, 30), ot='Int16', 
                         co="COMPRESS=LZW BIGTIFF=IF_SAFER")
                return(data.frame(msk=mask_tiffile, img=image_tiffile, 
                                  stringsAsFactors=FALSE))
            }
            epoch_mask_files_aligned <- aligned_files$msk
            epoch_image_files_aligned <- aligned_files$img
        }

        #######################################################################
        # Mosaick the images

        # Define a function to determine the output value for a block of 
        # pixels, based on a stack of masks and reflectance from multiple 
        # overlapping images. Use RcppArmadillo for speed.
        src <- '
            using namespace arma;

            // Read input data into arrays
            Rcpp::NumericVector img_vecArray(img);
            Rcpp::IntegerVector img_arrayDims = img_vecArray.attr("dim");
            cube img_cube(img_vecArray.begin(), img_arrayDims[0], 
                          img_arrayDims[1], img_arrayDims[2], false);

            Rcpp::NumericVector msk_vecArray(msk);
            Rcpp::IntegerVector msk_arrayDims = msk_vecArray.attr("dim");
            cube msk_cube(msk_vecArray.begin(), msk_arrayDims[0], 
                          msk_arrayDims[1], msk_arrayDims[2], false);

            mat fillnofill_mat = msk_cube.tube(0, 0, msk_cube.n_rows - 1, 0);
            mat fmask_mat = msk_cube.tube(0, 1, msk_cube.n_rows - 1, 1);

            //Rcpp::Rcout << "fillnofill 0" << sum(fillnofill_mat == 0) << std::endl;
            //Rcpp::Rcout << "fillnofill 1" << sum(fillnofill_mat == 1) << std::endl;
            //Rcpp::Rcout << "fillnofill 2" << sum(fillnofill_mat == 2) << std::endl;
            //Rcpp::Rcout << "fillnofill 3" << sum(fillnofill_mat == 3) << std::endl;
            //Rcpp::Rcout << "fillnofill 4" << sum(fillnofill_mat == 4) << std::endl;
            //Rcpp::Rcout << "fillnofill 255" << sum(fillnofill_mat == 255) << std::endl;
            //Rcpp::Rcout << "fmask 0" << sum(fmask_mat == 0) << std::endl;
            //Rcpp::Rcout << "fmask 1" << sum(fmask_mat == 1) << std::endl;
            //Rcpp::Rcout << "fmask 2" << sum(fmask_mat == 2) << std::endl;
            //Rcpp::Rcout << "fmask 3" << sum(fmask_mat == 3) << std::endl;
            //Rcpp::Rcout << "fmask 4" << sum(fmask_mat == 4) << std::endl;
            //Rcpp::Rcout << "fmask 255" << sum(fmask_mat == 255) << std::endl;

            //Rcpp::Rcout << fillnofill_mat.n_rows << std::endl;
            //Rcpp::Rcout << fillnofill_mat.n_cols << std::endl;
            //Rcpp::Rcout << fmask_mat.n_rows << std::endl;
            //Rcpp::Rcout << fmask_mat.n_cols << std::endl;

            // Create output arrays
            // cols in mask are fill mask (col 0) and fmask (col 1)
            mat msk_out(msk_arrayDims[0], msk_arrayDims[1]);
            // cols in image are bands
            mat img_out(img_arrayDims[0], img_arrayDims[1]);

            // Make a uvec to track cloud/shadow/fill pixels
            uvec gooddata_indices(img_out.n_cols);
            for (unsigned pixelnum = 0; pixelnum < img_cube.n_rows; pixelnum++) {

                // Load the fill/not fill band just to mosaick it
                rowvec fillnofill_pixel = fillnofill_mat.row(pixelnum);

                // Load the fmask band and use this band for masking the image
                // during mosaicking
                rowvec fmask_pixel = fmask_mat.row(pixelnum);

                gooddata_indices = vectorise((fmask_pixel != 2) &&
                                             (fmask_pixel != 4) &&
                                             (fmask_pixel != 255));
                gooddata_indices = find(gooddata_indices);

                // Note that img_pixel is a mat with n_layers x n_images
                mat img_pixel = img_cube.tube(pixelnum, 0, pixelnum, img_out.n_cols - 1);

                if (gooddata_indices.n_elem > 0) {
                    //Rcpp::Rcout << "got here" << std::endl;
                    if (img_cube.n_slices == 1) {
                        img_out.row(pixelnum) = img_pixel;
                    } else {
                        img_out.row(pixelnum) = trans(mean(img_pixel.cols(gooddata_indices), 1));
                    }
                    // Take a conservative approach to merging masks - highest code 
                    // takes precedence (snow (3) over water (1) over clear (0)).
                    msk_out(pixelnum, 0) = 0;
                    msk_out(pixelnum, 1) = max(fmask_pixel(gooddata_indices));
                } else {
                    //Rcpp::Rcout << "got here 1" << std::endl;
                    img_out.row(pixelnum).fill(datum::nan);
                    // No data in this pixel. Use the lowest code in the
                    // output mosaic. This means cloud takes precedence
                    // over cloud shadow, which takes precedence over fill
                    msk_out(pixelnum, 0) = min(fillnofill_pixel);
                    msk_out(pixelnum, 1) = min(fmask_pixel);
                }
            }
             
            return Rcpp::List::create(Rcpp::Named("img") = img_out,
                                      Rcpp::Named("msk") = msk_out);
        '
        mosaic_block <- cxxfunction(signature(img="numeric", msk="numeric"),
                                     body=src, plugin="RcppArmadillo")

        # results <- mosaic_block(image_array, mask_array)
        #
        # plot(raster(matrix(results$img[, 1], nrow=bs$nrows[block_num], 
        #                    byrow=TRUE)))
        #
        # plot(raster(matrix(results$msk[, 1], nrow=bs$nrows[block_num], 
        #                    byrow=TRUE)))
        #
        # plot(raster(matrix(results$msk[, 2], nrow=bs$nrows[block_num], 
        #                    byrow=TRUE)))
        #
        # plot(raster(matrix(image_array[, 1, 1], nrow=bs$nrows[block_num], 
        #                    byrow=TRUE)))
        #
        # plot(raster(matrix(mask_array[, 1, 1], nrow=bs$nrows[block_num], 
        #                    byrow=TRUE)))
        #
        # plot(raster(matrix(mask_array[, 2, 1], nrow=bs$nrows[block_num], 
        #                    byrow=TRUE)))

        if (length(epoch_image_files_aligned) > 1) {
            epoch_mask_stacks <- lapply(epoch_mask_files_aligned, stack)
            epoch_image_stacks <- lapply(epoch_image_files_aligned, stack)
        } else {
            epoch_mask_stacks <- list(stack(epoch_mask_files_aligned))
            epoch_image_stacks <- list(stack(epoch_image_files_aligned))
        }
        sample_mask <- epoch_mask_stacks[[1]]
        sample_image <- epoch_image_stacks[[1]]

        mask_out <- brick(sample_mask, values=FALSE)
        # Set NAflag to 99 as a kludge - writeRaster doesn't allow omitting an 
        # NAflag, and I don't want 255 to be flagged as nodata
        mask_out <- writeStart(mask_out, filename=mask_out_file, 
                               overwrite=overwrite, datatype='INT1U', 
                               NAflag=99)
        image_out <- brick(sample_image, values=FALSE)
        image_out <- writeStart(image_out, filename=mosaic_out_file, 
                                overwrite=overwrite, datatype='INT2S')
        bs <- blockSize(sample_image)
        for (block_num in 1:bs$n) {
            image_dims <- c(bs$nrows[block_num], ncol(sample_image), 
                            nlayers(sample_image))
            mask_dims <- c(bs$nrows[block_num], ncol(sample_mask), 
                           nlayers(sample_mask))
            # Make image and mask arrays. Image array has nrow*ncol rows, 
            # n_layers columns, and n_images in z direction. Mask array has 
            # nrow*ncol rows, and n_images columns
            for (image_num in 1:length(epoch_image_files_aligned)) {
                image_bl <- array(getValuesBlock(epoch_image_stacks[[image_num]], 
                                                 row=bs$row[block_num], 
                                                 nrows=bs$nrows[block_num]),
                                  dim=c(image_dims[1] * image_dims[2], image_dims[3], 1))
                mask_bl <- array(getValuesBlock(epoch_mask_stacks[[image_num]], 
                                                row=bs$row[block_num], 
                                                nrows=bs$nrows[block_num]),
                                 dim=c(mask_dims[1] * mask_dims[2], mask_dims[3], 1))
                if (image_num == 1) {
                    image_array <- image_bl
                    mask_array <- mask_bl
                } else {
                    image_array <- abind(image_array, image_bl, along=3)
                    mask_array <- abind(mask_array, mask_bl, along=3)
                }
            }
            mosaicked_block <- mosaic_block(image_array, mask_array)
            mask_out <- writeValues(mask_out, mosaicked_block$msk, 
                                    bs$row[block_num])
            image_out <- writeValues(image_out, mosaicked_block$img, 
                                     bs$row[block_num])
        }
        mask_out <- writeStop(mask_out)
        image_out <- writeStop(image_out)

#         epoch_images <- lapply(epoch_image_files, brick)
#         epoch_masks <- lapply(epoch_mask_files, brick)
#
#         masked_epoch_image_files <- foreach(epoch_image=iter(epoch_images), 
#                                             epoch_mask=iter(epoch_masks), 
#                                             .packages=c('raster', 'rgdal', 
#                                                         'gdalUtils'),
#                                             .combine=c) %do% {
#             # Make sure clouds are masked out (NA) and make sure missing values 
#             # and SLC-off is masked out.
#             #
#             # Remember layer 2 is fmask
#             out_file <- extension(rasterTmpFile(), '.tif')
#             epoch_image <- overlay(epoch_image, epoch_mask[[2]], 
#                                    fun=function(img, msk) {
#                 img[(msk == 2) | (msk == 4) | (msk == 255)] <- NA 
#                 img[is.na(msk)] <- NA 
#                 return(img)
#             }, datatype=dataType(epoch_image)[1], filename=out_file)
#             return(out_file)
#         }
#
#         mask_vrtfile <- tempfile(fileext='.vrt')
#         # The hidenodata and vrtnodata lines below ensure that no data areas 
#         # are coded 255 in the output mosaic (consistent with the Landsat CDR 
#         # mask coding for fill area).
#         gdalbuildvrt(epoch_mask_files, mask_vrtfile, vrtnodata=255, 
#                      hidenodata=FALSE, te=mosaic_te, tr=c(30, 30))
#         mask_stack <- gdalwarp(mask_vrtfile, dstfile=mask_out_file, 
#                                r='near', output_Raster=TRUE, of='GTiff', 
#                                overwrite=overwrite, multi=TRUE, 
#                                wo=paste0("NUM_THREADS=", n_cpus), 
#                                ot='Byte', co="COMPRESS=LZW")
#
#         image_stack <- gdalwarp(masked_epoch_image_files,
#                                 dstfile=mosaic_out_file,
#                                 r='cubicspline', output_Raster=TRUE, 
#                                 of='GTiff',
#                                 overwrite=overwrite, multi=TRUE, 
#                                 wo=paste0("NUM_THREADS=", n_cpus), 
#                                 te=mosaic_te, tr=c(30, 30),
#                                 ot='Int16', co="COMPRESS=LZW")
        removeTmpFiles(h=0)
        unlink(raster_tmpdir)
    }

    # Check extents of all mosaics are equal
    if (imgtype == 'normalized') {
        pattern <- paste0('^', sitecode, '_mosaic_normalized_[0-9]{4}.tif$')
    } else {
        pattern <- paste0('^', sitecode, '_mosaic_[0-9]{4}.tif$')
    }
    mosaic_files <- dir(output_dir, pattern=pattern, full.names=TRUE)
    mosaic_stacks <- lapply(mosaic_files, stack)
    mos_exts <- lapply(mosaic_stacks, extent)
    for (mos_ext in mos_exts) {
        stopifnot(mos_ext == mos_exts[[1]])
    }

    dem_mosaic_filename <- file.path(output_dir,
                                     paste0(sitecode, '_mosaic_dem.tif'))
    if (builddem & (!file_test('-f', dem_mosaic_filename) | reprocess)) {
        message(paste0('Mosaicking DEMs for ', sitecode, '...'))
        
        mos_ext <- as(mos_exts[[1]], 'SpatialPolygons')

        proj4string(mos_ext) <- proj4string(mosaic_stacks[[1]])

        mos_ext_dem_proj <- spTransform(mos_ext, CRS(proj4string(dem_extents)))

        intersecting <- as.logical(gIntersects(dem_extents, 
                                               gUnaryUnion(mos_ext_dem_proj), byid=TRUE))
        if (sum(intersecting) == 0) {
            stop('no intersecting dem extents found')
        }

        dem_list <- dem_extents[intersecting, ]$filename
        dem_rasts <- lapply(dem_list, raster)

        to_srs <- proj4string(mosaic_stacks[[1]])

        # Calculate minimum bounding box coordinates:
        dem_te <- as.numeric(bbox(mos_ext))
        to_res <- c(30, 30)
        dem_mosaic <- gdalwarp(dem_list, dstfile=dem_mosaic_filename,
                               te=dem_te, t_srs=to_srs, tr=to_res, 
                               r='cubicspline', output_Raster=TRUE, multi=TRUE, 
                               of='GTiff', wo=paste0("NUM_THREADS=", n_cpus), 
                               overwrite=overwrite)

        # Note that the default output of 'terrain' is in radians
        slopeaspect <- terrain(dem_mosaic, opt=c('slope', 'aspect'))
        slopeaspect$aspect <- calc(slopeaspect$aspect, fun=function(vals) {
            vals[vals >= 2*pi] <- 0
            vals
            })
        # Note that slopeaspect is scaled - slope by 10000, and aspect by 1000 so 
        # that the layers can be saved as INT2S
        slopeaspect <- stack(round(raster(slopeaspect, layer=1) * 10000),
                             round(raster(slopeaspect, layer=2) * 1000))

        slopeaspect_mosaic_filename <- file.path(output_dir,
                                         paste0(sitecode, '_mosaic_slopeaspect.tif'))
        slopeaspect <- writeRaster(slopeaspect, filename=slopeaspect_mosaic_filename, 
                                 overwrite=overwrite, datatype='INT2S')
    }

    removeTmpFiles(h=0)
    unlink(raster_tmpdir)
}
notify('Finished mosaicking.')
#' A class for representing accuracy assessment results
#' @exportClass accuracy
#' @rdname accuracy-class
#' @slot ct a simple sample contingency table
#' @slot pop_ct a population contingency table (if \code{pop} was provided - 
#' see \code{\link{accuracy}})
#' @slot Q quantity disagreement
#' @slot A allocation disagreement
#' @slot n_test the number of samples
#' @slot pop the population of each class as a numeric
#' @import methods
setClass('accuracy', slots=c(ct='table', pop_ct='table', Q='numeric', 
                             A='numeric', n_test='numeric',
                             pop='numeric')
)

#' @export
#' @method summary accuracy
summary.accuracy <- function(object, ...) {
    obj = list()
    obj[['class']] <- class(object)
    obj[['Q']] <- object@Q
    obj[['A']] <- object@A
    obj[['ct']] <- object@ct
    obj[['pop_ct']] <- object@pop_ct
    obj[['n_test']] <- object@n_test
    margined_pop_ct <- .add_ct_margins(object@pop_ct)
    obj[['overall_acc']] <- margined_pop_ct[length(margined_pop_ct)]
    class(obj) <- 'summary.accuracy'
    obj
}

#' @export
#' @method print summary.accuracy
print.summary.accuracy <- function(x, ...) {
    cat(paste('Object of class "', x[['class']], '"\n', sep = ''))
    cat('\n')
    cat(paste('Testing samples:\t', x[['n_test']], '\n', sep = ''))
    cat('\n')
    cat('Sample contingency table:\n')
    print(.add_ct_margins(x[['ct']]))
    cat('\n')
    cat('Population contingency table:\n')
    print(.add_ct_margins(x[['pop_ct']]))
    cat('\n')
    cat(paste("Overall accuracy:\t", round(x[['overall_acc']], digits=4), "\n", sep = ""))
    cat('\n')
    cat(paste('Quantity disagreement:\t\t', round(x[['Q']], digits=4), '\n', sep = ''))
    cat(paste('Allocation disagreement:\t', round(x[['A']], digits=4), '\n', sep = ''))
    invisible(x)
}

#' @export
#' @method print accuracy
print.accuracy <- function(x, ...) {
    print(summary(x, ...))
}

setMethod("show", signature(object="accuracy"), function(object) print(object))

#' A class for error adjusted class areas
#'
#' @seealso \code{\link{adj_areas}}.
#' @import methods
#' @exportClass error_adj_area
setClass('error_adj_area', slots=c(adj_area_mat='matrix'))

#' Calculated adjusted class areas for an image classification
#'
#' Calculates the adjusted areas of each class in an image after taking account 
#' of omission and commission errors. For unbiased adjustments, error rates 
#' should be calculated using a population sample matrix (see 
#' \code{\link{accuracy}}.
#'
#' Standard errors for the adjusted areas are calculated as in Olofsson et al.  
#' (2013).
#' @export adj_areas
#' @param x an \code{accuracy} object or a list of populations as a 
#' \code{numeric}
#' @param y missing, or a contingency table
#' @references Olofsson, P., G. M. Foody, S. V. Stehman, and C. E. Woodcock.  
#' 2013. Making better use of accuracy data in land change studies: Estimating 
#' accuracy and area and quantifying uncertainty using stratified estimation.  
#' Remote Sensing of Environment 129:122-131.
setGeneric("adj_areas", function(x, y) standardGeneric("adj_areas"))

#' @rdname adj_areas
#' @aliases adj_areas,numeric,table-method
setMethod("adj_areas", signature(x="numeric", y="table"),
function(x, y) {
    pop <- x
    ct <- y
    Wi <- pop / sum(pop)
    adj_area_est <- sum(pop) * colSums(Wi * (ct / rowSums(ct)))
    # Calculate standard errors of the proportions
    std_err_p <- sqrt(colSums(Wi^2 *
                              (((ct / rowSums(ct))*(1 - ct / rowSums(ct))) /
                               (rowSums(ct) - 1))))
    # Now calculate standard error of adjusted area estimate
    std_err_area <- sum(pop) * std_err_p
    adj_area_mat <- cbind(pop, adj_area_est, std_err_area, 1.96 * std_err_area)
    adj_area_mat <- round(adj_area_mat, 0)
    dimnames(adj_area_mat)[[1]] <- dimnames(ct)[[1]]
    dimnames(adj_area_mat)[[2]] <- c('Mapped area', 'Adj. area', 'S.E.', 
                                     '1.96 * S.E.')
    return(new('error_adj_area', adj_area_mat=adj_area_mat))
})

#' @rdname adj_areas
#' @aliases adj_areas,numeric,matrix-method
setMethod("adj_areas", signature(x="numeric", y="matrix"),
function(x, y) {
    class(y) <- "table"
    adj_areas(x, y)
})

#' @rdname adj_areas
#' @aliases adj_areas,numeric,missing-method
setMethod("adj_areas", signature(x="accuracy", y='missing'),
function(x) {
    pop <- x@pop
    ct <- x@ct
    adj_areas(pop, ct)
})

setMethod("show", signature(object="error_adj_area"),
function(object) {
    cat('Object of class: error_adj_area\n')
    cat('Accuracy-adjusted area table:\n')
    print(object@adj_area_mat)
})


plot.error_adj_area <- function(x, ...) {
    classes <- dimnames(x@adj_area_mat)[[1]]
    areas <- x@adj_area_mat[, 2]
    se <- x@adj_area_mat[, 3]
    plt_data <- data.frame(x=classes, y=areas, se=se)
    y <- NULL # Fix for R CMD check
    ggplot(plt_data, aes(x, y)) + geom_bar(stat="identity") + 
        geom_errorbar(aes(ymin=y - 1.96 * se, ymax=y + 1.96 * se), width=.25) +
        xlab("Class") + ylab("Area")
}

.calc_pop_ct <- function(ct, pop) {
    # Below uses the notation of Pontius and Millones (2011)
    nijsum <- matrix(rowSums(ct), nrow=nrow(ct), ncol=ncol(ct))
    Ni <- matrix(pop, nrow=nrow(ct), ncol=ncol(ct))
    # pop_ct is the population contigency table
    pop_ct <- (ct / nijsum) * (Ni / sum(pop))
    dimnames(pop_ct)[[1]] <- dimnames(ct)[[1]]
    dimnames(pop_ct)[[2]] <- dimnames(ct)[[2]]
    class(pop_ct) <- 'table'
    return(pop_ct)
}

.calc_Q <- function(pop_ct) {
    # Calculate quantity disagreement (Pontius and Millones, 2011, eqns 2-3)
    qg_mat = abs(rowSums(pop_ct) - colSums(pop_ct))
    return(sum(qg_mat) / 2)
}

.calc_A <- function(pop_ct) {
    # Calculate allocation disagreement (Pontius and Millones, 2011, eqns 4-5)
    diag_indices <- which(diag(nrow(pop_ct)) == TRUE)
    ag_mat = 2 * apply(cbind(rowSums(pop_ct) - pop_ct[diag_indices],
                             colSums(pop_ct) - pop_ct[diag_indices]), 1, min)
    return(sum(ag_mat) / 2)
}

# Adds margins to contingency table
.add_ct_margins <- function(ct, digits=4) {
    # For user's, producer's, and overall accuracy formulas, see Table 
    # 21.3 in Foody, G.M., Stehman, S.V., 2009. Accuracy Assessment, in: 
    # Warner, T.A., Nellis, M.D., Foody, G.M. (Eds.), The SAGE Handbook of 
    # Remote Sensing. SAGE.
    diag_indices <- which(diag(nrow(ct)) == TRUE)
    users_acc <- ct[diag_indices] / colSums(ct)
    prod_acc <- ct[diag_indices] / rowSums(ct)
    overall_acc <- sum(ct[diag_indices]) / sum(ct)
    ct <- addmargins(ct)
    dimnames(ct)[[1]][nrow(ct)] <- "Total"
    dimnames(ct)[[2]][nrow(ct)] <- "Total"
    ct <- rbind(ct, Producers=c(users_acc, NA))
    ct <- cbind(ct, Users=c(prod_acc, NA, overall_acc))
    ct <- round(ct, digits=digits)
    dimnames(ct) <- list(predicted=dimnames(ct)[[1]],
                         observed=dimnames(ct)[[2]])
    class(ct) <- 'table'
    return(ct)
}

#' Calculate statistics summarizing classification accuracy
#'
#' Calculates a contingency table and various statistics for use in image 
#' classification accuracy assessment and map comparison. Contingency table 
#' includes user's, producer's, and overall accuracies for an image 
#' classification, and quantity disagreement \code{Q} and allocation 
#' disagreement \code{A}. \code{Q} and \code{A} are calculated based on Pontius 
#' and Millones (2011). Standard errors for 95 percent confidence intervals for 
#' the user's, producer's and overall accuracies are calculated as in Foody and 
#' Stehman (2009) Table 21.3. To avoid bias due to the use of a sample 
#' contingency table, the contingency table will be converted to a population 
#' contingency table if the variable 'pop' is provided. For an accuracy 
#' assessment using testing data from a simple random sample, 'pop' does not 
#' need to be provided (see Details).
#'
#' \code{x} can be one of:
#' \enumerate{
#'
#'   \item A prediction model as output from one of the \code{teamlucc} 
#'   \code{classify} functions. If \code{x} is a model, and testing data 
#'   is included in the model, \code{pop} and \code{test_data} can both be 
#'   missing, and accuracy will still run (though the output will in this case 
#'   be biased unless the testing data is from a simple random sample). If 
#'   \code{x} is a \code{RasterLayer}, then \code{test_data} must be supplied.
#'
#'   \item A \code{RasterLayer} with a predicted map.
#' }
#'
#' \code{test_data} can be one of:
#' \enumerate{
#'   \item \code{NULL}. If test_data is \code{NULL}, \code{accuracy} will try to use 
#'         testing data included in \code{x}. This will only work if \code{x}
#'         is a model of class \code{train} from the \code{caret} package, and 
#'         if the model was run using the one of the \code{teamlucc} 
#'         \code{classify} functions.
#'
#'   \item A \code{SpatialPolygonsDataFrame} object, in which case \code{accuracy} 
#'         will extract the predicted classes within each polygon from \code{x}.  
#'         This will only work if \code{x} is a \code{RasterLayer}.
#'
#'   \item A \code{pixel_data} object, in which case \code{accuracy} will use the 
#'         included \code{training_flag} indicator to separate testing and 
#'         training data.
#' }
#'
#' \code{pop} can be one of:
#' \enumerate{
#'   \item NULL, in which case the sample frequencies will be used as estimates 
#'         of the population frequencies of each class.
#'
#'   \item A list of length equal to the number of classes in the map giving 
#'         the total number of pixels in the population for each class.
#'
#'   \item A predicted cover map from as a \code{RasterLayer}, from which the 
#'         class frequencies will be tabulated and used as the population 
#'         frequencies.
#' }
#' @export accuracy
#' @param x either a classification model with a \code{predict} method or a 
#' \code{RasterLayer} (see Details)
#' @param test_data a \code{link{pixel_data}} object, 
#' \code{SpatialPolygonsDataFrame}, or NULL (see Details).
#' @param pop A \code{RasterLayer}, \code{numeric} of length equal to the 
#' number of clasess, or NULL (see Details).
#' @param class_col required if \code{test_data} is a 
#' \code{SpatialPolygonsDataFrame}. Defines the name of the column containing 
#' the observed cover class IDs
#' @param reclass_mat a reclassification matrix to be used in the case of a 
#' model fit by \code{classify} with the \code{do_split} option selected
#' @return \code{\link{accuracy-class}} instance
#' @references Pontius, R. G., and M. Millones. 2011. Death to Kappa: birth of 
#' quantity disagreement and allocation disagreement for accuracy assessment.  
#' International Journal of Remote Sensing 32:4407-4429.
#'
#' Olofsson, P., G. M. Foody, S. V. Stehman, and C. E. Woodcock.  2013. Making 
#' better use of accuracy data in land change studies: Estimating accuracy and 
#' area and quantifying uncertainty using stratified estimation.  Remote 
#' Sensing of Environment 129:122-131.
#'
#' Foody, G.M., Stehman, S.V., 2009. Accuracy Assessment, in: Warner, T.A., 
#' Nellis, M.D., Foody, G.M. (Eds.), The SAGE Handbook of Remote Sensing. SAGE.
#' @examples
#' \dontrun{
#' train_data <- get_pixels(L5TSR_1986, L5TSR_1986_2001_training, "class_1986", 
#'                          training=.6)
#' model <- train_classifier(train_data)
#' accuracy(L5TSR_1986_rfmodel)
#' }
setGeneric("accuracy", function(x, test_data, pop, class_col, reclass_mat) 
           standardGeneric("accuracy"))

#' @rdname accuracy
#' @aliases accuracy,train,ANY,ANY,missing,ANY-method
setMethod("accuracy", signature(x="train", test_data="ANY", pop="ANY", class_col="missing", reclass_mat="ANY"),
    function(x, test_data, pop, class_col, reclass_mat) {
        if (missing(test_data)) {
            test_data <- x$trainingData
            names(test_data)[names(test_data) == '.outcome'] <- 'y'
        } else {
            test_data <- cbind(y=test_data@y, 
                               test_data@x,
                               training_flag=test_data@training_flag)
        }
        if (!('training_flag' %in% names(test_data))) {
            warning('no training_flag variable found - assuming none of "test_data" was used for model training')
        } else if (sum(test_data$training_flag == 1) == length(test_data$training_flag)) {
            stop('cannot conduct accuracy assessment without independent testing data')
        }
        test_data <- test_data[!test_data$training_flag, ]
        complete_rows <- complete.cases(test_data)
        if (sum(complete_rows) != nrow(test_data)) {
            warning(paste('ignored', nrow(test_data) - sum(complete_rows), 
                          'rows because of missing data'))
            test_data <- test_data[complete.cases(test_data), ]
        }
        predicted <- predict(x, test_data)
        observed <- test_data$y

        calc_accuracy(predicted, observed, pop, reclass_mat)
    }
)

#' @rdname accuracy
#' @aliases accuracy,RasterLayer,pixel_data,ANY,missing,ANY-method
setMethod("accuracy", signature(x="RasterLayer", test_data="pixel_data", pop="ANY", class_col="missing", reclass_mat="ANY"),
    function(x, test_data, pop, class_col, reclass_mat) {
        if (all(test_data@training_flag == 1)) {
            stop('cannot conduct accuracy assessment without independent testing data')
        } else if (all(test_data@training_flag == 0)) {
            # All the test_data is for testing
            predicted <- extract(x, test_data@polys, small=TRUE, df=TRUE)[, 2]
            observed <- test_data@y
        } else {
            # Mix of testing and validation data
            predicted <- extract(x, test_data@polys[!test_data@training_flag], 
                                 small=TRUE, df=TRUE)[, 2]
            observed <- test_data@y[!test_data@training_flag]
        }
        predicted <- factor(predicted, labels=levels(observed))
        calc_accuracy(predicted, observed, pop, reclass_mat)
    }
)

#' @rdname accuracy
#' @aliases accuracy,RasterLayer,SpatialPolygonsDataFrame,ANY,character,ANY-method
setMethod("accuracy", signature(x="RasterLayer", test_data="SpatialPolygonsDataFrame", pop="ANY", class_col="character", reclass_mat="ANY"),
    function(x, test_data, pop, class_col, reclass_mat) {
        ext <- get_pixels(x, test_data, class_col=class_col)
        # Since x is the predicted image, the output of get_pixels gives 
        # the predicted value in slot x, and the observed value in slot y.  
        # However x is converted to a numeric from a factor, so it needs to be 
        # converted back to a factor with the same levels as y.
        observed <- ext@y
        predicted <- factor(ext@x[, ], labels=levels(ext@y))
        calc_accuracy(predicted, observed, pop, reclass_mat)
    }
)

calc_accuracy <- function(predicted, observed, pop, reclass_mat) {
    if (!missing(reclass_mat)) {
        stop('reclass_mat not yet supported')
    }

    # ct is the sample contigency table
    ct <- table(predicted, observed)

    if (missing(pop)) {
        warning('pop was not provided - assuming sample frequencies equal population frequencies')
        pop <- rowSums(ct)
    } else if (class(pop) == 'RasterLayer') {
        pop <- freq(pop, useNA='no')[, 2]
        if (length(pop) != nrow(ct)) {
            stop('number of classes in pop must be equal to nrow(ct)')
        }
    } else if (class(pop) %in% c('integer', 'numeric')) {
        if (length(pop) != nrow(ct)) {
            stop('length(pop) must be equal to number of classes in the predicted data')
        }
    } else { 
        stop('pop must be a numeric vector or integer vector of length equal to the number of classes in x, or a RasterLayer, or NULL')
    }

    pop_ct <- .calc_pop_ct(ct, pop)
    Q <- .calc_Q(pop_ct)
    A <- .calc_A(pop_ct)

    return(new("accuracy", ct=ct, pop_ct=pop_ct, Q=Q, A=A, 
               n_test=length(observed), pop=pop))
}
#' Apply a raster function with edge effects over a series of blocks
#'
#' This function can be useful when applying windowed functions over a raster, 
#' as with \code{glcm}. This function allows windows functions that have edge 
#' effects to be applied over a raster in block-by-block fashion.  
#' \code{apply_windowed} avoids the striping that would result if the edge 
#' effects were ignored.
#'
#' @export
#' @param x a \code{Raster*}
#' @param fun the function to apply
#' @param edge length 2 numberic with number of rows on top and bottom with 
#' edge effects, defined as c(top, bottom)
#' @param chunksize the number of rows to read per block (passed to 
#' \code{raster} \code{blockSize} function.
#' @param filename file on disk to save \code{Raster*} to (optional)
#' @param overwrite whether to overwrite any existing files (otherwise an error 
#' will be raised)
#' @param datatype the \code{raster} datatype to use
#' @param ... additional arguments to pass to \code{fun}
#' @examples
#' \dontrun{
#' L5TSR_1986_b1 <- raster(L5TSR_1986, layer=1)
#' min_x <- cellStats(L5TSR_1986_b1, 'min')
#' max_x <- cellStats(L5TSR_1986_b1, 'max')
#' apply_windowed(L5TSR_1986_b1, glcm, edge=c(1, 3), min_x=min_x, max_x=max_x)
#' }
apply_windowed <- function(x, fun, edge=c(0, 0), chunksize=NULL, filename='', 
                          overwrite=FALSE, datatype='FLT4S', ...) {
    if ((length(edge) != 2) || (class(edge) != 'numeric') || any(edge < 0)) {
        stop('edge must be a length 2 positive numeric')
    }

    if (is.null(chunksize)) {
        bs <- blockSize(x)
    } else {
        bs <- blockSize(x, chunksize)
    }
    n_blocks <- bs$n

    # bs_mod is the blocksize that will contain blocks that have been expanded 
    # to avoid edge effects
    bs_mod <- bs
    # Expand blocks to account for edge effects on the top:
    bs_mod$row[2:n_blocks] <- bs_mod$row[2:n_blocks] - edge[1]
    # Need to read additional rows from these blocks to avoid an offset
    bs_mod$nrows[2:n_blocks] <- bs_mod$nrows[2:n_blocks] + edge[1]

    # Read additional bottom rows to account for edge effects on the bottom:
    bs_mod$nrows[1:(n_blocks - 1)] <- bs_mod$nrows[1:(n_blocks - 1)] + edge[2]

    if (any(bs_mod$row < 1)) {
        stop('too many blocks to read without edge effects - try increasing chunksize')
    } else if (any((bs_mod$nrows + bs_mod$row - 1) > nrow(x))) {
        stop('too many blocks to read without edge effects - try increasing chunksize')
    }
    
    started_writes <- FALSE
    for (block_num in 1:bs$n) {
        this_block <- getValues(x, row=bs_mod$row[block_num], 
                                nrows=bs_mod$nrows[block_num],
                                format='matrix')
        out_block <- fun(this_block, ...)
        layer_names <- dimnames(out_block)[[3]]
        # Drop the padding added to top to avoid edge effects, unless we are 
        # really on the top of the image, where top edge effects cannot be 
        # avoided
        if ((block_num != 1) && (edge[1] > 0)) {
            out_block <- out_block[-(1:edge[1]), , ]
            # The below line is needed to maintain a 3 dimensional array, 
            # even when an n x m x 1 array is returned from 
            # calc_texture_full_image because a single statistic was chosen. 
            # Without the below line, removing a row will coerce the 3d array 
            # to a 2d matrix, and the bottom padding removal will fail as it 
            # references a 3d matrix).
            if (length(dim(out_block)) < 3) dim(out_block) <- c(dim(out_block), 1)
        }
        # Drop the padding added to bottom to avoid edge effects, unless we are 
        # really on the bottom of the image, where bottom edge effects cannot 
        # be avoided
        if ((block_num != n_blocks) && (edge[2] > 0)) {
            out_block <- out_block[-((nrow(out_block)-edge[2]+1):nrow(out_block)), , ]
            if (length(dim(out_block)) < 3) dim(out_block) <- c(dim(out_block), 1)
        }
        if (!started_writes) {
            # Setup an output raster with number of layers equal to the number 
            # of layers in out_block, and extent/resolution equal to extent and 
            # resolution of x
            if (dim(out_block)[3] == 1) {
                out <- raster(x)
            } else {
                out <- brick(stack(rep(c(x), dim(out_block)[3])), values=FALSE)
            }
            if (filename == '') filename <- rasterTmpFile()
            out <- writeStart(out, filename=filename, overwrite=overwrite, 
                              datatype=datatype)
            names(out) <- layer_names
            started_writes <- TRUE
        }
        # To write to a RasterBrick the out_block needs to be structured as 
        # a 2-d matrix with bands in columns and columns as row-major vectors
        if (dim(out_block)[3] == 1) {
            out_block <- aperm(out_block, c(3, 2, 1))
            out_block <- matrix(out_block, ncol=nrow(out_block))
        } else {
            out_block <- aperm(out_block, c(3, 2, 1))
            out_block <- matrix(out_block, ncol=nrow(out_block), byrow=TRUE)
        }
        out <- writeValues(out, out_block, bs$row[block_num])
    }
    out <- writeStop(out)

    return(out)
}

calc_glcm_edge <- function(shift, window) {
    if ((length(shift) == 2) && is.numeric(shift)) shift <- list(shift)
    if ((!(is.vector(shift) && all(lapply(shift, length) == 2)) &&
         !(is.matrix(shift) && ncol(shift) == 2)) ||
        !(all(floor(unlist(shift)) == unlist(shift)))) {
        stop('shift must be a list of length 2 integer vectors, or a 2 column matrix')
    }
    if (!is.matrix(shift)) {
        shift <- matrix(unlist(shift), ncol=2, byrow=TRUE)
    }
    neg_shifts <- shift[, 2][shift[, 2] < 0]
    pos_shifts <- shift[, 2][shift[, 2] > 0]
    if (length(neg_shifts) == 0) neg_shifts <- 0
    if (length(pos_shifts) == 0) pos_shifts <- 0
    return(c(abs(min(neg_shifts)) + ceiling(window[2] / 2) - 1,
             abs(max(pos_shifts)) + ceiling(window[2] / 2) - 1))
}
#' Calculate predictor layers for a classification
#'
#' This function automates the calculation of a layer stack of predictor layers 
#' to use in a land use and/or land cover classification. See Details for the 
#' output layers.
#' 
#' The layers in the output layer stack are listed below. Note that all the 
#' layers are rescaled so that they range between -32,767 and 32,767 (allowing 
#' them to be stored as 16 bit unsigned integers).
#'
#' \bold{Predictor layer stack:}
#' \tabular{ll}{
#'     Layer 1: \tab Band 1 reflectance \cr
#'     Layer 2: \tab Band 2 reflectance \cr
#'     Layer 3: \tab Band 3 reflectance \cr
#'     Layer 4: \tab Band 4 reflectance \cr
#'     Layer 5: \tab Band 5 reflectance \cr
#'     Layer 6: \tab Band 7 reflectance \cr
#'     Layer 7: \tab MSAVI2 \cr
#'     Layer 8: \tab GLCM mean (from MSAVI2) \cr
#'     Layer 9: \tab GLCM variance (from MSAVI2) \cr
#'     Layer 10: \tab GLCM dissimilarity (from MSAVI2) \cr
#'     Layer 11: \tab Elevation \cr
#'     Layer 12: \tab Slope (radians X 10000) \cr
#'     Layer 13: \tab Aspect (see below) \cr
#' }
#'
#' The aspect is recoded as:
#'
#' \bold{Aspect coding:}
#' \tabular{ll}{
#'     1: \tab north facing (0-45 degrees, 315-360 degrees) \cr
#'     2: \tab east facing (45-135 degrees) \cr
#'     3: \tab south facing (135-225 degrees) \cr
#'     4: \tab west facing (225-315 degrees) \cr
#' }
#' @export
#' @importFrom glcm glcm
#' @importFrom stringr str_extract
#' @param x path to a preprocessed image as output by 
#' \code{auto_preprocess_landsat} or \code{auto_cloud_fill}.
#' @param dem DEM \code{RasterLayer} as output by \code{auto_setup_dem}
#' @param slopeaspect \code{RasterStack} as output by \code{auto_setup_dem}
#' @param output_path the path to use for the output (optional - if NULL then 
#' output images will be saved alongside the input images in the same folder).
#' @param ext file extension to use when saving output rasters (determines 
#' output file format).
#' @param overwrite whether to overwrite existing files (otherwise an error 
#' will be raised)
#' @param ...  additional arguments passed to \code{\link{glcm}}, such as
#' \code{n_grey}, \code{window}, or \code{shift}
#' @param notify notifier to use (defaults to \code{print} function). See the 
#' \code{notifyR} package for one way of sending notifications from R. The 
#' \code{notify} function should accept a string as the only argument.
auto_calc_predictors <- function(x, dem, slopeaspect, output_path=NULL, 
                                 ext='tif', overwrite=FALSE, notify=print,
                                 ...) {
    if (!file_test("-f", x)) {
        stop(paste("input image", x, "does not exist"))
    }
    if (!is.null(output_path) && !file_test("-d", output_path)) {
        stop(paste(output_path, "does not exist"))
    }

    ext <- gsub('^[.]', '', ext)

    timer <- Track_time(notify)
    timer <- start_timer(timer, label='Predictor calculation')

    # Setup a regex to identify preprocessed images
    preproc_regex <- '^[a-zA-Z]{2,3}_[0-9]{3}-[0-9]{3}_[0-9]{4}-[0-9]{3}_L[457][ET]SR(_tc)?'

    # Update the basename to refer the chosen file
    image_basename <- basename(file_path_sans_ext(x))

    image_stack <- brick(x)

    if (is.null(output_path)) {
        output_path <- dirname(x)
    }

    mask_stack_file <- paste0(file_path_sans_ext(x), '_masks.', ext)
    if (!file_test('-f', mask_stack_file)) {
        mask_stack_file <- gsub(paste0('(_tc)?.', ext, '$'), paste0('_masks.', ext), x)
        if (file_test('-f', mask_stack_file)) {
            warning('using masks file with old format (pre v0.5) teamlucc naming')
        } else {
            stop('could not find masks file')
        }
    }
    mask_stack <- brick(mask_stack_file)
    image_mask <- calc(mask_stack[[2]], function(maskvals) {
        # Mask clouds, cloud shadow, and fill
        (maskvals == 2) | (maskvals == 4) | (maskvals == 255)
    })

    ######################################################################
    # Calculate additional predictor layers (MSAVI and textures)
    timer <- start_timer(timer, label='Calculating MSAVI2')
    MSAVI2_filename <- file.path(output_path,
                                 paste0(image_basename, '_MSAVI2.', ext))
    MSAVI2_layer <- MSAVI2(red=raster(image_stack, layer=3),
                           nir=raster(image_stack, layer=4))
    # Truncate MSAVI2 to range between 0 and 1, and scale by 10,000 so it 
    # can be saved as a INT2S
    MSAVI2_layer <- calc(MSAVI2_layer, fun=function(vals) {
            vals[vals > 1] <- 1
            vals[vals < 0] <- 0
            vals <- round(vals * 10000)
        }, filename=MSAVI2_filename, overwrite=overwrite, datatype="INT2S")
    timer <- stop_timer(timer, label='Calculating MSAVI2')

    timer <- start_timer(timer, label='Calculating GLCM textures')
    MSAVI2_glcm_filename <- file.path(output_path,
                                      paste0(image_basename, 
                                            '_MSAVI2_glcm.', ext))
    glcm_statistics <- c('mean', 'variance', 'homogeneity', 'contrast', 
                         'dissimilarity', 'entropy', 'second_moment', 
                         'correlation')
    MSAVI2_layer[image_mask] <- NA
    # Need to know window and shift to calculate edge for apply_windowed. So if 
    # they are not in the dotted args, assume the defaults (since glcm will use 
    # the defaults if these parameters are not supplied).
    dots <- list(...)
    if (!("window" %in% names(dots))) {
        dots$window <- c(3, 3)
    }
    if (!("shift" %in% names(dots))) {
        dots$shift <- c(1, 1)
    }
    edge <- calc_glcm_edge(dots$shift, dots$window)
    # Note the min_x and max_x are given for MSAVI2 that has been scaled by 
    # 10,000
    apply_windowed_args <- list(x=MSAVI2_layer, fun=glcm, edge=edge, min_x=0, 
                             max_x=10000, filename=MSAVI2_glcm_filename, 
                             overwrite=overwrite, statistics=glcm_statistics, 
                             na_opt='center')
    apply_windowed_args <- c(apply_windowed_args, dots)
    MSAVI2_glcm <- do.call(apply_windowed, apply_windowed_args)
    names(MSAVI2_glcm) <- paste('glcm', glcm_statistics, sep='_')
    timer <- stop_timer(timer, label='Calculating GLCM textures')

    if (!missing(slopeaspect)) {
        timer <- start_timer(timer, label='Processing slopeaspect')
        names(slopeaspect) <- c('slope', 'aspect')
        # Classify aspect into north facing, east facing, etc., recalling 
        # that the aspect is stored in radians scaled by 1000.
        #     1: north facing (0-45, 315-360)
        #     2: east facing (45-135)
        #     3: south facing (135-225)
        #     4: west facing (225-315)
        aspect_cut <- raster::cut(slopeaspect$aspect/1000,
                                  c(-1, 45, 135, 225, 315, 361)*(pi/180))
        # Code both 0-45 and 315-360 aspect as North facing (1)
        aspect_cut[aspect_cut == 5] <- 1
        names(aspect_cut) <- 'aspect'
        timer <- stop_timer(timer, label='Processing slopeaspect')
    }

    ######################################################################
    # Layer stack predictor layers:
    timer <- start_timer(timer, label='Writing predictors')
    predictors <- stack(raster(image_stack, layer=1),
                        raster(image_stack, layer=2),
                        raster(image_stack, layer=3),
                        raster(image_stack, layer=4),
                        raster(image_stack, layer=5),
                        raster(image_stack, layer=6),
                        MSAVI2_layer,
                        scale_raster(MSAVI2_glcm$glcm_mean),
                        scale_raster(MSAVI2_glcm$glcm_variance),
                        scale_raster(MSAVI2_glcm$glcm_dissimilarity))
    predictor_names <- c('b1', 'b2', 'b3', 'b4', 'b5', 'b7', 'msavi', 
                         'msavi_glcm_mean', 'msavi_glcm_variance', 
                         'msavi_glcm_dissimilarity')

    if (!missing(dem)) {
        predictors <- stack(predictors, dem)
        predictor_names <- c(predictor_names, 'elev')
    }
    if (!missing(slopeaspect)) {
        predictors <- stack(predictors, slopeaspect$slope, aspect_cut)
        predictor_names <- c(predictor_names, 'slope', 'aspect')
    }

    predictors_filename <- file.path(output_path,
                                     paste0(image_basename, '_predictors.', 
                                            ext))

    names(predictors) <- predictor_names
    predictors <- mask(predictors, image_mask, maskvalue=1, 
                       filename=predictors_filename, 
                       overwrite=overwrite, datatype='INT2S')
    names(predictors) <- predictor_names

    # Save a copy of the original masks file along with the predictors file, so 
    # the masks can be easily located later.
    predictors_mask_filename <- file.path(output_path,
                                          paste0(image_basename, 
                                                 '_predictors_masks.', ext))
    mask_stack <- writeRaster(mask_stack, filename=predictors_mask_filename, 
                              overwrite=overwrite, 
                              datatype=dataType(mask_stack)[1])

    timer <- stop_timer(timer, label='Writing predictors')

    timer <- stop_timer(timer, label='Predictor calculation')

    return(predictors)
}
#' Perform change detection for two Landsat CDR surface reflectance images
#'
#' This image automates the change detection process using the Change Vector 
#' Analysis in Posterior Probability Space (CVAPS) algorithm. The threshold for 
#' change/no-change mapping is determined using Huang's algorithm (see 
#' \code{\link{threshold}} or can be specified manually. First the images 
#' should be classified using the \code{auto_classify} function (or any other 
#' classification approach that yields per-pixel probabilities of class 
#' membership).
#'
#' @export
#' @param t1_classes cover classes as output from \code{auto_classify_image} 
#' for time 1 image
#' @param t1_probs per class probabilities as output from 
#' \code{auto_classify_image} for time 1 image
#' @param t2_probs per class probabilities as output from 
#' \code{auto_classify_image} for time 2 image
#' @param output_path the path to use for the output
#' @param output_basename the base filename for output files from 
#' \code{auto_chg_detect} (without an extension)
#' @param ext file extension to use when saving output rasters (determines 
#' output file format).
#' @param overwrite whether to overwrite existing files (otherwise an error
#' will be raised)
#' @param chg_threshold the threshold to use determining change and no-change 
#' areas from the change magnitude image (see \code{\link{chg_mag}}. If 
#' \code{NULL}, then \code{\link{threshold}} will be used to dermine this 
#' threshold value automatically. A threshold in the range of .75-1 is 
#' recommended as a starting point.
#' @param notify notifier to use (defaults to \code{print} function).  See the 
#' \code{notifyR} package for one way of sending notifications from R.  The 
#' \code{notify} function should accept a string as the only argument.
#' @return nothing - used for the side effect of performing change detection
#' @references Chen, J., X. Chen, X. Cui, and J. Chen. 2011. Change vector 
#' analysis in posterior probability space: a new method for land cover change 
#' detection.  IEEE Geoscience and Remote Sensing Letters 8:317-321.
auto_chg_detect <- function(t1_classes, t1_probs, t2_probs, output_path, 
                            output_basename, ext='tif', overwrite=FALSE, 
                            chg_threshold=NULL, notify=print) {
    if (!file_test("-d", output_path)) {
        stop(paste(output_path, "does not exist"))
    }

    ext <- gsub('^[.]', '', ext)

    timer <- Track_time(notify)
    timer <- start_timer(timer, label='Change detection')

    ###########################################################################
    # Calculate change magnitude and direction
    ###########################################################################
    timer <- start_timer(timer, label='Change magnitude and direction')

    chg_dir_filename <- file.path(output_path, paste0(output_basename, 
                                                     '_chgdir.', ext))
    chg_dir_image <- chg_dir(t1_probs, t2_probs, filename=chg_dir_filename, 
                             overwrite=overwrite)

    chg_mag_filename <- file.path(output_path, paste0(output_basename, 
                                                     '_chgmag.', ext))
    chg_mag_image <- chg_mag(t1_probs, t2_probs, filename=chg_mag_filename, 
                             overwrite=overwrite)

    timer <- stop_timer(timer, label='Change magnitude and direction')

    ###########################################################################
    # Calculate change trajectories
    ###########################################################################
    timer <- start_timer(timer, label='Change trajectories')

    if (is.null(chg_threshold)) chg_threshold <- threshold(chg_mag_image)
    
    notify(paste0('Using threshold=', chg_threshold))

    chg_traj_filename <- file.path(output_path,
                                   paste0(output_basename, '_chgtraj.', ext))
    chg_traj_out <- chg_traj(chg_mag_image, chg_dir_image, 
                             chg_threshold=chg_threshold, overwrite=overwrite, 
                             filename=chg_traj_filename)
    timer <- stop_timer(timer, label='Change trajectories')

    timer <- stop_timer(timer, label='Change detection')
}
#' Classify a preprocessed surface reflectance image
#'
#' First the image should be preprocessed using the \code{auto_preprocess} 
#' function. For Landsat CDR imagery, predictor layers can be generated using 
#' the \code{auto_generate_predictors} function.
#'
#' @export
#' @importFrom rgdal readOGR
#' @importFrom sp spTransform
#' @importFrom tools file_path_sans_ext
#' @param predictor_file a \code{Raster*} of predictor layers output by the 
#' \code{auto_preprocess} function or path to an image stack in a format 
#' readable by the \code{raster} package.
#' @param train_shp a file readable by readOGR with training polygons
#' @param output_path the path to use for the output
#' @param class_col the name of the column containing the response variable 
#' (for example the land cover type of each pixel)
#' @param training indicator of which polygons to use in training. Can be: 1) a 
#' string giving the name of a column indicating whether each polygon is to be 
#' used in training (column equal to TRUE) or in testing (column equal to 
#' FALSE), or 2) a logical vector of length equal to length(polys), or 3) a 
#' number between 0 and 1 indicating the fraction of the polygons to be 
#' randomly selected for use in training.
#' @param overwrite whether to overwrite existing files (otherwise an error 
#' will be raised)
#' @param notify notifier to use (defaults to \code{print} function). See the 
#' \code{notifyR} package for one way of sending notifications from R. The 
#' \code{notify} function should accept a string as the only argument.
#' @examples
#' #TODO: Add example
auto_classify <- function(predictor_file, train_shp, output_path, 
                          class_col="Poly_Type", training=.6, overwrite=FALSE, 
                          notify=print) {
    if (!file_test("-f", train_shp)) {
        stop(paste(train_shp, "does not exist"))
    }
    if (!file_test("-f", predictor_file)) {
        stop(paste(predictor_file, "does not exist"))
    }
    if (!file_test("-d", output_path)) {
        stop(paste(output_path, "does not exist"))
    }

    timer <- Track_time(notify)
    timer <- start_timer(timer, label='Running auto_classify')

    predictors <- brick(predictor_file)
    pred_rast_basename <- basename(file_path_sans_ext(predictor_file))

    train_polys <- readOGR(dirname(train_shp), basename(file_path_sans_ext(train_shp)))
    train_polys <- spTransform(train_polys, crs(predictors))
    train_data <- get_pixels(predictors, train_polys, class_col=class_col, 
                             training=training)

    timer <- start_timer(timer, label='Running classification')
    classification <- classify(predictors, train_data)
    model <- classification$model
    save(model, file=file.path(output_path, paste(pred_rast_basename, 
                                                  'predmodel.RData', sep='_')))
    writeRaster(classification$pred_classes,
                filename=file.path(output_path, paste(pred_rast_basename, 
                                                      'predclasses.tif', 
                                                      sep='_')),
                datatype='INT2S', overwrite=overwrite)
    writeRaster(scale_raster(classification$pred_probs),
                filename=file.path(output_path, paste(pred_rast_basename, 
                                                      'predprobs.tif', 
                                                      sep='_')),
                datatype='INT2S', overwrite=overwrite)
    timer <- stop_timer(timer, label='Running classification')

    # cls <- levels(train_data$y) 
    # cls <- data.frame(code=seq(1:length(cls)), name=cls)
    # color_image(classification$predclasses, cls,
    #             file.path(output_path, paste(pred_rast_basename, 
    #             'predclasses_colored.tif', sep='_')))

    # Perform accuracy assessment using an independent dataset:
    timer <- start_timer(timer, label='Running accuracy assessment')
    acc <- accuracy(classification$model, 
                    pop=classification$pred_classes)
    capture.output(summary(acc),
                   file=file.path(output_path, paste(pred_rast_basename, 'predacc.txt', sep='_')))
    timer <- stop_timer(timer, label='Running accuracy assessment')

    timer <- stop_timer(timer, label='Running auto_classify')
}
pct_clouds <- function(cloud_mask) {
    num_clouds <- cellStats(cloud_mask == 1, stat='sum', na.rm=TRUE)
    num_clear <- cellStats(cloud_mask == 0, stat='sum', na.rm=TRUE)
    return((num_clouds / (num_clouds + num_clear)) * 100)
}

#' Automated removal of clouds from Landsat CDR imagery
#'
#' Uses one of four cloud reomval algorithms (see \code{\link{cloud_remove}}) 
#' to remove thick clouds from Landsat imagery. In hilly areas, topographic 
#' correction should be done before cloud fill.
#'
#' The \code{auto_cloud_fill} function allows an analyst to automatically 
#' construct a cloud-filled image after specifying: \code{data_dir} (a folder 
#' of Landsat images), \code{wrspath} and \code{wrsrow} (the WRS-2 path/row to 
#' use), and \code{start_date} and \code{end_date} (a start and end date 
#' limiting the images to use in the algorithm).  The analyst can also 
#' optionally specify a \code{base_date}, and the \code{auto_cloud_fill} 
#' function will automatically pick the image closest to that date to use as 
#' the base image.
#' 
#' As the \code{auto_cloud_fill} function automatically chooses images for 
#' inclusion in the cloud fill process, it relies on having images stored on 
#' disk in a particular way, and currently only supports cloud fill for Landsat 
#' CDR surface reflectance images. To ensure that images are correctly stored 
#' on your hard disk, use the \code{\link{auto_preprocess_landsat}} function to 
#' extract the original Landsat CDR hdf files from the USGS archive. The 
#' \code{auto_preprocess_landsat} function will ensure that images are 
#' extracted and renamed properly so that they can be used with the 
#' \code{auto_cloud_fill} script.
#'
#' @export
#' @importFrom tools file_path_sans_ext
#' @importFrom lubridate as.duration new_interval
#' @importFrom stringr str_extract
#' @importFrom SDMTools ConnCompLabel
#' @param data_dir folder where input images are located, with filenames as 
#' output by the \code{\link{auto_preprocess_landsat}} function. This folder 
#' will be searched recursively for images (taking the below path/row, date, 
#' and topographic correction options into account).
#' @param wrspath World Reference System (WRS) path
#' @param wrsrow World Reference System (WRS) row
#' @param start_date start date of period from which images will be chosen to 
#' fill cloudy areas in the base image (as \code{Date} object)
#' @param end_date end date of period from which images will be chosen to fill 
#' cloudy areas in the the base image (as \code{Date} object)
#' @param base_date ideal date for base image (base image will be chosen as the 
#' image among the available images that is closest to this date). If NULL, 
#' then the base image will be the image with the lowest cloud cover.
#' @param out_name base filename (without an extension - see \code{ext} 
#' argument) for cloud filled image.  The mask file for the cloud filled image 
#' will be saved with the same name, with the added suffix "_mask".
#' @param tc if \code{TRUE}, use topographically corrected imagery as output by 
#' \code{auto_preprocess_landsat}. IF \code{FALSE} use bands 1-5 and 7 surface 
#' reflectance as output by \code{unstack_ledaps} or 
#' \code{auto_preprocess_landsat} (if \code{auto_preprocess_landsat} was also 
#' run with tc=FALSE).
#' @param ext file extension to use when searching for input rasters and when 
#' saving output rasters (determines output file format). Should match file 
#' extension of input rasters (and should most likely match the value chosen 
#' for \code{ext} when \code{auto_preprocess_landsat} was run).
#' @param sensors choose the sensors to include when selecting images (useful 
#' for excluding images from a particular satellite if desired). Can be any of 
#' "L4T", "L5T", "L7E", and/or "L8C".
#' @param img_type type of Landsat imagery to preprocess. Can be "CDR" for 
#' Landsat Climate Data Record (CDR) imagery in HDR format, or "L1T" for 
#' Standard Terrain Correction (Level 1T) imagery. Note that if L1T imagery is 
#' used, fmask must be run locally (see https://code.google.com/p/fmask) prior 
#' to using \code{auto_preprocess_landsat}.
#' @param threshold maximum percent cloud cover allowable in base image. Cloud 
#' fill will iterate until percent cloud cover in base image is below this 
#' value, or until \code{max_iter} iterations have been run
#' @param max_iter maximum number of times to run cloud fill script
#' @param notify notifier to use (defaults to \code{print} function).  See the 
#' \code{notifyR} package for one way of sending notifications from R.  The 
#' \code{notify} function should accept a string as the only argument.
#' @param verbose whether to print detailed status messages. Set to FALSE or 0 
#' for no status messages. Set to 1 for basic status messages. Set to 2 for 
#' detailed status messages.
#' @param overwrite whether to overwrite \code{out_name} if it already exists
#' @param ...  additional arguments passed to \code{\link{cloud_remove}}, such 
#' as \code{DN_min}, \code{DN_max}, \code{algorithm}, \code{byblock}, 
#' \code{verbose}, etc. See \code{\link{cloud_remove}} for details
#' @return a list with two elements: "filled", a \code{Raster*} object with 
#' cloud filled image, and "mask", a \code{RasterLayer} object with the cloud 
#' mask for the cloud filled image.
#' @references Zhu, X., Gao, F., Liu, D., Chen, J., 2012. A modified 
#' neighborhood similar pixel interpolator approach for removing thick clouds 
#' in Landsat images.  Geoscience and Remote Sensing Letters, IEEE 9, 521--525.  
#' doi:10.1109/LGRS.2011.2173290
auto_cloud_fill <- function(data_dir, wrspath, wrsrow, start_date, end_date, 
                            out_name, base_date=NULL, tc=TRUE, ext='tif',
                            sensors=c('L4T', 'L5T', 'L7E', 'L8C'), 
                            img_type="CDR", threshold=1, max_iter=5, 
                            notify=print, verbose=1, overwrite=FALSE, ...) {
    if (!file_test('-d', data_dir)) {
        stop('data_dir does not exist')
    }
    if (!file_test('-d', dirname(out_name))) {
        stop('output folder does not exist')
    }
    if (file_path_sans_ext(out_name) != out_name) {
        stop('out_name should not have a file extension')
    }

    ext <- gsub('^[.]', '', ext)

    output_file <- paste0(out_name, '.', ext)
    if (file_test('-f', output_file) & !overwrite) {
        stop(paste0('output file "', output_file, '" already exists'))
    }
    if (!all(sensors %in% c('L4T', 'L5T', 'L7E', 'L8C'))) {
        stop('"sensors" must be a list of one or more of: "L4T", "L5T", "L7E", "L8C"')
    }

    log_file <- file(paste0(out_name, '_log.txt'), open="wt")
    msg <- function(txt) {
        cat(paste0(txt, '\n'), file=log_file, append=TRUE)
        print(txt)
    }

    timer <- Track_time(msg)
    timer <- start_timer(timer, label='Cloud fill')

    stopifnot(class(start_date) == 'Date')
    stopifnot(class(end_date) == 'Date')

    wrspath <- sprintf('%03i', wrspath)
    wrsrow <- sprintf('%03i', wrsrow)

    # Find image files based on start and end dates
    prefix_re <- "^([a-zA-Z]*_)?"
    #pathrow_re <-"[012][0-9]{2}-[012][0-9]{2}"
    pathrow_re <- paste(wrspath, wrsrow, sep='-')
    date_re <-"((19)|(2[01]))[0-9]{2}-[0123][0-9]{2}"
    if (img_type == "CDR") {
        sensor_re <- paste0('(', paste0(paste0('(', sensors,')'), collapse='|'), ')', "SR")
    } else if (img_type == "L1T") {
        sensor_re <- paste0('(', paste0(paste0('(', sensors,')'), collapse='|'), ')', "L1T")
    } else {
        stop(paste(img_type, "is not a recognized img_type"))
    }
    if (tc) {
        suffix_re <- paste0('_tc.', ext, '$')
    } else {
        suffix_re <- paste0('.', ext, '$')
    }
    file_re <- paste0(prefix_re, paste(pathrow_re, date_re, sensor_re, 
                                       sep='_'), suffix_re)
    img_files <- dir(data_dir, pattern=file_re, recursive=TRUE)

    img_dates <- str_extract(basename(img_files), date_re)
    img_dates <- as.Date(img_dates, '%Y-%j')

    which_files <- which((img_dates >= start_date) &
                          (img_dates < end_date))
    img_dates <- img_dates[which_files]
    img_files <- file.path(data_dir, img_files[which_files])

    if (length(img_files) == 0) {
        stop('no images found - check date_dir, check wrspath, wrsrow, start_date, and end_date')
    } else if (length(img_files) < 2) {
        stop(paste('Only', length(img_files),
                   'image(s) found. Need at least two images to perform cloud fill'))
    }

    if (verbose > 0) {
        msg(paste('Found', length(img_files), 'image(s)'))
        timer <- start_timer(timer, label='Analyzing cloud cover in input images')
    }
    # Run QA stats
    fmasks <- list()
    fill_QAs <- list()
    imgs <- list()
    for (img_file in img_files) {
        masks_file <- paste0(file_path_sans_ext(img_file), '_masks.', ext)
        if (!file_test('-f', masks_file)) {
            masks_file <- gsub(suffix_re, paste0('_masks.', ext), img_file)
            if (file_test('-f', masks_file)) {
                warning('using masks file with old format (pre v0.5) teamlucc naming')
            } else {
                stop('could not find masks file')
            }
        }
        this_fill_QA <- raster(masks_file, band=1)
        fill_QAs <- c(fill_QAs, this_fill_QA)
        this_fmask <- raster(masks_file, band=2)
        fmasks <- c(fmasks, this_fmask)
        this_img <- stack(img_file)
        imgs <- c(imgs, stack(this_img))
    }
    
    compareRaster(imgs, res=TRUE, orig=TRUE)
    compareRaster(fmasks, res=TRUE, orig=TRUE)

    freq_table <- freq(stack(fmasks), useNA='no', merge=TRUE)
    # Convert frequency table to fractions
    freq_table[-1] <- freq_table[-1] / colSums(freq_table[-1], na.rm=TRUE)
    if (verbose > 0) {
        timer <- stop_timer(timer, label='Analyzing cloud cover in input images')
    }
    if (verbose > 0) {
        timer <- start_timer(timer, label='Calculating cloud masks')
    }

    # Find image that is either closest to base date, or has the maximum 
    # percent clear
    if (is.null(base_date)) {
        clear_row <- which(freq_table$value == 0)
        base_img_index <- which(freq_table[clear_row, -1] == 
                                max(freq_table[clear_row, -1]))
    } else {
        base_date_diff <- lapply(img_dates, function(x) 
                                 as.duration(new_interval(x, base_date)))
        base_date_diff <- abs(unlist(base_date_diff))
        base_img_index <- which(base_date_diff == min(base_date_diff))
        # Handle ties - two images that are the same distance from base date.  
        # Default to earlier image.
        if (length(base_img_index) > 1) {
            base_img_index <- base_img_index[1]
        }
    }

    # Save the original base image fmask so it can be used to recode the final 
    # cloud mask at the end of cloud filling
    base_fmask <- fmasks[[base_img_index]]
    base_fill_QA <- fill_QAs[[base_img_index]]

    # Convert masks to indicate: 0 = clear; 1 = cloud or shadow; 2 = fill
    #
    #   fmask_band key:
    #       0 = clear
    #       1 = water
    #       2 = cloud_shadow
    #       3 = snow
    #       4 = cloud
    #       255 = fill value
    calc_cloud_mask <- function(fmask, img) {
        # Code clouds and cloud shadows as 1
        ret <- (fmask == 2) | (fmask == 4)
        # Code fill as 2
        ret[fmask == 255] <- 2
        # Code other missing data that is not in fill areas as NA. The (ret != 
        # 1) test is necessary to ensures that only NAs that are NOT in clouds 
        # will be copied to the mask images (the assumption being that NAs in 
        # clouds should be marked as cloud and fill should be attempted).  This 
        # is necessary in case clouded areas in img are mistakenly coded NA 
        # (they should not be).
        ret[(ret != 1) & (ret != 2) & is.na(img)] <- NA
        return(ret)
    }
    for (n in 1:length(fmasks)) {
        fmasks[n] <- overlay(fmasks[[n]], imgs[[n]][[1]], fun=calc_cloud_mask, 
                             datatype=dataType(fmasks[[n]]))
    }

    base_img <- imgs[[base_img_index]]
    imgs <- imgs[-base_img_index]
    base_mask <- fmasks[[base_img_index]]
    fmasks <- fmasks[-base_img_index]

    base_img_date <- img_dates[base_img_index]
    img_dates <- img_dates[-base_img_index]

    if (verbose > 0) {
        msg(paste('Using image from', base_img_date, 'as base image.'))
    }

    if (verbose > 0) {
        timer <- stop_timer(timer, label='Calculating cloud masks')
    }

    if (verbose > 0) {
        timer <- start_timer(timer, label='Masking base image')
    }
    # Mask out clouds in base image. Save this image to disk so it is available 
    # even if no cloud fill is done (if the pct_clouds in this image is below 
    # the threshold).
    base_img <- overlay(base_img, base_mask,
        fun=function(base_vals, mask_vals) {
            # Set clouds/shadows to 0
            base_vals[mask_vals == 1] <- 0
            # Allow fill to be attempted in NA areas
            base_vals[is.na(base_vals)] <- 0
            # Set slc-off gaps and areas outside scene to NA
            base_vals[mask_vals == 2] <- NA
            return(base_vals)
        }, datatype=dataType(base_img[[1]]), 
        filename=extension(rasterTmpFile(), ext), overwrite=overwrite)

    cur_pct_clouds <- pct_clouds(base_mask)

    if (verbose > 0) {
        msg(paste0('Base image has ', round(cur_pct_clouds, 2), '% cloud cover before fill'))
    }

    if (verbose > 0) {
        timer <- stop_timer(timer, label='Masking base image')
    }

    n <- 0
    while ((cur_pct_clouds > threshold) & (n < max_iter) & (length(imgs) >= 1)) {
        if (verbose > 0) {
            timer <- start_timer(timer, label=paste('Fill iteration', n + 1))
        }

        # Calculate a raster indicating the pixels in each potential fill image 
        # that are available for filling pixels of base_img that are missing 
        # due to cloud contamination. Areas coded 1 are missing due to cloud or 
        # shadow in the base image and are available in the merge image. This 
        # will return a stack with number of layers equal to number of masks.
        fill_areas <- overlay(base_mask, stack(fmasks),
            fun=function(base_mask_vals, fill_mask_vals) {
                ret <- rep(NA, length(base_mask_vals))
                # Code cloudy in base, clear in fill as 1
                ret[(base_mask_vals == 1) & (fill_mask_vals == 0)] <- 1
                # Code clear in base, clear in fill as 0
                ret[(base_mask_vals == 0) & (fill_mask_vals == 0)] <- 0
                # Code NA in base, clear in fill as clouded, so these NAs will 
                # be filled if possible.
                ret[is.na(base_mask_vals) & (fill_mask_vals == 0)] <- 1
                # Ensure SLC-off gaps and background areas in each image are 
                # not filled:
                ret[(base_mask_vals == 2) | (fill_mask_vals == 2)] <- NA
                return(ret)
            }, datatype=dataType(base_mask))
        fill_areas_freq <- freq(fill_areas, useNA='no', merge=TRUE)
        # Below is necessary as for some reason when fill_areas is of length 
        # one, freq returns a matrix rather than a data.frame
        fill_areas_freq <- as.data.frame(fill_areas_freq)
        # Select the fill image with the maximum number of available pixels 
        # (counting only pixels in the fill image that are not ALSO clouded in 
        # the fill image)
        avail_fill_row <- which(fill_areas_freq$value == 1)
        if (length(avail_fill_row) == 0) {
            msg(paste('No fill pixels available. Stopping fill.'))
            break
        }
        # Remove the now unnecessary "value" column
        fill_areas_freq <- fill_areas_freq[!(names(fill_areas_freq) == 'value')]
        fill_img_index <- which(fill_areas_freq[avail_fill_row, ] == 
                                max(fill_areas_freq[avail_fill_row, ], na.rm=TRUE))
        if ((length(fill_img_index) == 0) ||
            (fill_areas_freq[avail_fill_row, fill_img_index] == 0)) {
            msg(paste('No fill pixels available. Stopping fill.'))
            break
        }

        fill_img <- imgs[[fill_img_index]]
        imgs <- imgs[-fill_img_index]
        base_img_mask <- fill_areas[[fill_img_index]]
        fmasks <- fmasks[-fill_img_index]
        fill_img_date <- img_dates[fill_img_index]
        img_dates <- img_dates[-fill_img_index]

        # Add numbered IDs to the cloud patches
        base_img_mask <- ConnCompLabel(base_img_mask)

        # Ensure dataType is properly set prior to handing off to IDL
        dataType(base_img_mask) <- 'INT2S'

        if (verbose > 0) {
            msg(paste0('Filling image from ', base_img_date,
                          ' with image from ', fill_img_date, '.'))
            timer <- start_timer(timer, label="Performing fill")
        }
        base_img <- cloud_remove(base_img, fill_img, base_img_mask, 
                                 out_name=extension(rasterTmpFile(), ext), 
                                 verbose=verbose, overwrite=TRUE, ...)
        # base_img <- cloud_remove(base_img, fill_img, base_img_mask, 
        #                          out_name=extension(rasterTmpFile(), ext), 
        #                          verbose=verbose, overwrite=TRUE, 
        #                          DN_min=DN_min, DN_max=DN_max, 
        #                          algorithm=algorithm, byblock=byblock)
        if (verbose > 0) {
            timer <- stop_timer(timer, label="Performing fill")
        }

        # Revise base mask to account for newly filled pixels
        base_mask <- overlay(base_mask, base_img[[1]],
            fun=function(mask_vals, filled_vals) {
                mask_vals[(mask_vals == 1) & (filled_vals != 0)] <- 0
                return(mask_vals)
            }, datatype=dataType(base_mask), 
            filename=extension(rasterTmpFile(), ext), overwrite=TRUE)

        cur_pct_clouds <- pct_clouds(base_mask)
        if (verbose > 0) {
            msg(paste0('Base image has ', round(cur_pct_clouds, 2),
                          '% cloud cover remaining'))
            timer <- stop_timer(timer, label=paste('Fill iteration', n + 1))
        }

        n <- n + 1
    }

    base_img <- writeRaster(base_img, filename=output_file, datatype="INT2S", 
                            overwrite=overwrite)

    # Recode base mask so final coding matches that of fmask (though cloud and 
    # cloud shadow are no longer differentiated)
    #   fmask_band key:
    #       0 = clear
    #       1 = water
    #       2 = cloud_shadow
    #       3 = snow
    #       4 = cloud
    #       255 = fill value
    #   base_mask key:
    #   	0 = clear
    #   	1 = cloud
    #   	2 = fill
    mask_output_file <- paste0(out_name, '_masks.', ext)
    filled_fmask <- overlay(base_mask, base_fmask,
        fun=function(after_fill, before_fill) {
            ret <- after_fill
            # Code clear after filling but water in fmask as water (1 in fmask)
            ret[(after_fill == 0) & (before_fill == 1)] <- 1
            # Code clear after filling but snow in fmask as snow (3 in fmask)
            ret[(after_fill == 0) & (before_fill == 3)] <- 3
            # Code cloudy after filling as cloud (4 in fmask)
            ret[after_fill == 1] <- 4
            # Code gap in fmask as gap (3 in fmask)
            ret[before_fill == 255] <- 255
            return(ret)
        }, datatype=dataType(base_mask))
    final_masks <- stack(base_fill_QA, filled_fmask)
    names(final_masks) <- c("fill_QA", "fmask")
    final_masks <- writeRaster(final_masks, datatype=dataType(base_mask), 
                               filename=mask_output_file, overwrite=TRUE)

    timer <- stop_timer(timer, label='Cloud fill')

    close(log_file)

    return(list(filled=base_img, mask=final_masks))
}
pct_gap <- function(gap_mask) {
    num_gap <- cellStats(gap_mask == 1, stat='sum', na.rm=TRUE)
    num_clear <- cellStats(gap_mask == 0, stat='sum', na.rm=TRUE)
    return((num_gap / num_clear) * 100)
}

#' Automated removal of gaps in SLC-off images using GNSPI
#'
#' Uses the GNSPI algorithm from Zhu et al. See \code{\link{fill_gaps}} for 
#' details.  In hilly areas, gap fill should be done after topographic 
#' correction.
#'
#' The \code{auto_gap_fill} function allows an analyst to automatically 
#' construct a gap-filled image after specifying: \code{data_dir} (a folder of 
#' Landsat images), \code{wrspath} and \code{wrsrow} (the WRS-2 path/row to 
#' use), and \code{start_date} and \code{end_date} (a start and end date 
#' limiting the images to use in the algorithm).  The analyst can also 
#' optionally specify a \code{base_date}, and the \code{auto_gap_fill} function 
#' will automatically pick the image closest to that date to use as the base 
#' image.
#' 
#' As the \code{auto_gap_fill} function automatically chooses images for 
#' inclusion in the gap fill process, it relies on having images stored on disk 
#' in a particular way. To ensure that images are correctly stored on your hard 
#' disk, use the \code{\link{auto_preprocess_landsat}} function to extract the 
#' original Landsat CDR hdf files from the USGS archive. The 
#' \code{auto_preprocess_landsat} function will ensure that images are 
#' extracted and renamed properly so that they can be used with the 
#' \code{auto_gap_fill} script.
#'
#' @export
#' @importFrom spatial.tools sfQuickInit sfQuickStop
#' @importFrom lubridate as.duration new_interval
#' @importFrom stringr str_extract
#' @importFrom SDMTools ConnCompLabel
#' @param data_dir folder where input images are located, with filenames as 
#' output by the \code{\link{auto_preprocess_landsat}} function. This folder 
#' will be searched recursively for images (taking the below path/row, date, 
#' and topographic correction options into account).
#' @param wrspath World Reference System (WRS) path
#' @param wrsrow World Reference System (WRS) row
#' @param start_date start date of period from which images will be chosen to 
#' fill cloudy areas in the base image (as \code{Date} object)
#' @param end_date end date of period from which images will be chosen to fill 
#' cloudy areas in the the base image (as \code{Date} object)
#' @param base_date ideal date for base image (base image will be chosen as the 
#' image among the available images that is closest to this date). If NULL, 
#' then the base image will be the image with the lowest cloud cover.
#' @param tc if \code{TRUE}, use topographically corrected imagery as output by 
#' \code{auto_preprocess_landsat}. IF \code{FALSE} use bands 1-5 and 7 surface 
#' reflectance as output by \code{unstack_ledaps} or 
#' \code{auto_preprocess_landsat} (if \code{auto_preprocess_landsat} was also 
#' run with tc=FALSE).
#' @param threshold maximum percent gap allowable in base image. Gap fill will 
#' not occur unless percent gap in base image is greater than this value.
#' @param n_cpus the number of CPUs to use for processes that can run in 
#' parallel
#' @param notify notifier to use (defaults to \code{print} function).  See the 
#' \code{notifyR} package for one way of sending notifications from R.  The 
#' \code{notify} function should accept a string as the only argument.
#' @param verbose whether to print detailed status messages
#' @param ... additional arguments passed to \code{\link{fill_gaps}}, such as 
#' \code{DN_min}, \code{DN_max}, \code{use_IDL}, \code{verbose}, etc. See 
#' \code{\link{fill_gaps}}.
#' @return \code{Raster*} object with gap filled image.
#' @references Zhu, X., Liu, D., Chen, J., 2012. A new geostatistical approach 
#' for filling gaps in Landsat ETM+ SLC-off images. Remote Sensing of 
#' Environment 124, 49--60.
auto_gap_fill <- function(data_dir, wrspath, wrsrow, start_date, end_date, 
                          base_date=NULL, tc=TRUE, threshold=1, n_cpus=1, 
                          notify=print, verbose=TRUE, ...) {

    stop('auto_gap_fill not yet supported')

    if (!file_test('-d', data_dir)) {
        stop('data_dir does not exist')
    }
    timer <- Track_time(notify)
    timer <- start_timer(timer, label='Gap fill')

    if (n_cpus > 1) sfQuickInit(n_cpus)

    wrspath <- sprintf('%03i', wrspath)
    wrsrow <- sprintf('%03i', wrsrow)

    # Find image files based on start and end dates
    prefix_re <- "^([a-zA-Z]*_)?"
    #pathrow_re <-"[012][0-9]{2}-[012][0-9]{2}"
    pathrow_re <- paste(wrspath, wrsrow, sep='-')
    date_re <-"((19)|(2[01]))[0-9]{2}-[0123][0-9]{2}"
    sensor_re <-"((L[45]T)|(L7E)|(L8C))SR"
    if (tc) {
        suffix_re <- '_tc.tif$'
    } else {
        suffix_re <- '.tif$'
    }
    file_re <- paste0(prefix_re, paste(pathrow_re, date_re, sensor_re, 
                                       sep='_'), suffix_re)
    img_files <- dir(data_dir, pattern=file_re, recursive=TRUE)

    img_dates <- str_extract(basename(img_files), date_re)
    img_dates <- as.Date(img_dates, '%Y-%j')

    which_files <- which((img_dates >= start_date) &
                          (img_dates < end_date))
    img_dates <- img_dates[which_files]
    img_files <- file.path(data_dir, img_files[which_files])

    if (length(img_files) == 0) {
        stop('no images found - check date_dir, check wrspath, wrsrow, start_date, and end_date')
    } else if (length(img_files) <= 2) {
        stop(paste('Only', length(img_files),
                   'image(s) found. Need at least two images to perform gap fill'))
    }

    if (verbose) {
        notify(paste('Found', length(img_files), 'image(s)'))
        timer <- start_timer(timer, label='Analyzing cloud cover and gaps in input images')
    }
    # Run QA stats - remember band 1 is fmask band, and band 2 is fill_QA
    masks <- list()
    imgs <- list()
    for (img_file in img_files) {
        masks_file <- gsub(suffix_re, '_masks.tif', img_file)
        this_mask <- raster(masks_file, band=2)
        masks <- c(masks, this_mask)
        this_img <- stack(img_file)
        imgs <- c(imgs, stack(this_img))
    }
    freq_table <- freq(stack(masks), merge=TRUE)
    # Convert frequency table to fractions
    freq_table[-1] <- freq_table[-1] / colSums(freq_table[-1], na.rm=TRUE)
    if (verbose) {
        timer <- stop_timer(timer, label='Analyzing cloud cover and gaps in input images')
    }

    # Find image that is either closest to base date, or has the maximum 
    # percent not in cloud or gap
    if (is.null(base_date)) {
        clear_row <- which(is.na(freq_table$value))
        base_img_index <- which(freq_table[clear_row, -1] == 
                                max(freq_table[clear_row, -1]))
    } else {
        base_date_diff <- lapply(img_dates, function(x) 
                                 as.duration(new_interval(x, base_date)))
        base_date_diff <- abs(unlist(base_date_diff))
        base_img_index <- which(base_date_diff == min(base_date_diff))
    }

    # Convert masks to binary indicating: 0=other; 1=gap, shadow, or cloud.  
    # Note that gaps are coded as NAs in the fmask band.
    #
    #   band1: fmask_band
    #       0 = clear
    #       1 = water
    #       2 = cloud_shadow
    #       3 = snow
    #       4 = cloud
    #       NA = gap or background
    #   band 2: fill_QA
    #      	0 = not fill
    #    	255 = fill
    for (n in 1:length(masks)) {
        masks[n] <- (is.na(masks[[n]])) | (masks[[n]] == 2) | (masks[[n]] == 4)
    }
    # Code areas of imgs that are background, gap, cloud, or shadow as 0
    for (n in 1:length(imgs)) {
        imgs[n][masks[[1]] == 1] <- 0
    }

    base_img <- imgs[[base_img_index]]
    imgs <- imgs[-base_img_index]
    base_mask <- masks[[base_img_index]]
    masks <- masks[-base_img_index]

    base_img_date <- img_dates[base_img_index]
    img_dates <- img_dates[-base_img_index]

    # Save base_img in filled so it will be returned if base_img already has 
    # pct_gap below threshold
    start_pct_gap <- pct_gap(base_mask)
    if (verbose) {
        notify(paste0('Base image has ', round(start_pct_gap, 2), '% gap before fill'))
    }

    if (start_pct_gap > threshold) {
        if (verbose) {
            timer <- start_timer(timer, label='Performing gap fill')
        }

        # Calculate a raster indicating the pixels in each potential fill image 
        # that are available for filling pixels of base_img that are missing 
        # due to SLC-off gaps. Areas coded 1 are missing due to gaps in the 
        # base image and are available (i.e. are not gaps or clouds) in the 
        # merge image.
        fill_areas <- list()
        for (mask_img in masks) {
            fill_areas <- c(fill_areas, list(base_mask == 1 & mask_img == 0))
        }
        fill_areas_freq <- freq(stack(fill_areas), useNA='no', merge=TRUE)

        # Select the fill image with the maximum number of available pixels 
        # (counting only pixels in the fill image that are not ALSO in gaps or 
        # clouded in the fill image)
        avail_fill_row <- which(fill_areas_freq$value == 1)
        fill_img_index <- which(fill_areas_freq[avail_fill_row, -1] == 
                                max(fill_areas_freq[avail_fill_row, -1]))
        fill_img <- imgs[[fill_img_index]]
        imgs <- imgs[-fill_img_index]
        cloud_mask <- fill_areas[[fill_img_index]]
        fill_img_mask <- masks[[fill_img_index]]
        masks <- masks[-fill_img_index]

        fill_img_date <- img_dates[fill_img_index]
        img_dates <- img_dates[-fill_img_index]

        # Mark areas of the cloud_mask where fill_img is blank (clouded) with 
        # -1
        coded_cloud_mask[fill_img_mask] <- -1
        NAvalue(coded_cloud_mask) <- -2

        if (verbose) {
            notify(paste0('Filling image from ', base_img_date,
                          ' with image from ', fill_img_date, 'as input image...'))
        }
        filled <- fill_gaps(base_img, fill_img, imgs, verbose=verbose, ...)
        if (verbose) {
            notify('Fill complete.')
        }

        # Revise base mask to account for newly filled pixels
        # TODO: Fix this
        base_mask[coded_cloud_mask >= 1] <- 0

        max_iter <- max_iter + 1

        if (verbose) {
            final_pct_gap <- pct_gap(base_mask)
            notify(paste0('Base image has ', round(final_pct_gap, 2), '% gap remaining'))
            timer <- stop_timer(timer, label='Performing gap fill')
        }
    } else {
        notify('Percent gap < threshold. Skipping gap fill.')
    }

    timer <- stop_timer(timer, label='Gap fill')

    if (n_cpus > 1) sfQuickStop(n_cpus)

    return(filled)
}
#' Normalize a set of preprocessed CDR images to a base image
#'
#' This function uses model II regression to perform relative normalization to 
#' match a set of Landsat CDR surface reflectance images. The function assumes 
#' the images were preprocessed using the \code{auto_preprocess_landsat} 
#' function. A base image can be optionally supplied. If a base image is not 
#' supplied, then the function will calculate the percent cloud cover of each 
#' input image, and automatically choose the image with the least cloud cover 
#' as the base image. This function assumes that the images (and image masks) 
#' were preprocessed using the \code{auto_preprocess_landsat} function.
#'
#' This function will run in parallel if a parallel backend is registered with 
#' \code{\link{foreach}}.
#'
#' @export
#' @import foreach
#' @importFrom tools file_path_sans_ext
#' @param image_files list of filenames for images to normalize
#' @param base (optional) filename of base image. If not supplied, the base 
#' image will be automatically chosen from the images in \code{image_files}, as 
#' the image with the lowest percent cloud cover.
#' @param overwrite whether to overwrite existing files
#' @return nothing - used for side effect of normalizing imagery
auto_normalize <- function(image_files, base, overwrite=FALSE) {
    stopifnot(length(image_files) >= 1)

    image_stacks <- lapply(image_files, stack)

    mask_files <- paste0(file_path_sans_ext(image_files), '_masks', 
                         extension(image_files))
    mask_stacks <- lapply(mask_files, stack)

    if (!missing(base)) {
        base_img_file <- base
    } else if (missing(base) & (length(image_files) == 1)) {
        stop('length of image_files is 1 but no base image was supplied')
    } else {
        # Figure out which image has lowest percent cloud cover - use that 
        # image as the base image
        pct_clouds <- function(cloud_mask) {
            clouded_pixels <- calc(cloud_mask, fun=function(vals) {
                # For fmask layer, 2 is cloud shadow, and 4 is cloud
                (vals == 2) | (vals == 4)
            })
            num_clouds <- cellStats(clouded_pixels, stat='sum', na.rm=TRUE)
            # For fmask layer, 255 is fill
            num_clear <- cellStats(cloud_mask != 255, stat='sum', na.rm=TRUE)
            return((num_clouds / (num_clouds + num_clear)) * 100)
        }

        cloud_cover <- foreach(mask_stack=iter(mask_stacks),
                 .packages=c('teamlucc', 'stringr', 'rgdal'),
                 .combine=c) %dopar% {
            # Note that fmask layer is 2nd layer in stack
            pct_clouds(mask_stack[[2]])
        }
        base_index <- which(cloud_cover == min(cloud_cover))
        
        base_img <- image_stacks[[base_index]]
        image_stacks <- image_stacks[-base_index]

        base_img_file <- image_files[[base_index]]
        image_files <- image_files[-base_index]

        base_mask <- mask_stacks[[base_index]]
        mask_stacks <- mask_stacks[-base_index]
    }

    # Copy the base image to a new file with the _base.tif extension
    base_copy_filename <- paste0(file_path_sans_ext(base_img_file), 
                                 '_normbase.tif')
    base_img <- writeRaster(base_img, filename=base_copy_filename, 
                            datatype=dataType(base_img)[1], 
                            overwrite=overwrite)
    base_mask_copy_filename <- paste0(file_path_sans_ext(base_img_file), 
                                      '_normbase_masks.tif')
    base_mask <- writeRaster(base_mask, filename=base_mask_copy_filename, 
                             datatype=dataType(base_mask)[1], 
                             overwrite=overwrite)

    stopifnot(length(image_files) == length(image_stacks))
    stopifnot(length(image_files) == length(mask_stacks))

    image_file=image_stack=NULL
    # Now normalize each remaining image to the _base.tif file
    foreach (image_file=iter(image_files), image_stack=iter(image_stacks), 
             mask_stack=iter(mask_stacks),
             .packages=c('teamlucc', 'stringr', 'tools')) %dopar% {
        message(paste('Preprocessing ', image_file))
        output_normed_file <- paste0(file_path_sans_ext(image_file), 
                                     '_normalized.tif')
        output_normed_masks_file <- paste0(file_path_sans_ext(image_file), 
                                           '_normalized_masks.tif')

        # Note that fmask layer is 2nd layer in stack
        missing_vals <- overlay(base_mask[[2]], mask_stack[[2]],
                            fun=function(base_vals, this_vals) {
            # Only use clear pixels when normalizing (0 in fmask)
            (base_vals != 0) & (this_vals != 0)
        }, datatype=dataType(base_mask))

        if (ncell(image_stack) > 500000) {
            size <- 500000
        } else {
            size <- ncell(image_stack)
        }
        normed_image <- normalize(base_img, image_stack, missing_vals, size=size)

        normed_image <- writeRaster(normed_image, filename=output_normed_file, 
                                    datatype=dataType(base_img)[1], 
                                    overwrite=overwrite)
        mask_stack <- writeRaster(mask_stack, 
                                  filename=output_normed_masks_file, 
                                  datatype=dataType(mask_stack)[1], 
                                  overwrite=overwrite)
    }
}
get_gdalinfo_item <- function(item, gdalinfo_text) {
    gdalinfo_text <- gdalinfo_text[grepl(paste0('^[ ]*', item), gdalinfo_text)]
    if (length(gdalinfo_text) > 1) stop('more than one item found')
    gdalinfo_text <- gsub(paste0('[ ]*', item, '='), '', gdalinfo_text)
    return(gdalinfo_text)
}

get_mtl_item <- function(item, mtl_txt) {
    mtl_txt <- mtl_txt[grepl(paste0('^[ ]*', item), mtl_txt)]
    if (length(mtl_txt) > 1) stop('more than one item found')
    mtl_txt <- gsub(paste0('[ ]*', item, ' = '), '', mtl_txt)
    # Strip leading/following quotes
    mtl_txt <- gsub('^"', '', mtl_txt)
    mtl_txt <- gsub('"$', '', mtl_txt)
    return(mtl_txt)
}

#' @importFrom stringr str_extract
#' @importFrom gdalUtils gdalinfo
get_metadata <- function(ls_file, img_type) {
    meta <- list()
    if (img_type == "CDR") {
        ls_file_gdalinfo <- gdalinfo(ls_file)
        aq_date <- get_gdalinfo_item('AcquisitionDate', ls_file_gdalinfo)
        meta$aq_date <- strptime(aq_date, format="%Y-%m-%dT%H:%M:%OSZ", tz="UTC")
        meta$WRS_Path <- sprintf('%03i', as.numeric(get_gdalinfo_item('WRS_Path', ls_file_gdalinfo)))
        meta$WRS_Row <- sprintf('%03i', as.numeric(get_gdalinfo_item('WRS_Row', ls_file_gdalinfo)))
        meta$sunelev <- 90 - as.numeric(get_gdalinfo_item('SolarZenith', ls_file_gdalinfo))
        meta$sunazimuth <- as.numeric(get_gdalinfo_item('SolarAzimuth', ls_file_gdalinfo))
        meta$short_name  <- get_gdalinfo_item('ShortName', ls_file_gdalinfo)
    } else if (img_type == "L1T") {
        if (!grepl("_MTL.txt$", ls_file)) {
            stop("ls_file must be a *_MTL.txt file")
        }
        mtl_txt <- readLines(ls_file, warn=FALSE)
        aq_date <- get_mtl_item('DATE_ACQUIRED', mtl_txt)
        aq_time <- get_mtl_item('SCENE_CENTER_TIME', mtl_txt)
        meta$aq_date <- strptime(paste0(aq_date, "T", aq_time), format="%Y-%m-%dT%H:%M:%OSZ", tz="UTC")
        meta$WRS_Path <- sprintf('%03i', as.numeric(get_mtl_item('WRS_PATH', mtl_txt)))
        meta$WRS_Row <- sprintf('%03i', as.numeric(get_mtl_item('WRS_ROW', mtl_txt)))
        meta$sunelev <- as.numeric(get_mtl_item('SUN_ELEVATION', mtl_txt))
        meta$sunazimuth <- as.numeric(get_mtl_item('SUN_AZIMUTH', mtl_txt))
        # Build a shortname based on satellite and img_type that is consistent 
        # with the format of the CDR image shortnames
        satellite <- str_extract(get_mtl_item('SPACECRAFT_ID', mtl_txt), '[4578]')
        sensor_string <- str_extract(basename(ls_file), '^((LT[45])|(LE7)|(LC8))')
        meta$short_name  <- paste0(substr(sensor_string, 1, 1),
                                   substr(sensor_string, 3, 3),
                                   substr(sensor_string, 2, 2), img_type)
    } else {
        stop(paste(img_type, "is not a recognized img_type"))
    }
    return(meta)
}

calc_cloud_mask <- function(mask_stack, mask_type, ...) {
    if (mask_type == 'fmask') {
        # Make a mask where clouds and gaps are coded as 1, clear as 0
        # fmask_band key:
        # 	0 = clear
        # 	1 = water
        # 	2 = cloud_shadow
        # 	3 = snow
        # 	4 = cloud
        # 	255 = fill value
        cloud_mask <- calc(mask_stack$fmask_band,
            fun=function(fmask) {
                return((fmask == 2) | (fmask == 4) | (fmask == 255))
            }, datatype='INT2S', ...)
    } else if (mask_type == '6S') {
        # This cloud mask includes the cloud_QA, cloud_shadow_QA, and 
        # adjacent_cloud_QA layers. Pixels in cloud, cloud shadow, or 
        # adjacent cloud are coded as 1.
        cloud_mask <- overlay(mask_stack$fill_QA,
                              mask_stack$cloud_QA, 
                              mask_stack$cloud_shadow_QA, 
                              mask_stack$adjacent_cloud_QA,
            fun=function(fill, clo, sha, adj) {
                return((fill == 255) | (clo == 255) | (sha == 255) | 
                       (adj == 255))
            }, datatype='INT2S', ...)

    } else if (mask_type == 'both') {
        cloud_mask <- overlay(mask_stack$fmask_band, 
                              mask_stack$cloud_QA, 
                              mask_stack$cloud_shadow_QA, 
                              mask_stack$adjacent_cloud_QA,
            fun=function(fmask, clo, sha, adj) {
                return((fmask == 2) | (fmask == 4) | (fmask == 255) | 
                       (clo == 255) | (sha == 255) | (adj == 255))
            }, datatype='INT2S', ...)
    } else {
        stop(paste0('unrecognized option "', cloud_mask, '" for mask_type"'))
    }
    return(cloud_mask)
}

#' @importFrom gdalUtils get_subdatasets gdalbuildvrt
build_band_vrt <- function(ls_file, band_vrt_file, img_type) {
    image_bands <- c('band1', 'band2', 'band3', 'band4', 'band5', 'band7')
    if (img_type == "CDR") {
        sds <- get_subdatasets(ls_file)
        band_sds <- sds[grepl(paste0(':(', paste(image_bands, collapse='|'), ')$'), sds)]
        gdalbuildvrt(band_sds, band_vrt_file, separate=TRUE)
    } else if (img_type == "L1T") {
        if (!grepl("_MTL.txt$", ls_file)) {
            stop("ls_file must be a *_MTL.txt file")
        }
        ls_file_base <- gsub("_MTL.txt", "", ls_file)
        ls_files <- dir(dirname(ls_file_base),
                        pattern=paste0(basename(ls_file_base), '_B[123457].((TIF)|(tif))$'),
                        full.names=TRUE)
        gdalbuildvrt(ls_files, band_vrt_file, separate=TRUE)

    } else {
        stop(paste(img_type, "is not a recognized img_type"))
    }
    return(image_bands)
}

#' @importFrom gdalUtils get_subdatasets gdalbuildvrt
build_mask_vrt <- function(ls_file, mask_vrt_file, img_type) {
    if (img_type == "CDR") {
        mask_bands <- c('fill_QA', 'cfmask_band', 'cloud_QA', 'cloud_shadow_QA', 
                        'adjacent_cloud_QA')
        sds <- get_subdatasets(ls_file)
        # Below is to support CDR imagery downloaded prior to late August 2014
        if (any(grepl("fmask_band", sds))) {
            warning('Using "fmask_band" instead of newer "cfmask_band" band name')
            mask_bands[grepl("^cfmask_band$", mask_bands)] <- "fmask_band"
        }
        mask_sds <- sds[grepl(paste0(':(', paste(mask_bands, collapse='|'), ')$'), sds)]
        stopifnot(length(mask_sds) == 5)
        gdalbuildvrt(mask_sds, mask_vrt_file, separate=TRUE, srcnodata='None')
    } else if (img_type == "L1T") {
        mask_bands <- c('fill_QA', 'fmask_band')
        if (!grepl("_MTL.txt$", ls_file)) {
            stop("ls_file must be a *_MTL.txt file")
        }
        ls_file_base <- gsub("_MTL.txt", "", ls_file)

        fmask_file <- dir(dirname(ls_file_base),
                          pattern=paste0(basename(ls_file_base), '_MTLFmask$'),
                          full.names=TRUE)

        # Calculate a QA mask file from the fmask file, since teamlucc expects 
        # this file as part of the mask stack.
        qa_mask_file <- extension(rasterTmpFile(), '.tif')
        # TODO: Check if this is proper coding - should it be reversed?
        qa_mask <- calc(raster(fmask_file),
                        fun=function(x) {
                            out <- x == 255
                            out[x == 255] <- 255
                            return(out)
                        }, datatype="INT2S", filename=qa_mask_file)

        # Note that allow_projection_difference is used below as GDAL thinks 
        # the two images have different projection systems, even though they 
        # are in identical projection systems.
        gdalbuildvrt(c(qa_mask_file, fmask_file), mask_vrt_file, 
                     separate=TRUE, allow_projection_difference=TRUE,
                     srcnodata='None')
    } else {
        stop(paste(img_type, "is not a recognized img_type"))
    }
    return(mask_bands)
}

#' Preprocess surface reflectance imagery from the Landsat CDR archive
#'
#' This function preprocesses surface reflectance imagery from the Landsat 
#' Climate Data Record (CDR) archive. \code{auto_preprocess_landsat} can 
#' reproject CDR tiles to match the projection of a given \code{aoi}, crop the 
#' tiles to match the \code{aoi} or a common WRS-2 path/row polygon, mask 
#' missing data and clouds out of the CDR tiles, and perform topographic 
#' correction.
#'
#' \code{mask_type} chooses the cloud mask to use if topographic correction is 
#' performed (\code{tc=TRUE}). The mask can be one of three different options: 
#' "6S", "fmask", or "combined". Each option uses a different combination of 
#' cloud mask layers from the CDR product. The "6S" masks out any areas coded 
#' as fill (fill_QA=255), cloud (cloud_QA=255), cloud shadow
#' (cloud_shadow_QA=255) or adjacent to cloud (adjacent_cloud_QA=255). The 
#' "fmask" option masks out any areas coded as fill (fmask=255), cloud 
#' (fmask=4) or cloud shadow (fmask=2).  The combined option combines the "6S" 
#' and "fmask" approaches to masks out areas coded as fill, cloud, cloud 
#' shadow, or adjacent to cloud using either method. Note that "fmask" is the 
#' only supported option when \code{img_type} is L1T.
#'
#' Prior to running \code{auto_preprocess_landsat}, \code{\link{espa_extract}} 
#' should be used to extract the original zipfiles supplied by USGS. To perform 
#' topographic correction with \code{auto_preprocess_landsat}, first run 
#' \code{\link{auto_setup_dem}} to preprocess a set of DEM tiles. Then run 
#' \code{auto_preprocess_landsat} with the \code{tc=TRUE} option.
#'
#' If topographic correction is being performed, it will be run in parallel if 
#' a parallel backend is registered with \code{\link{foreach}}.
#'
#' @export
#' @importFrom rgeos gIntersection
#' @importFrom wrspathrow pathrow_poly
#' @importFrom tools file_path_sans_ext
#' @importFrom gdalUtils gdalwarp
#' @importFrom sp is.projected
#' @param image_dirs list of paths to a set of Landsat CDR image files in HDF 
#' format
#' @param prefix string to use as a prefix for all filenames
#' @param img_type type of Landsat imagery to preprocess. Can be "CDR" for 
#' Landsat Climate Data Record (CDR) imagery in HDR format, or "L1T" for 
#' Standard Terrain Correction (Level 1T) imagery. Note that if L1T imagery is 
#' used, fmask must be run locally (see https://code.google.com/p/fmask) prior 
#' to using \code{auto_preprocess_landsat}.
#' @param tc whether to topographically correct imagery (if \code{TRUE}, then 
#' \code{dem_path} must be specified)
#' @param dem_path path to a set of DEMs as output by \code{auto_setup_dem} 
#' (only required if tc=TRUE)
#' @param aoi area of interest (AOI), as a \code{SpatialPolygonsDataFrame}.  If 
#' supplied, this aoi is used to crop and set the projection system of the 
#' output. Must be in a projected coordinate system.
#' @param output_path the path to use for the output (optional - if NULL then 
#' output images will be saved alongside the input images in the same folder).
#' @param mask_type which cloud mask to use to mask clouds when performing 
#' topographic correction. Can be one of "fmask", "6S", or "both".  See 
#' Details.  (Ignored if \code{tc=FALSE)}.
#' @param mask_output if \code{TRUE}, cloud, cloud shadow, and fill areas 
#' (SLC-off gaps and areas with no data) will be set to \code{NA} in the 
#' output. Note this setting affects the final output file only - cloud, cloud 
#' shadow, and gap areas are masked out of the image during topographic 
#' correction regardless of the value of \code{mask_output}.
#' @param n_cpus the number of CPUs to use for processes that can run in 
#' parallel
#' @param cleartmp whether to clear temp files on each run through the loop
#' @param overwrite whether to overwrite existing files (otherwise an error 
#' will be raised)
#' @param of output format to use when saving output rasters. See description 
#' of \code{of} in \code{\link{gdalwarp}}.
#' @param ext file extension to use when saving output rasters (determines 
#' output file format). Should match file extension for output format chosen by 
#' \code{of}.
#' @param notify notifier to use (defaults to \code{print} function). See the 
#' \code{notifyR} package for one way of sending notifications from R. The 
#' \code{notify} function should accept a string as the only argument.
#' @param verbose whether to print detailed status messages and timing 
#' information
#' @return nothing - used for the side effect of preprocessing imagery
#' @seealso \code{\link{espa_extract}}, \code{\link{unstack_ledapscdr}}, 
#' \code{\link{auto_setup_dem}}
auto_preprocess_landsat <- function(image_dirs, prefix, img_type="CDR", 
                                    tc=FALSE, dem_path=NULL, aoi=NULL, 
                                    output_path=NULL, mask_type='fmask', 
                                    mask_output=FALSE, n_cpus=1, 
                                    cleartmp=FALSE,  overwrite=FALSE, 
                                    of="GTiff", ext='tif', notify=print, 
                                    verbose=FALSE) {
    if (grepl('_', prefix)) {
        stop('prefix cannot contain underscores (_)')
    }
    if (tc && is.null(dem_path)) {
        stop('dem_path must be supplied if tc=TRUE')
    }
    if (tc && !file_test("-d", dem_path)) {
        stop(paste(dem_path, "does not exist"))
    }
    if (!is.null(output_path) && !file_test("-d", output_path)) {
        stop(paste(output_path, "does not exist"))
    }
    if (!is.null(aoi)) {
        if (length(aoi) > 1) {
            stop('aoi should be a SpatialPolygonsDataFrame of length 1')
        }
        stopifnot(is.projected(aoi))
    }

    ext <- gsub('^[.]', '', ext)

    # Setup a regex to identify Landsat CDR images
    if (img_type == "CDR") {
        ls_regex <- '^(lndsr.)?((LT4)|(LT5)|(LE7)|(LC8))[0-9]{6}[12][0-9]{6}[a-zA-Z]{3}[0-9]{2}.hdf$'
    } else if (img_type == "L1T") {
        ls_regex <- '((LT[45])|(LE7)|(LC8))[0-9]{6}[12][0-9]{6}[a-zA-Z]{3}[0-9]{2}_MTL.txt$'
    } else {
        stop(paste(img_type, "is not a recognized img_type"))
    }

    if (img_type == "CDR") {
        stopifnot(mask_type %in% c('fmask', '6S', 'both'))
    } else if (img_type == "L1T") {
        stopifnot(mask_type == 'fmask')
    }

    ls_files <- c()
    for (image_dir in image_dirs) {
        if (!file_test("-d", image_dir)) {
            stop(paste(image_dir, "does not exist"))
        }
        ls_files <- c(ls_files, dir(image_dir, pattern=ls_regex, full.names=TRUE))
    }

    if (length(ls_files) == 0) {
        stop(paste0('No Landsat files found using img_type="', img_type, '".'))
    }

    for (ls_file in ls_files) {
        ######################################################################
        # Determine image basename for use in naming subsequent files
        meta <- get_metadata(ls_file, img_type)

        image_basename <- paste0(meta$WRS_Path, '-', meta$WRS_Row, '_',
                                 format(meta$aq_date, '%Y-%j'), '_', meta$short_name)

        if (is.null(output_path)) {
            this_output_path <- dirname(ls_file)
        } else {
            this_output_path  <- output_path
        }

        if (tc) {
            output_filename <- file.path(this_output_path,
                                         paste0(prefix, '_', image_basename, 
                                                '_tc.', ext))
        } else {
            # Skip topographic correction, so don't append _tc to filename
            output_filename <- file.path(this_output_path,
                                         paste0(prefix, '_', image_basename, 
                                                '.', ext))
        }

        log_file <- file(paste0(file_path_sans_ext(output_filename), '_log.txt'), open="wt")
        msg <- function(txt) {
            cat(paste0(txt, '\n'), file=log_file, append=TRUE)
            print(txt)
        }

        timer <- Track_time(msg)
        timer <- start_timer(timer, label=paste('Preprocessing', image_basename))

        #######################################################################
        # Crop and reproject images to match the projection being used for this 
        # image.  This is either the projection of the aoi (if aoi is 
        # supplied), or the UTM zone of the centroid of this path and row.
        if (verbose) timer <- start_timer(timer, label='cropping and reprojecting')

        band_vrt_file <- extension(rasterTmpFile(), '.vrt')
        band_names <- build_band_vrt(ls_file, band_vrt_file, img_type)
        mask_vrt_file <- extension(rasterTmpFile(), '.vrt')
        mask_band_names <- build_mask_vrt(ls_file, mask_vrt_file, img_type)

        this_pathrow_poly <- pathrow_poly(as.numeric(meta$WRS_Path), 
                                          as.numeric(meta$WRS_Row))
        if (!is.null(aoi)) {
            to_srs <- proj4string(aoi)
        } else {
            to_srs <- utm_zone(this_pathrow_poly, proj4string=TRUE)
        }

        # Calculate minimum bounding box coordinates:
        this_pathrow_poly <- spTransform(this_pathrow_poly, CRS(to_srs))
        if (!is.null(aoi)) {
            # If an aoi IS supplied, match the image extent to that of the AOI 
            # cropped to the appropriate Landsat path/row polygon.
            crop_area <- gIntersection(this_pathrow_poly, aoi, byid=TRUE)
        } else {
            # If an aoi IS NOT supplied, match the image extent to the 
            # appropriate Landsat path/row polygon.
            crop_area <- this_pathrow_poly
        }
        out_te <- as.numeric(bbox(crop_area))

        # Ensure origin is set at 0,0
        to_res <- c(30, 30)
        out_te <- normalize_extent(out_te, to_res)

        image_stack_reproj_file <- extension(rasterTmpFile(), ext)
        image_stack <- gdalwarp(band_vrt_file,
                                dstfile=image_stack_reproj_file,
                                te=out_te, t_srs=to_srs, tr=to_res, 
                                r='cubicspline', output_Raster=TRUE, of=of, 
                                multi=TRUE, wo=paste0("NUM_THREADS=", n_cpus), 
                                overwrite=overwrite, ot='Int16')
        names(image_stack) <- band_names

        mask_stack_reproj_file <- extension(rasterTmpFile(), paste0('.', ext))
        mask_stack <- gdalwarp(mask_vrt_file,
                               dstfile=mask_stack_reproj_file,
                               te=out_te, t_srs=to_srs, tr=to_res, 
                               r='near', output_Raster=TRUE, of=of, 
                               multi=TRUE, wo=paste0("NUM_THREADS=", n_cpus), 
                               overwrite=overwrite, ot='Int16')
        # Can't just directly assign mask_bands as the names since the bands 
        # may have been read in different order from the HDF file
        names(mask_stack) <- mask_band_names

        if (verbose) timer <- stop_timer(timer, label='cropping and reprojecting')

        ######################################################################
        # Perform topographic correction if tc=TRUE
        if (tc) {
            if (verbose) timer <- start_timer(timer, label='topocorr')

            ######################################################################
            # Load dem, slope, and aspect
            slopeaspect_filename <- file.path(dem_path,
                                              paste0('slopeaspect_', 
                                                     meta$WRS_Path, '-', meta$WRS_Row, '.', ext))
            slopeaspect <- brick(slopeaspect_filename)

            if (!proj4comp(proj4string(image_stack), proj4string(slopeaspect))) {
                stop(paste0('slopeaspect and image_stack projections do not match.\nslopeaspect proj4string: ', 
                            proj4string(slopeaspect), '\nimage_stack proj4string: ',
                            proj4string(image_stack)))
            } else {
                # Projection strings are functionally identical - so make sure 
                # their textual representations are the same.
                proj4string(slopeaspect) <- proj4string(image_stack)
            }

            compareRaster(slopeaspect, image_stack, orig=TRUE)

            image_stack_mask <- calc_cloud_mask(mask_stack, mask_type)

            image_stack_masked <- image_stack
            image_stack_masked[image_stack_mask] <- NA
            if (ncell(image_stack_masked) > 500000) {
                # Draw a sample for the Minnaert k regression. Note that 
                # sampleRegular with cells=TRUE returns cell numbers in the 
                # first column
                sampleindices <- sampleRegular(image_stack_masked, size=500000, 
                                               cells=TRUE)
                sampleindices <- as.vector(sampleindices[, 1])
            } else {
                sampleindices <- NULL
            }
            # Remember that slopeaspect layers are scaled to INT2S, but 
            # topographic_corr expects them as floats, so apply the scale factors 
            # used in auto_setup_dem
            slopeaspect_flt <- stack(raster(slopeaspect, layer=1) / 10000,
                                     raster(slopeaspect, layer=2) / 1000)
            image_stack_tc <- topographic_corr(image_stack_masked, 
                                               slopeaspect_flt, meta$sunelev, 
                                               meta$sunazimuth, 
                                               method='minnaert_full', 
                                               asinteger=TRUE, 
                                               sampleindices=sampleindices)
            if (!mask_output) {
                # Add back in the original values of areas that were masked out 
                # from the topographic correction:
                image_stack_tc[image_stack_mask] <- image_stack[image_stack_mask]
            }
            image_stack <- image_stack_tc
            
            if (verbose) timer <- stop_timer(timer, label='topocorr')
        }

        ######################################################################
        # Write final data
        if (verbose) timer <- start_timer(timer, label='writing data')

        mask_stack_path <- paste0(file_path_sans_ext(output_filename), 
                                  '_masks.', ext)
        mask_stack <- writeRaster(stack(mask_stack$fill_QA,
                                        mask_stack$fmask_band),
                                  filename=mask_stack_path, 
                                  overwrite=overwrite, datatype='INT2S')
        names(mask_stack) <- c('fill_QA', 'fmask_band')

        image_stack <- writeRaster(image_stack, filename=output_filename, 
                                   overwrite=overwrite, datatype='INT2S')
        if (verbose) timer <- stop_timer(timer, label='writing data')

        timer <- stop_timer(timer, label=paste('Preprocessing', image_basename))

        close(log_file)

        if (cleartmp) removeTmpFiles(h=1)
    }
}
# Function for retrieving frequencies from a raster frequency table, given a 
# band name and value. Handles the case of a given code not occurring in a 
# particular raster, in which case it will not show up as a row in the 
# frequency table.
get_freq <- function(band, value, freq_table) {
    band_col <- grep(band, names(freq_table))
    if (!(value %in% freq_table[, 1])) {
        # Return 0 if this value never shows up in the raster
        return(0)
    }
    frac <- freq_table[freq_table[1] == value, band_col]
    if (is.na(frac)) {
        return(0)
    } else {
        return(round(frac, 4))
    }
}

#' Calculate statistics on imagery within an AOI
#'
#' @export
#' @importFrom stringr str_extract
#' @param image_dirs list of paths to a set of Landsat CDR image files in 
#' GeoTIFF format as output by the \code{unstack_ledapscdr} function.
#' @param aoi an area of interest (AOI) to crop from each image
#' @return a \code{data.frame}
auto_QA_stats <- function(image_dirs, aoi) {
    lndsr_regex <- '^(lndsr.)?((LT4)|(LT5)|(LE7)|(LC8))[0-9]{6}[12][0-9]{6}[a-zA-Z]{3}[0-9]{2}'
    mask_bands <- c('fill_QA', 'fmask_band')

    out <- c()
    for (image_dir in image_dirs) {
        lndsr_files <- dir(image_dir, pattern=lndsr_regex)
        image_basenames <- unique(str_extract(lndsr_files,lndsr_regex))

        if (length(image_basenames) == 0) {
            stop(paste('no files found in', image_dir))
        }

        for (image_basename in image_basenames) {
            message(paste0('Processing ', image_basename, '...'))
            metadata_string <- str_extract(image_basename, 
                                           '((LT4)|(LT5)|(LE7)|(LC8))[0-9]{13}')
            sensor <- str_extract(metadata_string, '^((LT[45])|(LE7)|(LC8))')
            year <- substr(metadata_string, 10, 13)
            julian_day <- substr(metadata_string, 14, 16)
            img_path <- substr(metadata_string, 4, 6)
            img_row <- substr(metadata_string, 7, 9)

            mask_band_files <- c()
            for (mask_band in mask_bands) {
                mask_band_files <- c(mask_band_files,
                                     paste(file.path(image_dir, 
                                                     image_basename), 
                                           mask_band, sep='_'))
            }
            mask_band_files <- paste0(mask_band_files, '.tif')
            mask_stack <- stack(mask_band_files)
            names(mask_stack) <- mask_bands
            if (!missing(aoi)) {
                if (proj4string(aoi) != proj4string(mask_stack)) {
                    if (class(aoi) == 'Raster') {
                        aoi <- projectRaster(aoi, mask_stack)
                    } else {
                        aoi <- spTransform(aoi, CRS(proj4string(mask_stack)))
                    }
                }
                mask_stack <- crop(mask_stack, aoi)
                mask_stack <- mask(mask_stack, aoi)
            }

            freq_table <- freq(mask_stack, useNA='no', merge=TRUE)
            # Convert frequency table to fractions
            freq_table[-1] <- freq_table[-1] / colSums(freq_table[-1], na.rm=TRUE)

            out <- c(out, list(list(img_path,
                                    img_row,
                                    year,
                                    julian_day,
                                    sensor,
                                    get_freq('fill_QA', 0, freq_table),
                                    get_freq('fill_QA', 255, freq_table),
                                    get_freq('fmask_band', 0, freq_table),
                                    get_freq('fmask_band', 1, freq_table),
                                    get_freq('fmask_band', 2, freq_table),
                                    get_freq('fmask_band', 3, freq_table),
                                    get_freq('fmask_band', 4, freq_table),
                                    get_freq('fmask_band', 255, freq_table))))
        }
    }

    out <- data.frame(matrix(unlist(out), nrow=length(out), byrow=T))
    names(out) <- c('path', 'row', 'year', 'julian', 'sensor',
                    'fill_QA_notfill', 'fill_QA_fill', 'fmask_clear', 
                    'fmask_water', 'fmask_cloud_shadow', 'fmask_snow', 
                    'fmask_cloud', 'fmask_fill')
    return(out)
}
normalize_extent <- function(te, res=c(30, 30)) {
    # Setup xmin
    te[1] <- round(te[1] - te[1] %% res[1])
    # Setup ymin
    te[2] <- round(te[2] - te[2] %% res[2])
    # Setup xmax
    te[3] <- round(te[3] + res[1] - te[3] %% res[1])
    # Setup ymax
    te[4] <- round(te[4] + res[2] - te[4] %% res[2])
    stopifnot(round(te[1] / res[1]) == (te[1] / res[1]))
    stopifnot(round(te[2] / res[2]) == (te[2] / res[2]))
    stopifnot(round(te[3] / res[1]) == (te[3] / res[1]))
    stopifnot(round(te[4] / res[2]) == (te[4] / res[2]))
    return(te)
}

#' Setup the DEM mosaic for a given AOI
#'
#' This function will setup a set of DEM tiles for each the Landsat path/row 
#' needed to cover a given AOI. The tiles can optionally be cropped to cover 
#' only the portion of each path/row that is included in the AOI, or can cover 
#' the full scene for each path/row needed to cover the AOI.
#'
#' This function uses \code{gdalUtils}, which requires a local GDAL 
#' installation.  See http://trac.osgeo.org/gdal/wiki/DownloadingGdalBinaries 
#' or http://trac.osgeo.org/osgeo4w/ to download the appropriate installer for 
#' your operating system.
#'
#' @export
#' @importFrom wrspathrow pathrow_num
#' @importFrom rgdal readOGR writeOGR
#' @importFrom sp spTransform is.projected
#' @importFrom rgeos gBuffer gIntersects gUnaryUnion gIntersection
#' @importFrom tools file_path_sans_ext
#' @importFrom gdalUtils mosaic_rasters gdalwarp
#' @param aoi area of interest (AOI), as a \code{SpatialPolygonsDataFrame}, to 
#' use as as bounding box when selecting DEMs. Also used to crop and set 
#' projection of the output DEM(s) if \code{crop_to_aoi=TRUE}. Must be in a 
#' projected coordinate system.
#' @param output_path the path to use for the output
#' @param dem_extents a \code{SpatialPolygonsDataFrame} of the extents and 
#' filenames for a set of locally available DEM raster(s) that cover the 
#' \code{aoi}. See the \code{\link{get_extent_polys}} function for one means of 
#' generating this list. \code{dem_extents} must have a "filename" column.
#' @param of output format to use when saving output rasters. See description 
#' of \code{of} in \code{\link{gdalwarp}}.
#' @param ext file extension to use when saving output rasters (determines 
#' output file format). Should match file extension for output format chosen by 
#' \code{of}.
#' @param n_cpus the number of CPUs to use for processes that can run in 
#' parallel
#' @param overwrite whether to overwrite existing files (otherwise an error 
#' will be raised)
#' @param crop_to_aoi whether to crop the dem to the supplied AOI, or to the
#' Landsat path/row polygon for that particular path/row
#' @param notify notifier to use (defaults to \code{print} function). See the 
#' \code{notifyR} package for one way of sending notifications from R. The 
#' \code{notify} function should accept a string as the only argument.
#' @param verbose whether to print detailed status messages and timing 
#' information
#' @return nothing - used for the side effect of setting up DEMs
auto_setup_dem <- function(aoi, output_path, dem_extents, of="GTiff", 
                           ext='tif', n_cpus=1, overwrite=FALSE, 
                           crop_to_aoi=FALSE, notify=print, verbose=FALSE) {
    if (!file_test("-d", output_path)) {
        stop(paste(output_path, "does not exist"))
    }

    if (length(aoi) > 1) {
        stop('aoi should be a SpatialPolygonsDataFrame of length 1')
    }
    stopifnot(is.projected(aoi))

    ext <- gsub('^[.]', '', ext)

    timer <- Track_time(notify)

    pathrows <- pathrow_num(aoi, wrs_type=2, wrs_mode='D', as_polys=TRUE)
    aoi_prproj <- spTransform(aoi, CRS(proj4string(pathrows)))

    timer <- start_timer(timer, label=paste('Processing DEMS for', nrow(pathrows), 
                                            'path/rows'))
    if (crop_to_aoi) {
        # Do a rough crop of the pathrows to the AOI in the pathrow CRS 
        # (pathrow will later be cropped in the AOI CRS).
        pathrows_cropped <- gIntersection(pathrows, aoi_prproj, byid=TRUE)
        row.names(pathrows_cropped) <- row.names(pathrows)
        pathrows_cropped <- SpatialPolygonsDataFrame(pathrows_cropped, 
                                                     data=pathrows@data)
    } else {
        pathrows_cropped <- pathrows
    }

    # Add a 500 m buffer in UTM coordinate system, as 1) slope calculation 
    # requires a window of pixels, and 2) this buffer also helps avoid missing 
    # pixels on the sides of the DEM due to slight misalignments from the 
    # reprojection that will occur later. After buffering transform back to 
    # WGS84 to use for preliminary cropping of the dem mosaic. 
    pathrows_utm <- spTransform(pathrows_cropped,
                                CRS(utm_zone(pathrows_cropped, proj4string=TRUE)))
    pathrows_buffered <- spTransform(gBuffer(pathrows_utm, width=500, byid=TRUE), 
                                 CRS(proj4string(dem_extents)))
    intersecting <- as.logical(gIntersects(dem_extents, 
                                           gUnaryUnion(pathrows_buffered), byid=TRUE))
    if (sum(intersecting) == 0) {
        stop('no intersecting dem extents found')
    } else {
        dem_extents <- dem_extents[intersecting, ]
    }

    dem_list <- dem_extents$filename
    dem_rasts <- lapply(dem_list, raster)

    if (length(dem_list) > 1) {
        ######################################################################
        # Verify projections of DEMs match
        dem_prj <- projection(dem_rasts[[1]])
        if (any(lapply(dem_rasts, projection) != dem_prj)) {
            stop("each DEM in dem_list must have the same projection")
        }

        ######################################################################
        # Mosaic DEMs
        if (verbose) timer <- start_timer(timer, label='Mosaicking DEMs')
        mosaic_file <- extension(rasterTmpFile(), ext)
        # Calculate minimum bounding box coordinates:
        mosaic_te <- as.numeric(bbox(pathrows_buffered))
        # Use mosaic_rasters from gdalUtils for speed:
        mosaic_rasters(dem_list, mosaic_file, te=mosaic_te, of=of, 
                       overwrite=overwrite, ot='Int16')
        dem_mosaic <- raster(mosaic_file)
        if (verbose) timer <- stop_timer(timer, label='Mosaicking DEMs')
    } else {
        dem_mosaic <- dem_rasts[[1]]
        mosaic_file <- filename(dem_mosaic)
    }

    for (n in 1:length(pathrows)) {
        pathrow <- pathrows[n, ]
        pathrow_label <- paste(sprintf('%03i', pathrow@data$PATH), 
                               sprintf('%03i', pathrow@data$ROW), sep='-')
        timer <- start_timer(timer, label=paste0('Processing ', n, ' of ', 
                                                 nrow(pathrows), ': ', 
                                                 pathrow_label))

        if (verbose) timer <- start_timer(timer,
                                          label=paste('Cropping/reprojecting DEM mosaic crop for', 
                                          pathrow_label))
        if (crop_to_aoi) {
            to_srs <- proj4string(aoi)
            pathrow_tosrs <- spTransform(pathrow, CRS(to_srs))
            to_ext <- extent(gIntersection(pathrow_tosrs, aoi, byid=TRUE))
        } else {
            to_srs <- utm_zone(pathrow, proj4string=TRUE)
            to_ext <- projectExtent(pathrow, to_srs)
        }
        dem_te <- as.numeric(bbox(to_ext))

        # Ensure origin is set at 0,0
        to_res <- c(30, 30)
        dem_te <- normalize_extent(dem_te, to_res)

        # Calculate minimum bounding box coordinates:
        dem_mosaic_crop_filename <- file.path(output_path,
                                         paste0('dem_', pathrow_label, 
                                                '.', ext))
        dem_mosaic_crop <- gdalwarp(mosaic_file, 
                                    dstfile=dem_mosaic_crop_filename,
                                    te=dem_te, t_srs=to_srs, tr=to_res, 
                                    r='cubicspline', output_Raster=TRUE, 
                                    multi=TRUE, of=of,
                                    wo=paste0("NUM_THREADS=", n_cpus), 
                                    overwrite=overwrite, ot='Int16')
        if (verbose) timer <- stop_timer(timer,
                                         label=paste('Cropping/reprojecting DEM mosaic crop for', 
                                         pathrow_label))

        if (verbose) timer <- start_timer(timer, label=paste('Calculating slope/aspect for', 
                                                pathrow_label))
        slopeaspect_filename <- file.path(output_path,
                                          paste0('slopeaspect_',
                                                 pathrow_label, '.', ext))
        # Note that the default output of 'terrain' is in radians
        slopeaspect <- terrain(dem_mosaic_crop, opt=c('slope', 'aspect'))
        slopeaspect$aspect <- calc(slopeaspect$aspect, fun=function(vals) {
            vals[vals >= 2*pi] <- 0
            vals
            })
        # Note that slopeaspect is scaled - slope by 10000, and aspect by 1000 so 
        # that the layers can be saved as INT2S
        slopeaspect <- stack(round(raster(slopeaspect, layer=1) * 10000),
                             round(raster(slopeaspect, layer=2) * 1000))
        slopeaspect <- writeRaster(slopeaspect, filename=slopeaspect_filename, 
                                   overwrite=overwrite, datatype='INT2S')
        if (verbose) timer <- stop_timer(timer, label=paste('Calculating slope/aspect for', 
                                                pathrow_label))
        timer <- stop_timer(timer, label=paste0('Processing ', n, ' of ', 
                                                nrow(pathrows), ': ', 
                                                pathrow_label))
    }

    timer <- stop_timer(timer, label=paste('Processing DEMS for', nrow(pathrows), 
                                            'path/rows'))
}
plotprep <- function(x, maxpixels=500000, DN_min=0, DN_max=255, x_fun=NULL) {
    if (ncell(x) > maxpixels) {
        x <- sampleRegular(x, size=maxpixels, asRaster=TRUE, useGDAL=TRUE)
    }
    x <- calc(x, fun=function(vals) {
        vals[vals < DN_min] <- DN_min
        vals[vals > DN_max] <- DN_max
        vals <- ((vals - DN_min) / (DN_max - DN_min)) * 255
        if (!is.null(x_fun)) {
            vals <- x_fun(vals)
        }
        return(vals)
    }, datatype='INT1U')
    return(x)
}

#' Simple function to make small preview plot from large raster image
#'
#' @export
#' @param x input \code{RasterBrick} or \code{RasterStack} with at least three 
#' bands
#' @param m an optional mask \code{RasterLayer} to output below the browse 
#' image
#' @param maxpixels maximum number of pixels to use in plotting
#' @param DN_min minimum DN value
#' @param DN_max maximum DN value
#' @param r index in \code{x} of the band to use as the red band
#' @param g index in \code{x} of the band to use as the green band
#' @param b index in \code{x} of the band to use as the blue band
#' @param x_fun an optional function to apply to \code{x} after x is resampled 
#' according to \code{maxpixels}
#' @param m_fun an optional function to apply to \code{m} after m is resampled 
#' according to \code{maxpixels}
#' @return nothing - used for side-effect of saving browse image
browse_image <- function(x, m=NULL, maxpixels=500000, DN_min=0, DN_max=255, 
                         r=3, g=2, b=1, x_fun=NULL, m_fun=NULL) {
    if (!is.null(m)) stopifnot(nlayers(m) == 1)

    x <- plotprep(x, maxpixels=500000, DN_min=DN_min, DN_max=DN_max, 
                  x_fun=x_fun)

    if (!is.null(m) && !is.null(m_fun)) {
        m <- calc(m, fun=m_fun, datatype=dataType(m))
    }

    if (!is.null(m)) {
        m <- sampleRegular(m, size=maxpixels, asRaster=TRUE, useGDAL=TRUE)
        if (nrow(x) > ncol(x)) par(mfrow=c(1, 2))
        else par(mfrow=c(2, 1))
        plotRGB(x, r=r, g=g, b=b, maxpixels=maxpixels)
        plot(m, maxpixels=maxpixels, axes=FALSE, legend=FALSE, box=FALSE)
    } else {
        plotRGB(x, r=r, g=g, b=b, maxpixels=maxpixels)
    }
}
BSL <-
function(band3, band4, method = "quantile", ulimit = .99, llimit = .005)
{
    # find Bare Soil Line and vegetation peak

    if(is.character(band3)) {
        band3 <- read.asciigrid(band3)
        band3 <- band3@data[,1]
    } else {
        if(class(band3) == "SpatialGridDataFrame") {
            band3 <- band3@data[,1]
        } else {
            band3 <- as.vector(as.matrix(band3))
        }
    } 
    
    if(is.character(band4)) {
        band4 <- read.asciigrid(band4)
        band4 <- band4@data[,1]
    } else {
        if(class(band4) == "SpatialGridDataFrame") {
            band4 <- band4@data[,1]
        } else {
            band4 <- as.vector(as.matrix(band4))
        }
    } 


    # find joint minimum and maximum
    bsl.joint <- cbind(band3, band4)
    bsl.joint <- bsl.joint[apply(bsl.joint, 1, function(x)!any(is.na(x))), ]
    bsl.joint <- bsl.joint[apply(bsl.joint, 1, function(x)all(x < 255)), ]

    ratio43 <- bsl.joint[,2]/bsl.joint[,1]

    if(method == "quantile") {
        bsl.lmodel2 <- lmodel2(bsl.joint[ratio43 < quantile(ratio43, llimit), 2] ~ bsl.joint[ratio43 < quantile(ratio43, llimit), 1])
    }
    else if(method == "minimum") {
        # want lowest band4 value for each band3 value (lowest NIR for each red)
        bsl.min <- factor(bsl.joint[,1], levels=1:254)
        bsl.min <- split(bsl.joint[,2], bsl.min, drop=TRUE)
        bsl.min <- lapply(bsl.min, min)

        bsl.lmodel2 <- lmodel2(as.numeric(bsl.min) ~ as.numeric(names(bsl.min)))
    }
    else {
        stop("Method not found.\n")
    }

    bsl.lm <- unlist(bsl.lmodel2$regression.results[2, 2:3])
    names(bsl.lm) <- c("Intercept", "Slope")

    ### next, find top vegetation point

    bsl.test <- bsl.joint
    bsl.test[,2] <- 255 - bsl.test[,2] # want high values of band 4
    bsl.test <- apply(bsl.test, 1, sum)

    # want high veg cover
    bsl.top <- bsl.joint[ratio43 > quantile(ratio43, ulimit, na.rm=TRUE), ]
    bsl.test <- bsl.test[ratio43 > quantile(ratio43, ulimit, na.rm=TRUE)]
    bsl.top <- bsl.top[bsl.test == min(bsl.test), ]
    if(!is.null(dim(bsl.top))) bsl.top <- bsl.top[sample(1:nrow(bsl.top), 1),]

    list(BSL=bsl.lm, top=bsl.top)
}

#Jorge Ahumada. Tropical Ecology Assessment and Monitoring Network. Conservation International
# Code developed on 2010/07/02 - 2010/12/01
require(TeachingDemos)
require(reshape2)
require(ggplot2)
#script to process raw TEAM files and get them ready for analysis
f.readin.fix.data<-function(){
	require(lubridate)
	data<-read.csv(file.choose(),h=T,skip=62)
	data<-f.fix.data(data)
	#make sure date info makes sense
	data
	}


#function to create binary matrices for all species at a site and sampling period. Matrix has a 1 if the species was seen in a day a 0 if not seen and NA if not sampled
#The function requires data from one sampling event and will return a list composed of 0,1 matrices, one matrix for each species.

#THIS FUNCTION WORKS WITH NEW TEAM DATA ONLY - do not use with legacy TEAM data
# this works one year at a time. Separate data in different years first
f.matrix.creator2<-function(data,year){
  #results object
  res<-list()
  
  #get the dimensions of the matrix
  
  #list if sanpling units
  cams<-unique(data$Sampling.Unit.Name)
  cams<-sort(cams)
  rows<-length(cams)
  species<-unique(data$bin)
  #start and end dates of sampling periods
  data<-data[data$Sampling.Period==year,]
  min<-min(data$Start.Date)
  max<-max(data$End.Date)
  cols<-max-min+1
  
  #sampling period
  date.header<-seq(from=min,to=max, by="days")
  mat<-matrix(NA,rows,cols,dimnames=list(cams,as.character(date.header)))
  
  #for all cameras, determine the open and close date and mark in the matrix
  start.dates<-tapply(as.character(data$Start.Date),data$Sampling.Unit.Name,unique)
  nms<-names(start.dates)
  start.dates<-ymd(start.dates)
  names(start.dates)<-nms
  end.dates<-tapply(as.character(data$End.Date),data$Sampling.Unit.Name,unique)
  end.dates<-ymd(end.dates)
  names(end.dates)<-nms
  
  #outline the sampling periods for each camera j
  for(j in 1:length(start.dates)){
    #for each camera beginning and end of sampling
    low<-which(date.header==start.dates[j])
    hi<-which(date.header==end.dates[j])
    if(length(low)+length(hi)>0){
      indx<-seq(from=low,to=hi)
      mat[names(start.dates)[j],indx]<-0
    } else next
  }
  mat.template<-mat
  #get the species
  #species<-unique(data$bin)
  #construct the matrix for each species i
  for(i in 1:length(species)){
    indx<-which(data$bin==species[i])
    #dates and cameras when/where the species was photographed
    dates<-data$Photo.Date[indx]
    cameras<-data$Sampling.Unit.Name[indx]
    dates.cameras<-data.frame(dates,cameras)
    #unique combination of dates and cameras 
    dates.cameras<-unique(dates.cameras)
    #fill in the matrix
    for(j in 1:length(dates.cameras[,1])){
      col<-which(date.header==dates.cameras[j,1])
      row<-which(cams==dates.cameras[j,2])
      mat[row,col]<-1
    }
    mat.nas<-is.na(mat)
    sum.nas<-apply(mat.nas,2,sum)
    indx.nas<-which(sum.nas==rows)
    if(length(indx.nas)>0){
      mat<-mat[,-indx.nas]
    }
    
    res<-c(res,list(mat))
    #return the matrix to its original form
    mat<-mat.template
  }
  
  names(res)<-species
  #res<-lapply(res,f.dum)
  res
  
}

f.check.NA.breaks<-function(vector){
	notna<-which(!is.na(vector))
	if(min(notna)+length(notna)-1==max(notna)) print("ok")
	else print("aggh")
	}
	
f.start.minus.end<-function(data){
	data$End.Date-data$Start.Date
	}
f.start<-function(data){
	data$Start.Date
	}
f.end<-function(data){
	data$End.Date
	}

f.picture.dates<-function(data){
	data$Photo.Date
	}
f.picture.span <-function(data){
	max(data)-min(data)
	}
f.picture.min<-function(data){
	min(data)
	}
f.picture.max<-function(data){
	max(data)
	}
#function to fix the start/stop time of a camera if it is incorrectly entered	
f.start.stop.date.fixer<-function(data){
	
	cam.start.date<-by(data,data$Sampling.Unit.Name,f.start)
	cam.start.date<-lapply(cam.start.date,unique)
	cam.end.date<-by(data,data$Sampling.Unit.Name,f.end)
	cam.end.date<-lapply(cam.end.date,unique)
	
	#cam.span<-(by(data,data$Sampling.Unit.Name,f.start.minus.end))
	#cam.span<-lapply(cam.span,unique)
	
	pic.span<-by(data,data$Sampling.Unit.Name,f.picture.dates)
	min.pic<-lapply(pic.span,f.picture.min)
	max.pic<-lapply(pic.span,f.picture.max)
	#pic.span<-lapply(pic.span,f.picture.span)
	
	indx<-which(as.numeric(cam.start.date)-as.numeric(min.pic)>0 |as.numeric(cam.end.date)-as.numeric(max.pic)<0)
	#figure out which camera has the problem
	#indx<-which(as.numeric(cam.span)-as.numeric(pic.span)<=0)
	if(length(indx)){
	{cam.id<-names(pic.span)[indx]
	print("There are problems with the following cameras:")
	print(cam.id)}
	#for(i in 1:length(indx)){
	#	index<-which(data$Sampling.Unit.Name==cam.id[i])
	#	data$Start.Date[index]<-min.pic[[indx[i]]]
	#	data$End.Date[index]<-max.pic[[indx[i]]]
	#	
	#	}
		}
		else
			print("No problems detected..")
	#data
	}	
#function to convert a list of sampling matrices generated by f.matrix.creator2 into a data frame that can be used by the unmarked package	
f.convert.to.unmarked<-function(list){
require(unmarked)
nspecies<-length(list)
nrows<-dim(list[[1]])[1]
ncols<-dim(list[[1]])[2]
oldmat<-list()

for(i in 1:nspecies){
	mat<-rbind(oldmat,list[[i]])
	oldmat<-mat
	}	
y<-as.matrix(mat[,-ncols])
rownames(y)<-NULL
colnames(y)<-NULL
species<-gl(n=nspecies,k=nrows,labels=names(list))
siteCovs<-as.data.frame(species)
unmarkedFrameOccu(y=y,siteCovs=siteCovs)	
	}
	
	f.correct.DF<-function(DF){
ind <- sapply(DF, is.factor)
DF[ind] <- lapply(DF[ind], "[", drop=TRUE)
DF
	}


f.fix.data <- function(data){
  require(lubridate)
#This function converts the dates and times in the data into date and time
  data$Photo.Date<-ymd(as.character(data$Photo.Date))
	data$Photo.Time<-hms(as.character(data$Photo.Time))
	#this line stores the date and time info for each photo into a single object
  time.date<-ymd_hms(paste(data$Photo.Date,data$Photo.Time,sep=""))
	#split the date from the time for the Camera start date and time
  data$Camera.Start.Date.and.Time<-ymd_hms(as.character(data$Camera.Start.Date.and.Time))
  qwe<-data$Camera.Start.Date.and.Time
  qwe2<-ymd(paste(year(qwe),"-",month(qwe),"-",day(qwe),sep=""))
  data<-data.frame(data,Start.Date=qwe2)
	
	#Now do the same but for the End date and time of each camera trap
  #qwe<-strsplit(as.character(data$Camera.End.Date.and.Time)," ",fixed=T)
  data$Camera.End.Date.and.Time<-ymd_hms(as.character(data$Camera.End.Date.and.Time))
	qwe<-data$Camera.End.Date.and.Time
  qwe2<-ymd(paste(year(qwe),"-",month(qwe),"-",day(qwe),sep=""))
  data<-data.frame(data,End.Date=qwe2)
	#create new variable with binomial - genus species
  bin<-paste(data$Genus,data$Species)
	data<-data.frame(data,bin=bin,td.photo=time.date)
  data
	}
# This works with data downloaded from the database, not from the TEAM website
f.fix.data2 <- function(data){
  require(lubridate)
  #This function converts the dates and times in the data into date and time
  #data$Photo.Date<-ymd(as.character(data$Photo.Date))
  #data$Photo.Time<-hms(as.character(data$Photo.Time))
  #this line stores the date and time info for each photo into a single object
  time.date<-ymd_hms(as.character(data$Photo.Taken.Time))
  data$Photo.Date<-ymd(paste(year(time.date),"-",month(time.date),"-",day(time.date),sep=""))
  #split the date from the time for the Camera start date and time
  data$Camera.Start.Date.and.Time<-ymd_hms(as.character(data$Camera.Start.Date.and.Time))
  qwe<-data$Camera.Start.Date.and.Time
  qwe2<-ymd(paste(year(qwe),"-",month(qwe),"-",day(qwe),sep=""))
  data<-data.frame(data,Start.Date=qwe2)
  
  #Now do the same but for the End date and time of each camera trap
  #qwe<-strsplit(as.character(data$Camera.End.Date.and.Time)," ",fixed=T)
  data$Camera.End.Date.and.Time<-ymd_hms(as.character(data$Camera.End.Date.and.Time))
  qwe<-data$Camera.End.Date.and.Time
  qwe2<-ymd(paste(year(qwe),"-",month(qwe),"-",day(qwe),sep=""))
  data<-data.frame(data,End.Date=qwe2)
  #create new variable with binomial - genus species
  bin<-paste(data$Genus,data$Species)
  data<-data.frame(data,bin=bin,td.photo=time.date)
  data
}
f.dum<-function(data){
	dum<-apply(data,1,sum,na.rm=T)
	dum<-ifelse(dum>0,0,1)
	data<-data.frame(data,dum=dum)
	data
	}
f.extract.rare.sp<-function(raredata,alldata){
	spnum<-length(raredata[,1])
	oldindx<-numeric()
	for(i in 1:spnum){
		
		indx<-c(oldindx,which(alldata@siteCovs$species==as.character(raredata[i,1])))
		oldindx<-indx
		}
	sp<-siteCovs(alldata)[indx,]
	sp<-factor(sp)
	newufframe<-unmarkedFrameOccu(y=getY(alldata)[indx,],siteCovs=data.frame(species=sp))
	newufframe
	}

#Separate independent photographic events for a species in a given camera trap and date. thresh gives the threshold for considering events separate
#thresh is in minutes
f.separate<-function(data,thresh){
	
	#diff(data$td.photo)
	l<-length(data)
	interval<-diff(data)#abs(c(data[2:l],NA)-data)
	interval<-interval/60 #convert to minutes
	interval<-as.numeric(interval)
  ev<-1;res<-numeric()
	cond<-interval>thresh #about 5 minutes in between events
	for(i in 1:(l-1)){
		if(!cond[i]) ev<-ev
		else ev<-ev+1
		res<-c(res,ev)
		
		}
	c(1,res)
	}
#test function; not usually used	
f.test.sep<-function(cond){
	l<-length(cond)
	#interval<-c(data$Photo.Time[2:l],NA)-data$Photo.Time
	ev<-1;res<-numeric()
	#cond<-interval>5
	for(i in 1:(l-1)){
		if(!cond[i]) ev<-ev
		else ev<-ev+1
		res<-c(res,ev)
		
		}
	c(1,res)

	
	}
	
#Order the data by Sampling unit name and photo raw name. This will order images chronologically
f.order.data<-function(data){
	indx<-order(data$Sampling.Period,data$Sampling.Unit.Name,data$Photo.Taken.Time)
	data<-data[indx,]
	data
	}
#function to separate independent events, extract from the list and paste together with the data set.
#This function removes records that are NOT images.. e.g. Sampling Date records
f.separate.events<-function(data,thresh){
	
	#e.data<-by(data$td.photo,data$Sampling.Unit.Name,f.separate,thresh)
  indx<-which(is.na(data$Photo.Taken.Time))
  if(length(indx)>0)
    data<-data[-indx,]
  e.data<-f.separate(data$Photo.Taken.Time,thresh)
#e.data<-data.frame(grp=unlist(e.data))
data.frame(data,grp=paste(data$Sampling.Period,".",data$Sampling.Unit.Name,".",e.data,sep=""))

	}
#Simulation to explore the effect of changing the threshold on the number
# of independent events. Thresh range is given as a sequence in mins
f.sim.thres<-function(data,threshRange){
  qwe<-data[,-42]
  res<-data.frame(thresh=threshRange,n.events=NA)
  for(i in threshRange){
  qwe<-f.separate.events(qwe,threshRange[i])
  res[i,2]<-length(unique(qwe[,42]))
  qwe<-qwe[,-42]
  qwe<-f.correct.DF(qwe)
  }
  plot(res[,1],res[,2],xlab="Threshold (min)",ylab="Number of events",type='b')
  res
  }
  
#convert Farenheit to Celsius

f.FtoC<-function(temp) {
  round(5/9*(temp-32))
}

#extract temperatures for a given species and graph
f.extract.temp<-function(data,species) {
  qwe<-data[data$bin==species,]
  qwe<-f.correct.DF(qwe)
  res<-as.numeric(by(qwe$Temp,qwe$grp,mean))
  res
}

f.graph.temp<-function(species,spname,nbins) {
  par(lwd=2)
  truehist(species,xlim=c(15,35),ylim=c(0,0.25),xlab=expression(paste("Temperature (",degree,"C)",sep="")),col="blue",nbins=nbins)
  abline(v=28.6,lty=2,lwd=3)
  #abline(v=26.7,lwd=2)
  title(spname)
}

#create code to separate variables for a single event
f.events<-function(data){
  temp<-mean(data$Temperature)
  site<-unique(as.character(data$Site.Name))
  date<-min(as.character(data$Photo.Date))
  time<-min(as.character(data$Photo.Time))
  sp<-unique(as.character(data$bin))
  sun<-unique(as.character(data$Sampling.Unit.Name))
  lat<-unique(data$Latitude)
  lon<-unique(data$Longitude)
  sap<-unique(data$Sampling.Period)
  mp<-unique(data$Moon.Phase)
c(site,date,time,sap,temp,sp,sun,lat,lon,mp)
  }

#code to put together dataframe with each redcord being an event
f.events.dataframe<-function(data){
  require(chron)
  qwe<-by(data,data$grp,f.events)
  qwe<-as.data.frame(do.call("rbind",qwe))
  names(qwe)<-c("Site.Name","Date","Time","Sampling.Period","Temperature","bin","Sampling.Unit.Name","Latitude","Longitude","Moon.Phase")
  qwe$Date<-as.Date(chron(dates=as.character(qwe$Date),format=c(dates="y-m-d")))
  qwe$Time<-as.POSIXct(as.character(qwe$Time),format="%H:%M:%S")
  qwe$Temperature<-as.numeric(as.character(qwe$Temperature))
  qwe$Latitude<-as.numeric(as.character(qwe$Latitude))
  qwe$Longitude<-as.numeric(as.character(qwe$Longitude))
  qwe$Moon.Phase<-as.numeric(as.character(qwe$Moon.Phase))
  qwe
}  
  
#Code to create temperature event dataframes for a list of species.
#puts them all in a list

f.create.events.splist<-function(splist,fulldata){
  results<-list()
  for(i in 1:length(splist)){
    #Extract the data
    sp<-fulldata[fulldata$bin==splist[i],]
    sp<-f.correct.DF(sp)
    if(dim(sp)[1]<2){
      print(paste("Species ",splist[i]," has insufficient data..",sep=""))
      results<-c(results,list(NULL))
      next
    }
    #Order the data in chronological order
    sp<-f.order.data(sp)
    #Create independent observation events list with a threshold of 5 min
    sp<-f.separate.events(sp,5) 
   #Create a simplified data frame with just the events
    sp<-f.events.dataframe(sp)
    results<-c(results,list(sp))
    print(paste("Species ",splist[i]," processed..",sep=""))
  }
  names(results)<-splist
  results
  
}
f.print.graphs<-function(data){
  path="/Users/jorge/Analyses/TempTV/graphs2/"
for(i in 1:length(data)) {
  newp<-paste(path,names(data)[i],".pdf",sep="")
  pdf(newp)
  qplot(Temperature,data=data[[i]],geom="histogram",binwidth=1,main=names(data)[i])
  ggsave(newp)
}  
  
}
# funcion para asignar camaras faltantes que no tomaron fotos de animales.
# SuName,startDate y endDate deben estar entre comillas. Los demas argumentos no.
# startDate y endDate estan en format yyyy-mm-dd
f.assign.missing<-function(SuName,SuPeriod,startDate,endDate,data){
  rows<-dim(data)[1]
  
  #agregar Sampling Unit Name
  data[rows+1,3]<-SuName
  #agregar StartDate
  data[rows+1,38]<-as.Date(startDate)
  #agregar End Date
  data[rows+1,39]<-as.Date(endDate)
  #agregar sampling unit period
  data[rows+1,6]<-SuPeriod
  data
  
}

f.minusBirds<-function(data){
  indx<-which(data$Class=="AVES")
  data<-data[-indx,]
  data<-f.correct.DF(data)
}
#not reliable
#code to shrink the matrix by half
# f.shrink.matrix.half<-function(matrix){
#   #if number of columns in the matrix is even
#   if(!ncol(matrix)%%2){
#     #figure out how many columns
#     nc<-ncol(matrix)/2  
#     #disagregate into individual matrices
#     new.matrix<-matrix(NA,nr=nrow(matrix),nc=nc)
#     old.cols<-seq(1,ncol(matrix),2)
#     for(i in 1:nc){
#       #sum the rows for the column sections
#       sum.rows<-apply(matrix[,old.cols[i]:(old.cols[i]+1)],1,sum,na.rm=T)
#       #convert to 0s and 1s
#       new.matrix[,i]<-ifelse(sum.rows>=1,1,0)
#     }  
#     new.matrix
#   }
#   #if the number of columns is not even
#   else{
#     #store the first column in col1  
#     col1<-matrix[,1]
#     #convert the matrix to an even matrix
#     matrix<-matrix[,-1]
#     nc<-ncol(matrix)/2  
#     #disagregate into individual matrices
#     new.matrix<-matrix(NA,nr=nrow(matrix),nc=nc)
#     old.cols<-seq(1,ncol(matrix),2)
#     for(i in 1:nc){
#       sum.rows<-apply(matrix[,old.cols[i]:(old.cols[i]+1)],1,sum,na.rm=T)
#       new.matrix[,i]<-ifelse(sum.rows>=1,1,0)
#     }
#     cbind(col1,new.matrix)  
#   }
# }

#collapseData<-f.shrink.matrix(testData)
#umf2<-unmarkedFrameOccu(y=collapsedData)
#umf2
#fmcoll<-occu(~1 ~1,umf2)
#summary(fmcoll)
#plogis(coef(mod,"det"))
#plogis(coef(mod,"state"))
#plogis(coef(mod2,"det"))
#plogis(coef(mod2,"state"))

#code to shrink the matrix to exactly 15 columns
f.shrink.matrix.to15<-function(matrix){
  nc<-dim(matrix)[2]
  if(!nc%%15){ # of the number of columns is exactly divisible by 15
    newc<-nc%/%15
    old.cols<-seq(1,nc,newc)
    new.matrix<-matrix(NA,nr=nrow(matrix),nc=15)
    for(i in 1:15){
      new.matrix[,i]<-apply(matrix[,old.cols[i]:(old.cols[i]+newc-1)],1,max,na.rm=T)
    }
  } else{
    rem<-nc%%15
    newc<-nc%/%15
    old.cols<-seq(1,nc-rem,newc)
    new.matrix<-matrix(NA,nr=nrow(matrix),nc=15)
    for(i in 1:14)
      new.matrix[,i]<-apply(matrix[,old.cols[i]:(old.cols[i]+newc-1)],1,max,na.rm=T)
    new.matrix[,15]<-apply(matrix[,old.cols[15]:nc],1,max,na.rm=T) 
  }
  new.matrix[new.matrix=="-Inf"]<-NA
  rownames(new.matrix)<-rownames(matrix)
  new.matrix
}


#does not work
#f.shrink.matrix<-function(matrix){
  
  #disagregate into individual matrices
  #nc<-length(seq(1,ncol(matrix),9))  
  #new.matrix<-matrix(NA,nr=nrow(matrix),nc=nc)
  #rownames(new.matrix)<-rownames(matrix)  
  #old.cols<-seq(1,ncol(matrix),9)
  #for(i in 1:nc){
    #sum the rows for the column sections
  #  sum.rows<-apply(matrix[,old.cols[i]:(old.cols[i]+1)],1,sum)
    #convert to 0s and 1s
   # new.matrix[,i]<-ifelse(sum.rows>=1,1,0)
  #}  
  #new.matrix
#}

f.plot.jag.res<-function(jags,species.name,model.name){
  #naive occupancy
  naive.occ<-apply(tmp,2,sum,na.rm=T)/apply(tmp,2,function(x){length(which(!is.na(x)))})
  #plot results
  mat<-jags$BUGSoutput$summary
  #mean occupancy from model
  m.occ<-jags$BUGSoutput$mean$psi
  #median occupancy from model
  med.occ<-jags$BUGSoutput$median$psi
  #extract 95% confidence limits for occupancy
  psiCol<-which(rownames(mat)=="psi[1]")
  lo95ci<-mat[psiCol:(psiCol+3),3]
  hi95ci<-mat[psiCol:(psiCol+3),7]
  par(mfrow=c(2,1))
  plot(2009:2012,m.occ,ylim=c(0,1),type='b',xlab="year",ylab="occupancy")
  lines(2009:2012,as.numeric(lo95ci),lty=2)
  lines(2009:2012,as.numeric(hi95ci),lty=2)
  lines(2009:2012,naive.occ,col='red')
  #lines(2007:2011,med.occ,lwd=3)
  title(paste(species.name,"\n",model.name))
  
  #graph lambda
  lambdaCol<-which(rownames(mat)=="lambda[1]")
  m.lambda<-mat[lambdaCol:(lambdaCol+2),1]
  
  lo95ci<-mat[lambdaCol:(lambdaCol+2),3]
  hi95ci<-mat[lambdaCol:(lambdaCol+2),7]
  plot(1:3,m.lambda,ylim=range(lo95ci,hi95ci),type='b',xlab="year interval",ylab="lambda")
  lines(1:3,as.numeric(lo95ci),lty=2)
  lines(1:3,as.numeric(hi95ci),lty=2)
  abline(h=1,lty=3)
  title(paste(species.name,"\n",model.name))
}

# the function returns a reduced matrix collapsed using nday; if necessary, an X number of columns filled with NA are added
# to adjust the size of the shrinked matrix; be careful that nday makes sense for the size of the matrix, so that
# not many columns of NA are added 

shrink<-function(matrice,nday){
  dy<-nday
  while (dy < ncol(matrice)) {dy <- dy + nday}  
  addcol<-dy-ncol(matrice)
  if (addcol!=0) { 
    matNA<-matrix(NA,nrow=nrow(matrice),ncol=addcol)
    matrice<-data.frame(matrice,matNA)}    
  
  period<-ncol(matrice)/nday
  newday<-rep(1:period, each = nday)
  
  shr<-function (vec) {
    nav<-is.na(vec)
    dom<-all(nav==T)
    if(dom==T) {y<-NA}
    else {
      abb<-sum(vec,na.rm=T)
      y<-ifelse(abb==0,0,1)
    }
    return(y)
  }
  
  matday<-data.frame(newday,t(matrice))
  shrmat<-t(aggregate(matday[,-1],list(matday$newday),shr))
  
  return (shrmat[-1,]) 
}

#put together the pres/absence matrices for multiple years for a particular species
# starting with a list that has all matrices for each species for each year
f.multyear.sp<-function(LIST,spn){
  mats<-lapply(LIST,function(x) x[[spn]])
  res<-array(dim=c(dim(mats[[1]])[1],dim(mats[[1]])[2],length(mats)))
  for(i in 1:length(mats))
    res[,,i]<-mats[[i]]
  rownames(res)<-rownames(mats[[1]])
  res
}
#HELPER functions in WPI analysis and rare species analyses
#create some fake data for a species
#that is rare

#function to calculate the mode of a distribution
f.mode<-function(data,na.rm=T){
  qwe<-density(data,na.rm=T)
  qwe$x[which(qwe$y==max(qwe$y))]
  
}

#function to generate the WPI from the output simulations in JAGS
# psi is a three dimensional matrix with the psi of each species in each year
f.WPI <-function(psi){
  nsim <- dim(psi)[1]
  nyears <- dim(psi)[2]
  nsp <- dim(psi)[3]
  rel_psi<-numeric()
  wpi<-matrix(NA,nr=nsim,nc=nyears)
  for(i in 1:nsim){
    for(t in 1:nyears){
      for(s in 1:nsp){
        rel_psi[s] <- log(psi[i,t,s]/psi[i,1,s])
      }
      wpi[i,t]<-exp(1/nsp*sum(rel_psi))
    }
  }
  colnames(wpi)<-dimnames(psi)[[2]]
  wpi
  
}
#function to generate the WPI from the output simulations in JAGS
# psi is a three dimensional matrix with the psi of each species in each year
#this uses the odd ratio of psi/psi1 rather than the actual ration
f.WPI2 <-function(psi){
  nsim <- dim(psi)[1]
  nyears <- dim(psi)[2]
  nsp <- dim(psi)[3]
  rel_psi<-numeric()
  wpi<-matrix(NA,nr=nsim,nc=nyears)
  for(i in 1:nsim){
    for(t in 1:nyears){
      for(s in 1:nsp){
        rel_psi[s] <- log((psi[i,t,s]/(1-psi[i,t,s]+1/60/2))/(psi[i,1,s]/(1-psi[i,1,s]+1/60/2)))
      }
      wpi[i,t]<-exp(1/nsp*sum(rel_psi))
    }
  }
  colnames(wpi)<-dimnames(psi)[[2]]
  wpi
  
}
#graph the WPI through time with 95% confidence limits
#WPI is a matrix of n x t values (n = number of runs, t=number of years)
#calculate with mean, mode or median
graph.WPI <- function(wpi,fun=mean,title){
  require(TeachingDemos)
  require(ggplot2)
  year<-as.numeric(colnames(wpi))
  FUN<-match.fun(fun)
  ct<-apply(wpi,2,FUN,na.rm=T)
  #int<-apply(wpi,2,emp.hpd)
  lo50<-apply(wpi,2,quantile,0.25,na.rm=T)
  hi50<-apply(wpi,2,quantile,0.75,na.rm=T)
  conf75<-apply(wpi,2,quantile,c(0.125,1-0.125),na.rm=T)
  res<-data.frame(year=year,ct=ct,lo50=lo50,hi50=hi50,lo75=conf75[1,],hi75=conf75[2,])
  #res<-melt(res,id.vars=c('year'))
  
  p<-ggplot(data=res, aes(x=year))
  p<-p+geom_line(aes(y=ct),size=2)
  p<-p+geom_ribbon(aes(ymin=lo50,ymax=hi50),alpha=0.2)+geom_ribbon(aes(ymin=lo75,ymax=hi75),alpha=0.1)+xlab("Year")+ylab("Wildlife Picture Index")+labs(title="")+geom_hline(yintercept=1,size=0.5,linetype=2) +labs(title=title)+ylim(0,10)
  p
  
  #ggsave("SpeciesRichness.pdf",p,width=15,height=8,units="cm")
}

graph.psi <- function(psi,initial,fun=mean,title="",low=0.025,high=0.975,path=""){
  
  require(ggplot2)
  year<-as.numeric(colnames(psi))
  FUN<-match.fun(fun)
  ct<-apply(psi,2,FUN)
  lo<-apply(psi,2,quantile,low)
  hi<-apply(psi,2,quantile,high)
  naive<-initial
  #naive<-apply(initial,2,function(x) sum(x,na.rm=T)/sum(!is.na(x)))
  res<-data.frame(year=year,ct=ct,lo=lo,hi=hi,naive=naive)
  #res<-melt(res,id.vars=c('year'))
  
  p<-ggplot(data=res, aes(x=year))
  p<-p+geom_line(aes(y=ct),size=2)+geom_point(aes(y=naive),size=3)
  p<-p+geom_ribbon(aes(ymin=lo,ymax=hi),alpha=0.2)+xlab("Year")+ylab("Occupancy")+ labs(title="")+ ylim(0,1)+labs(title=title)
  #p
  
  ggsave(paste(path,"/Occ_",title,".pdf",sep=""),p,width=15,height=8,units="cm")
}

graph.psi2 <- function(psifit,title){
  
  require(ggplot2)
  #year<-as.numeric(colnames(psi))
  #FUN<-match.fun(fun)
  #ct<-apply(psi,2,FUN)
  #lo<-apply(psi,2,quantile,0.025)
  #hi<-apply(psi,2,quantile,0.975)
  #naive<-apply(initial,2,function(x) sum(x,na.rm=T)/sum(!is.na(x)))
  #res<-data.frame(year=year,ct=ct,lo=lo,hi=hi,naive=naive)
  #res<-melt(res,id.vars=c('year'))
  
  p<-ggplot(data=psifit, aes(x=year))
  p<-p+geom_line(aes(y=ct),size=2)+geom_point(aes(y=naive),size=3)
  p<-p+geom_ribbon(aes(ymin=lo,ymax=hi),alpha=0.2)+xlab("Year")+ylab("Occupancy")+ labs(title="")+ ylim(0,1)+labs(title=title)
  p
  
  #ggsave(paste("Occ_",title,".pdf",sep=""),p,width=15,height=8,units="cm")
}
#function to check for posterior predictive checks

f.ppc<-function(model){
  fit<-model$BUGSoutput$sims.list$fit
  fit.new<-model$BUGSoutput$sims.list$fit.new
  plot(fit,fit.new)
  abline(0,1)
  return(mean(fit.new>fit))
  
}
f.calc.psi<-function(psi,initial,fun=mean){
  year<-as.numeric(colnames(psi))
  FUN<-match.fun(fun)
  ct<-apply(psi,2,FUN)
  lo<-apply(psi,2,quantile,0.025)
  hi<-apply(psi,2,quantile,0.975)
  naive<-apply(initial,2,function(x) sum(x,na.rm=T)/sum(!is.na(x)))
  data.frame(year=year,ct=ct,lo=lo,hi=hi,naive=naive)
  
}

f.load.fitted<-function(path){
  load(path)
  temp2<-temp
  temp2
}
#This function takes a wpi object (matrix of n iterations by y years)
# and calculates lambda with confidence limits
f.calc.lambda<-function(wpi){
  years<-dim(wpi)[2]
  lambda<-matrix(NA,nr=dim(wpi)[1],nc=years-1)
  for(i in 2:years)
    lambda[,i-1]<-wpi[,i]/wpi[,i-1]
  mean<-apply(lambda,2,mean)
  conf<-apply(lambda,2,quantile,c(0.025,0.975))
  data.frame(year=dimnames(wpi)[[2]][2:years],mean.lambda=mean,lo95=conf[1,],hi95=conf[2,])
  
}

extractSpeciesObsOcc <-function(list){
#Extract species observed occupancy from a list that has nyear elements
  #each element is contanins nsp matrices (ncameras x 15) with presence/absence data at different #camera traps 
years<-names(list)
sp.names<-names(list[[1]])  
n.years<-length(years)  
n.sp<-length(sp.names)

results.matrix<-matrix(NA,nr=n.sp,nc=n.years)
for (i in 1:n.years){
  results.matrix[ ,i] <- as.numeric(lapply(list[[i]], calculateObsOcc))
  
}
colnames(results.matrix) <- years
rownames(results.matrix) <- sp.names
results.matrix

}

calculateObsOcc<-function(matrix){
  pres.abs<-apply(matrix,1,max,na.rm=T)
  pres.abs[is.infinite(pres.abs)] <- NA
  n.cam<-sum(!is.na(pres.abs))
  sum(pres.abs, na.rm=T)/n.cam
  
}
extractSpeciesDetections <-function(list){
  #Extract species detections from a list that has nyear elements
  #each element is contanins nsp matrices (ncameras x 15) with presence/absence data at different #camera traps 
  years<-names(list)
  sp.names<-names(list[[1]])  
  n.years<-length(years)  
  n.sp<-length(sp.names)
  
  results.matrix<-matrix(NA,nr=n.sp,nc=n.years)
  for (i in 1:n.years){
    results.matrix[ ,i] <- as.numeric(lapply(list[[i]], calculateObsDet))
    
  }
  colnames(results.matrix) <- years
  rownames(results.matrix) <- sp.names
  results.matrix
  
}
calculateObsDet<-function(matrix){
  pres.abs<-apply(matrix,1,max,na.rm=T)
  pres.abs[is.infinite(pres.abs)] <- NA
  sum(pres.abs, na.rm=T)
  
}
extractNumCameraTraps <-function(list){
  #Extract number of camera traps used from a list that has nyear elements
  #each element is contanins nsp matrices (ncameras x 15) with presence/absence data at different #camera traps 
  years<-names(list)
  sp.names<-names(list[[1]])  
  n.years<-length(years)  
  n.sp<-length(sp.names)
  
  results.matrix<-matrix(NA,nr=n.sp,nc=n.years)
  for (i in 1:n.years){
    results.matrix[ ,i] <- as.numeric(lapply(list[[i]], calculateNumCameraTraps))
    
  }
  colnames(results.matrix) <- years
  rownames(results.matrix) <- sp.names
  results.matrix
  
}
calculateNumCameraTraps<-function(matrix){
  pres.abs<-apply(matrix,1,max,na.rm=T)
  pres.abs[is.infinite(pres.abs)] <- NA
  sum(!is.na(pres.abs))
}

calculateWPIDiagnostics <- function(site.name){
# This function compares the observed occupancy with the fitted occupancy
# coming from the WPI analytics system
# It requires a vector with the names of the sites (site.names) and
# assumes that the whole camera trap data set is in the workspace (data)
  # and an object with the fitted results (wpi)
  results<-data.frame(site=character(),species=character(),
    tot.dets=numeric(),
    dets.per.year=numeric(),
    overall.occ=numeric(),
    diff.obs.mode=numeric(),
    diff.obs.median=numeric(),
    stringsAsFactors=F)     
#for(j in 1:length(site.names)){
  path<-as.character(site.name)
  dir.create(path)
  #Extract data for site j
  s.data<-subset(x=data,Site.Name==site.name,drop=T)
  # take care of some things with the data
  s.data<-f.fix.data2(s.data)
  nyears<-length(year<-sort(as.numeric(unique(s.data$Sampling.Period))))
  #create raw occupancy matrices
  mat<-list()
  for(i in 1:nyears)
    mat[[i]]<-f.matrix.creator2(s.data,year[i])  
  names(mat)<-year
  #Compress the matrices
  shmat<-list()
  for(i in 1:nyears)
    shmat[[i]]<-lapply(mat[[i]],FUN=f.shrink.matrix.to15)
  names(shmat)<-year
  #Extract species observed occupancy
  obs.occ.matrix<-extractSpeciesObsOcc(shmat)
  obs.det.matrix<-extractSpeciesDetections(shmat)
  obs.cams.matrix<-extractNumCameraTraps(shmat)
  
  #Extract a site and species and graph the results
  s.wpi<-subset(wpi,site_name==site.name,drop=T)
  #do it for one species
  
  s.sp.list<-unique(s.wpi$bin)

indx <- which(s.sp.list %in% rownames(obs.occ.matrix))
s.sp.list <- s.sp.list[indx] 
  
  
  for(i in 1:length(s.sp.list)){
    #i <- 1
    #j <- 5
    
    sp<-subset(x=s.wpi,bin==s.sp.list[i],drop=T)
    #turn into a matrix so I can graph it
    sp <- acast(sp, iteration~year, value.var="psi")
    #calculate mode, median and confidence intervals
    occ.mode<-apply(sp,2,f.mode)
    occ.median<-apply(sp,2,median)
    #lo<-apply(sp,2,quantile,c(0.1,0.9))
    #hi<-apply(sp,2,quantile,c(0.1,0.9))
    #calculate observed annual occupancy ()
    obs.occ <- obs.occ.matrix[s.sp.list[i],]
    #calculate observed number of detections per year
    obs.dets.per.year <- obs.det.matrix[s.sp.list[i],]
    #calculate total number of detections
    tot.dets <- sum(obs.det.matrix[s.sp.list[i],])
    #calculate mean number of detections
    dets.per.year <- tot.dets/nyears
    #calculate observed overall occupancy (for the whole time period)
    obs.all.occ <- sum(obs.det.matrix[s.sp.list[i],])/sum(obs.cams.matrix[s.sp.list[i],])
    #calculate distance between observed and fitted mode
    diff.obs.mode <- occ.mode - obs.occ
    #calculate distance between observed and fitted median
    diff.obs.median <- occ.median - obs.occ
    #calculate mean distance between observed and fitted mode
    mean.diff.obs.mode <- mean(diff.obs.mode, na.rm=T)
    #calculate mean distance between observed and fitted median
    mean.diff.obs.median <- mean(diff.obs.median, na.rm=T)
    #put everything together in a data frame
    temp <- c(as.character(site.name),s.sp.list[i], tot.dets,
              dets.per.year,
              obs.all.occ,
              mean.diff.obs.mode,
              mean.diff.obs.median)
    results[nrow(results)+1, ] <- temp                      
    
    graph.psi(title=paste(s.sp.list[i],"_",site.name),psi=sp,initial=obs.occ.matrix[s.sp.list[i],],fun=median,low=0.1,hi=0.9,path=path)
    print(paste("Done with species ",s.sp.list[i]))
  }
results
}#' Change Direction Image for CVAPS
#'
#' This code calculate the change direction image for the Change Vector 
#' Analysis in Posterior Probability Space (CVAPS) method of Chen et al. 2011.  
#' Use the change direction image in conjunction with the change magnitude 
#' image from \code{chg_dir}, and \code{DFPS} to use the Double Window Flexible 
#' Pace Search method (Chen et al. 2003) to determine the threshold to use to 
#' map areas of change and no-change.
#'
#' @export
#' @importFrom spatial.tools rasterEngine
#' @importFrom tools file_path_sans_ext
#' @param t1p time 0 posterior probability \code{Raster*}
#' @param t2p time 1 posterior probability \code{Raster*}
#' @param filename (optional) filename for output change direction
#' \code{RasterLayer}
#' @param overwrite whether to overwrite existing files (otherwise an error 
#' will be raised)
#' @param verbose whether to print detailed status messages
#' @param ... additional parameters to pass to rasterEngine
#' @return \code{Raster*} object with change direction image
#' @references Chen, J., P. Gong, C. He, R. Pu, and P. Shi. 2003.
#' Land-use/land-cover change detection using improved change-vector analysis.
#' Photogrammetric Engineering and Remote Sensing 69:369-380.
#' 
#' Chen, J., X. Chen, X. Cui, and J. Chen. 2011. Change vector analysis in 
#' posterior probability space: a new method for land cover change detection.  
#' IEEE Geoscience and Remote Sensing Letters 8:317-321.
#' @examples
#' \dontrun{
#' t0_train_data <- get_pixels(L5TSR_1986, L5TSR_1986_2001_training, "class_1986",training=.6)
#' t0_model <- train_classifier(t0_train_data)
#' t0_preds <- classify(L5TSR_1986, t0_model)
#' t1_train_data <- get_pixels(L5TSR_2001, L5TSR_1986_2001_training, "class_2001", training=.6)
#' t1_model <- train_classifier(t1_train_data)
#' t1_preds <- classify(L5TSR_2001, t1_model)
#' t0_t1_chgdir <- chg_dir(t0_preds$probs, t1_preds$probs)
#' }
chg_dir <- function(t1p, t2p, filename, overwrite=FALSE, verbose=FALSE, ...) {
    if (proj4string(t1p) != proj4string(t2p)) {
        stop('t0 and t1 coordinate systems do not match')
    }
    if (extent(t1p) != extent(t2p)) {
        stop('t0 and t1 extents do not match')
    }
    if (nlayers(t1p) != nlayers(t2p)) {
        stop('t0 and t1 probability maps have differing number of classes')
    }
    if (!missing(filename) && file_test('-f', filename) && !overwrite) {
        stop('output file already exists and overwrite=FALSE')
    }

    n_classes <- nlayers(t1p)
    if (n_classes == 1) {
        stop('cannot calculate change probabilities for only one class')
    }

    # calc_chg_dir_st <- function(t1p, t2p, n_classes, ...) {
    #     # Calculate change direction (eqns 5 and 6 in Chen 2011)
    #     dP <- array(t2p - t1p, dim=c(dim(t1p)[1], dim(t1p)[2], n_classes))
    #     unit_vecs <- array(diag(n_classes), dim=c(n_classes, n_classes))
    #     Eab <- apply(dP, c(1, 2), function(pixel) pixel %*% unit_vecs)
    #     chgdir <- apply(Eab, c(2, 3),
    #                     function(pixel) which(pixel == max(pixel)))
    #     chgdir <- array(chgdir, dim=c(dim(t1p)[1], dim(t1p)[2], 1))
    #     return(chgdir)
    # }
    # out <- rasterEngine(t1p=t1p, t2p=t2p, fun=calc_chg_dir_st, 
    #                     args=list(n_classes=n_classes), 
    #                     outbands=1, datatype='INT2S', ...)
    #
    # # spatial.tools can only output the raster package grid format - so output 
    # # to a tempfile in that format then copy over to the requested final output 
    # # format if a filename was supplied
    # if (!missing(filename)) {
    #     out <- writeRaster(out, filename=filename, dataType='INT2S', 
    #                        overwrite=overwrite)
    # }
    
    if (missing(filename)) {
        filename <- rasterTmpFile()
        overwrite <- TRUE
    }
   
    bs <- blockSize(t1p)
    out <- raster(t1p)
    out <- writeStart(out, filename=filename, overwrite=overwrite)
    for (block_num in 1:bs$n) {
        if (verbose > 0) {
            message("Processing block ", block_num, " of ", bs$n, "...")
        }
        dims <- c(bs$nrows[block_num], ncol(t1p), nlayers(t1p))
        t1p_bl <- array(getValuesBlock(t1p, row=bs$row[block_num], 
                                 nrows=bs$nrows[block_num]),
                        dim=c(dims[1] * dims[2], dims[3]))
        t2p_bl <- array(getValuesBlock(t2p, row=bs$row[block_num], 
                                       nrows=bs$nrows[block_num]),
                        dim=c(dims[1] * dims[2], dims[3]))
        chg_dirs <- calc_chg_dir(t1p_bl, t2p_bl)
        out <- writeValues(out, chg_dirs, bs$row[block_num])
    }
    out <- writeStop(out)

    return(out)
}
#' Change Magnitude Image for CVAPS
#'
#' This code calculate the change magnitude image for the Change Vector 
#' Analysis in Posterior Probability Space (CVAPS) method of Chen et al. 2011.  
#' Use the change magnitude image and use it in conjunction with the change 
#' direction image from \code{chg_dir} to map areas of change and no-change.  
#' The threshold can be determined using \code{\link{DFPS}} (to use the Double 
#' Window Flexible Pace Search method, from Chen et al. 2003) or 
#' \code{\link{threshold}} (which uses an unsupervised method).
#'
#' This function will run in parallel if a parallel backend is registered with 
#' \code{\link{foreach}}.
#'
#' @export
#' @importFrom spatial.tools rasterEngine
#' @param t1p time 0 posterior probability \code{Raster*}
#' @param t2p time 1 posterior probability \code{Raster*}
#' @param filename (optional) filename for output change magnitude
#' \code{RasterLayer}
#' @param overwrite whether to overwrite existing files (otherwise an error 
#' will be raised)
#' @param ... additional parameters to pass to rasterEngine
#' @return \code{Raster*} object with change magnitude image
#' @references Chen, J., P. Gong, C. He, R. Pu, and P. Shi. 2003.
#' Land-use/land-cover change detection using improved change-vector analysis.
#' Photogrammetric Engineering and Remote Sensing 69:369-380.
#' 
#' Chen, J., X. Chen, X. Cui, and J. Chen. 2011. Change vector analysis in 
#' posterior probability space: a new method for land cover change detection.  
#' IEEE Geoscience and Remote Sensing Letters 8:317-321.
#' @examples
#' \dontrun{
#' t0_train_data <- get_pixels(L5TSR_1986, L5TSR_1986_2001_training, "class_1986",training=.6)
#' t0_model <- train_classifier(t0_train_data)
#' t0_preds <- classify(L5TSR_1986, t0_model)
#' t1_train_data <- get_pixels(L5TSR_2001, L5TSR_1986_2001_training, "class_2001", training=.6)
#' t1_model <- train_classifier(t1_train_data)
#' t1_preds <- classify(L5TSR_2001, t1_model)
#' t0_t1_chgmag <- chg_mag(t0_preds$probs, t1_preds$probs)
#' }
chg_mag <- function(t1p, t2p, filename, overwrite=FALSE, ...) {
    if (proj4string(t1p) != proj4string(t2p)) {
        stop('t0 and t1 coordinate systems do not match')
    }
    if (extent(t1p) != extent(t2p)) {
        stop('t0 and t1 extents do not match')
    }
    if (nlayers(t1p) != nlayers(t2p)) {
        stop('t0 and t1 probability maps have differing number of classes')
    }
    if (!missing(filename) && file_test('-f', filename) && !overwrite) {
        stop('output file already exists and overwrite=FALSE')
    }

    n_classes <- nlayers(t1p)

    calc_chg_mag <- function(t1p, t2p, n_classes, ...) {
        if (is.null(dim(t1p))) {
            # Handle RasterLayer images
            chgmag <- abs(t2p - t1p)
        } else {
            # Handle RasterStack or RasterBrick images
            chgmag <- apply(t2p - t1p, c(1, 2), function(pixel) sqrt(sum(pixel^2)))
        }
        chgmag <- array(chgmag, dim=c(dim(t1p)[1], dim(t1p)[2], 1))
        return(chgmag)
    }
    out <- rasterEngine(t1p=t1p, t2p=t2p, fun=calc_chg_mag, 
                        args=list(n_classes=n_classes), 
                        outbands=1, outfiles=1, ...)
    
    # spatial.tools can only output the raster package grid format - so output 
    # to a tempfile in that format then copy over to the requested final output 
    # format if a filename was supplied
    if (!missing(filename)) {
        out <- writeRaster(out, filename=filename, overwrite=overwrite)
    }

    return(out)
}
#' Calculate change-trajectory lookup table
#'
#' This function will format a lookup table (lut) to allow coding change 
#' trajectories. Useful for use in conjunction with \code{\link{chg_traj}}.
#'
#' @export
#' @param class_codes a list of integer codes used to code land use/cover 
#' classes
#' @param class_names an (optional) list of class names as character vectors
#' @examples
#' lut <- traj_lut(c(1, 2), c("NonForest", "Forest"))
traj_lut <- function(class_codes, class_names=NULL) {
    lut <- expand.grid(t0_code=class_codes, t1_code=class_codes)
    if (!is.null(class_names)) {
        if (length(class_names) != length(class_codes)) {
            stop('class_names must be NULL or a vector of length equal to number of classes in initial image')
        }
        lut$t0_name <- class_names[match(lut$t0_code, class_codes)]
        lut$t1_name <- class_names[match(lut$t1_code, class_codes)]
    }
    # Code trajectories by summing t0 and t1 after multiplying t1 by the number 
    # of classes.
    lut$Code <- lut$t0_code + lut$t1_code * length(class_codes)
    # Exclude classes that are persistence - CVAPS doesn't directly code the 
    # class for classes that persist
    lut <- lut[!(lut$t0_code == lut$t1_code), ]
    return(lut)
}

#' Calculate change-trajectory image
#'
#' This function will calculate trajectories of land cover change using the 
#' Change Vector Analysis in Posterior Probability Space (CVAPS) approach of 
#' comparing posterior probabilities of class membership with an automatically 
#' determined threshold. Areas of no change are coded as -1. A lookup table for 
#' the codes output by \code{chg_traj} can be calculated with \code{traj_lut}.
#'
#' This function will run in parallel if a parallel backend is registered with 
#' \code{\link{foreach}}.
#'
#' @export
#' @importFrom spatial.tools rasterEngine
#' @param chg_mag change magnitude \code{RasterLayer} from \code{CVAPS}
#' @param chg_dir change direction \code{RasterLayer} from \code{CVAPS}
#' @param chg_threshold the threshold to use as a minimum when determining change 
#' areas (can use \code{DFPS} to determine this value).
#' @param filename filename to save the output \code{RasterLayer} to disk 
#' (optional)
#' @param overwrite whether to overwrite existing files (otherwise an error 
#' will be raised)
#' @param ... additional parameters to pass to rasterEngine
#' @return a {RasterLayer} of change trajectories, with change trajectories 
#' coded as in the \code{lut} output by \code{traj_lut}
#' @references Chen, J., P. Gong, C.  He, R.  Pu, and P.  Shi.  2003.
#' Land-use/land-cover change detection using improved change-vector analysis.
#' Photogrammetric Engineering and Remote Sensing 69:369-380.
#' 
#' Chen, J., X. Chen, X. Cui, and J. Chen. 2011. Change vector analysis in
#' posterior probability space: a new method for land cover change detection.
#' IEEE Geoscience and Remote Sensing Letters 8:317-321.
#' @examples
#' \dontrun{
#' t0_train_data <- get_pixels(L5TSR_1986, L5TSR_1986_2001_training, "class_1986",training=.6)
#' t0_model <- train_classifier(t0_train_data)
#' t0_preds <- classify(L5TSR_1986, t0_model)
#' t1_train_data <- get_pixels(L5TSR_2001, L5TSR_1986_2001_training, "class_2001", training=.6)
#' t1_model <- train_classifier(t1_train_data)
#' t1_preds <- classify(L5TSR_2001, t1_model)
#' t0_t1_chgmag <- chg_mag(t0_preds$probs, t1_preds$probs)
#' t0_t1_chgdir <- chg_dir(t0_preds$probs, t1_preds$probs)
#' 
#' lut <- traj_lut(t0_preds$codes$code, t0_preds$codes$class)
#' t0_t1_chgtraj <- chg_traj(lut, t0_t1_chgmag, t0_t1_chgdir, .5)
#' 
#' # Change areas are coded following the above lookup-table (lut):
#' plot(t0_t1_chgtraj)
#' 
#' # No change areas are -1:
#' plot(t0_t1_chgtraj == -1)
#' }
chg_traj <- function(chg_mag, chg_dir, chg_threshold, filename, 
                     overwrite=FALSE, ...) {
    if (nlayers(chg_mag) > 1) stop('chg_mag has more than 1 layer')
    if (nlayers(chg_dir) > 1) stop('chg_dir has more than 1 layer')
    compareRaster(chg_mag, chg_dir)
    if (!missing(filename) && file_test('-f', filename) && !overwrite) {
        stop('output file already exists and overwrite=FALSE')
    }

    calc_chg_traj <- function(chg_mag, chg_dir, chg_threshold, ...) {
        # Trajectories in chg_dir were coded by summing t0 and t1 classes after 
        # multiplying t1 class by the number of classes
        chg_dir[chg_mag < chg_threshold] <- -1
        chg_dir[is.na(chg_dir)] <- -2
        chg_dir <- array(chg_dir, dim=c(dim(chg_mag)[1], dim(chg_mag)[2], 1))
        return(chg_dir)
    }
    out <- rasterEngine(chg_mag=chg_mag, chg_dir=chg_dir, fun=calc_chg_traj,
                        args=list(chg_threshold=chg_threshold),
                        datatype='INT2S', ...)

    # spatial.tools doesn't properly handle NA values for integer layers, so 
    # they were coded as -2 above - now recode them as NA
    out[out == -2] <- NA

    # spatial.tools can only output the raster package grid format - so output 
    # to a tempfile in that format then copy over to the requested final output 
    # format if a filename was supplied
    if (!missing(filename)) {
        out <- writeRaster(out, filename=filename, overwrite=overwrite, 
                           datatype='INT2S')
    }

    return(out)
}
#' Calculate change-trajectory statistics
#'
#' @export
#' @param traj a list (as output by \code{chg_traj} with two elements: traj_lut 
#' (a lookup table of change trajectory codes) and chg_traj (a 
#' \code{RasterLayer} of change trajectory codes.
#' @return a \code{data.frame} object with change trajectory statistics
#' @examples
#' #TODO: Add examples
chg_traj_stats <- function(traj) {
    chg_table <- table(getValues(traj$chg_traj))
    summ_table <- data.frame(Traj_Code=traj$traj_lut$Code,
                             Trajectory=paste(traj$traj_lut$t0_name, traj$traj_lut$t1_name, 
                                              sep='-'))
    summ_table <- cbind(summ_table, n_pixels=chg_table[match(row.names(chg_table), summ_table$Traj_Code)])
    row.names(summ_table) <- NULL
    summ_table$Frac_Chg <- summ_table$n_pixels / sum(summ_table$n_pixels)
    summ_table$Frac_Tot <- summ_table$n_pixels / length(traj$chg_traj)
    return(summ_table)
}
#' Classify an image using a trained classifier
#'
#' This function will produce two outputs - a prediction image and a 
#' probability image. The prediction image contains the predicted classes, the 
#' and the probability image contains the per-pixel predicted probabilities of 
#' occurrence of each class.
#'
#' This function will run in parallel if a parallel backend is registered with 
#' \code{\link{foreach}} - TEMPORARILY DISABLED.
#'
#' @export
#' @import caret
#' @importFrom spatial.tools rasterEngine
#' @param x a \code{Raster*} image with the predictor layer(s) for the 
#' classification
#' @param model a trained classifier as output by 
#' \code{\link{train_classifier}}
#' @param classes_file filename for predicted classes (or missing)
#' @param prob_file filename for predicted probabilities (or missing)
#' @param factors a list of character vector giving the names of predictors 
#' (layer names from the images used to build \code{train_data}) that should be 
#' treated as factors, and specifying the levels of each factor. For example, 
#' \code{factors=list(year=c(1990, 1995, 2000, 2005, 2010))}.
#' @param overwrite whether to overwrite \code{out_name} if it already exists
#' @return a list with 2 elements: the predicted classes as a 
#' \code{RasterLayer} and the class probabilities as a \code{RasterBrick}
#' @examples
#' \dontrun{
#' train_data <- get_pixels(L5TSR_1986, L5TSR_1986_2001_training, "class_1986", 
#'                          training=.6)
#' model <- train_classifier(train_data)
#' preds <- classify(L5TSR_1986, model)
#' plot(preds$classes)
#' plot(preds$probs)
#' }
classify <- function(x, model, classes_file, prob_file, factors=list(), 
                     overwrite=FALSE) {
    # TODO: Check with Jonathan why below fix is needed
    if (!("RasterBrick" %in% class(x))) x <- brick(x)

    if (!missing(prob_file) && file_test('-f', prob_file) && !overwrite) {
        stop(paste('output file', prob_file, 'already exists and overwrite=FALSE'))
    }
    if (!missing(classes_file) && file_test('-f', classes_file) && !overwrite) {
        stop(paste('output file', classes_file, 'already exists and overwrite=FALSE'))
    }

    make_preds <- function(inrast, model, factors, ...) {
        # First, preserve the names:
        band_names <- dimnames(inrast)[3][[1]]

        # Flatten the array to a matrix (we lose the names here)
        inrast_mat <- inrast
        dim(inrast_mat) <- c(dim(inrast)[1]*dim(inrast)[2], dim(inrast)[3])
        inrast_df <- as.data.frame(inrast_mat)
        names(inrast_df) <- band_names

        # Make sure any factor variables are converted to factors and that the 
        # proper levels are assigned
        if (length(factors) > 0) {
            for (n in 1:length(factors)) {
                factor_var <- names(factors)[n]
                factor_col <- which(names(inrast_df) == factor_var)
                inrast_df[, factor_col] <- factor(inrast_df[, factor_col], 
                                                  levels=factors[[n]])
            }
        }

        good_obs <- complete.cases(inrast_df)
        preds <- matrix(NA, nrow=nrow(inrast_df), ncol=nlevels(model))
        if (sum(good_obs) > 0) {
            good_preds <- predict(model, inrast_df[good_obs, ], type="prob")
            preds[which(good_obs), ] <- as.matrix(good_preds)
        }

        preds_array <- array(preds, dim=c(dim(inrast)[1], dim(inrast)[2], 
                                          nlevels(model)))
        return(preds_array)
    }
    probs <- rasterEngine(inrast=x, fun=make_preds,
                          args=list(model=model, factors=factors),
                          filename=rasterTmpFile(), overwrite=overwrite, 
                          datatype="FLT4S", .packages=c("randomForest"),
                          setMinMax=TRUE)
    # spatial.tools can only output the raster package grid format - so output 
    # to a tempfile in that format then copy over to the requested final output 
    # format if a filename was supplied
    if (!missing(prob_file)) {
        probs <- writeRaster(probs, filename=prob_file, overwrite=overwrite, 
                             datatype='FLT4S')
    }
    names(probs) <- levels(model)

    # Calculate the highest probability class from the class probabilities
    if (missing(classes_file)) classes_file <- rasterTmpFile()
    classes <- calc(probs, fun=function(vals) {
            # Subtract 1 below as software like ENVI starts class codes at zero
            out <- as.numeric(which(vals == max(vals))) - 1
            #TODO: Need to handle case of ties (length(out) > 1)
            if (length(out) != 1) out <- NA
            return(out)
        }, datatype='INT2S', filename=classes_file, overwrite=overwrite)
    names(classes) <- 'prediction'

    codes <- data.frame(code=seq(0, (nlevels(model) - 1)), class=levels(model))

    return(list(classes=classes, probs=probs, codes=codes))
}
#' Exports statistics on pixels within each of a set of land cover classes
#'
#' @export
#' @importFrom dplyr group_by summarize
#' @importFrom reshape2 melt
#' @param x A \code{RasterLayer} from which class statistics will be 
#' calculated.
#' @param y A \code{SpatialPolygonsDataFrame} with cover class 
#' polygons
#' @param class_col the name of the column containing the response variable 
#' (for example the land cover type of each pixel)
#' @return A data.frame of class statistics.
#' @examples
#' class_statistics(L5TSR_1986, L5TSR_1986_2001_training, "class_1986")
class_statistics <- function(x, y, class_col) {
    if (projection(x) != projection(y)) {
        stop('Coordinate systems do not match')
    }
    if (class(y) == "SpatialPolygonsDataFrame") {
        pixels <- get_pixels(x, y, class_col)
    } else if (class(y) %in% c("RasterLayer", "RasterBrick", 
                                         "RasterStack")) {
        stop('class_statistics cannot yet handle Raster* objects')
    }
    pixels <- melt(data.frame(pixels@x, y=pixels@y), idvar='y')
    # Set y and variable to NULL to pass R CMD CHECK without notes
    value=variable=NULL
    class_stats <- summarize(group_by(pixels, y, variable), mean=mean(value), 
                             sd=sd(value), min=min(value), max=max(value), 
                             n_pixels=length(value))
    class_stats <- class_stats[order(class_stats$variable, class_stats$y), ]
    return(class_stats)
}
###############################################################################
# This code is used to clean the TEAM trees dataset for processing. This 
# combines the QA/QC checks from both Lydia Beaudrot and Alex Zvoleff.
#
# Use this script on a recent download from the team vegetation database.
#
# Date: July 2014
###############################################################################

library(dplyr)
library(stringr)
library(lubridate)

# Uncomment below to 
#veg_data <- f.teamdb.query('vegetation')

dir(".", pattern="veg_data")
load('H:/Data/TEAM_Database_Downloads/veg_data2014-09-11.gzip')

trees <- result$tree
sitecode_key <- read.csv("sitecode_key.csv")
trees$sitecode <- sitecode_key$sitecode[match(trees$SiteName, 
                                              sitecode_key$sitename_database)]
trees$ObservationDate <- as.Date(trees$ObservationDate)

trees <- tbl_df(trees)

###############################################################################
# Some sites (mainly BCI) incorrectly use zeros instead of NAs when NewDiameter 
# does not apply.  
trees$Diameter[trees$Diameter == 0] <- NA
trees$POMHeight[trees$POMHeight == 0] <- NA
trees$NewDiameter[trees$NewDiameter == 0] <- NA
trees$NewPOMHeight[trees$NewPOMHeight == 0] <- NA

###############################################################################
# Add an identifier to stems to code the sampling period number on a per site 
# basis, with 1 assigned to the first sampling period in each site
SamplingPeriods <- summarize(group_by(trees, sitecode, SamplingPeriod))
SamplingPeriods <- SamplingPeriods[order(SamplingPeriods$sitecode, SamplingPeriods$SamplingPeriod), ]
SamplingPeriods <- mutate(SamplingPeriods, SamplingPeriodNumber=seq(1, length(SamplingPeriod)))
trees$SamplingPeriodNumber <- SamplingPeriods$SamplingPeriodNumber[match(paste(trees$sitecode, trees$SamplingPeriod),
                                                                         paste(SamplingPeriods$sitecode, SamplingPeriods$SamplingPeriod))]

###############################################################################
# Clean the ConditionCodes field
trees$ConditionCodes[trees$ConditionCodes == " "] <- ""
trees$ConditionCodes[is.na(trees$ConditionCodes)] <- ""
trees$ConditionCodes[grepl(' ', trees$ConditionCodes)]
trees$ConditionCodes <- gsub('^,', '', trees$ConditionCodes)
trees$ConditionCodes <- gsub('[,.]$', '', trees$ConditionCodes)
trees$ConditionCodes <- gsub(' ', '', trees$ConditionCodes)
trees$ConditionCodes <- gsub('[.]', ',', trees$ConditionCodes)

table(grepl('[.]', trees$ConditionCodes))
table(grepl('[ ]', trees$ConditionCodes))
table(grepl('[.,]$', trees$ConditionCodes))
table(grepl('^[.,]', trees$ConditionCodes))

# Add condition code columns, one column per code
ConditionCodes <- c('B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M', 
                    'N', 'O', 'P', 'R', 'S', 'T', 'U', 'V', 'W')
for (ConditionCode in ConditionCodes) {
    this_code <- unlist(lapply(trees$ConditionCodes, function(x) ConditionCode %in% x))
    trees <- cbind(trees, this_code)
    names(trees)[ncol(trees)] <- paste0('ConditionCode_', ConditionCode)
}

###############################################################################
# There are multiple observations per sampling period for the same tree. This 
# should not occur except in case of remeasurement or multiple stems. Multiple 
# stems however should have unique SamplingUnitName values.
trees <- mutate(group_by(trees, sitecode, SamplingUnitName, SamplingPeriod),
                         n_obs=rep(length(Diameter), length(Diameter)),
                         obs_num=seq(1:length(Diameter)))
# Make a dataframe of trees with multiple measurements
mult_obs_trees <- select(filter(trees, n_obs > 1), ObservationDate, Diameter, POMHeight, SamplingUnitName, ConditionCodes, obs_num)
mult_obs_trees <- mult_obs_trees[order(mult_obs_trees$SamplingUnitName, mult_obs_trees$ObservationDate), ]
write.csv(mult_obs_trees, file="trees_with_multiple_obs_per_period.csv", row.names=FALSE)

mean(trees$n_obs)
table(trees$n_obs)
table(trees[trees$n_obs > 1,]$sitecode, trees[trees$n_obs > 1,]$SamplingPeriod)

###############################################################################
# Correct 999 used to code missing or dead trees in CSN per Jul 10 email from 
# Jimmy. This value codes missing or dead trees. Code as NA so these are 
# dropped from growth calculations. TODO: Need to recode these in the condition 
# code column.
trees[which((trees$sitecode == 'CSN') & (trees$Diameter == 999)), ]$Diameter <- NA

summ_stats <- summarize(group_by(trees, sitecode, SamplingPeriod),
                        dbh_mean=mean(Diameter, na.rm=TRUE),
                        dbh_min=min(Diameter, na.rm=TRUE),
                        dbh_max=max(Diameter, na.rm=TRUE),
                        dbh_sd=sd(Diameter, na.rm=TRUE),
                        POM_mean=mean(POMHeight, na.rm=TRUE),
                        POM_min=min(POMHeight, na.rm=TRUE),
                        POM_max=max(POMHeight, na.rm=TRUE),
                        POM_sd=sd(POMHeight, na.rm=TRUE),
                        newdbh_mean=mean(NewDiameter, na.rm=TRUE),
                        newdbh_min=min(NewDiameter, na.rm=TRUE),
                        newdbh_max=max(NewDiameter, na.rm=TRUE),
                        newdbh_sd=sd(NewDiameter, na.rm=TRUE),
                        newPOM_mean=mean(NewPOMHeight, na.rm=TRUE),
                        newPOM_min=min(NewPOMHeight, na.rm=TRUE),
                        newPOM_max=max(NewPOMHeight, na.rm=TRUE),
                        newPOM_sd=sd(NewPOMHeight, na.rm=TRUE))

###############################################################################
# Correct for NewDiameter issue. For now always take NewDiameter and/or 
# NewPOMHeight when available
trees$Diameter <- ifelse(is.na(trees$NewDiameter), trees$Diameter, trees$NewDiameter)
trees$POMHeight <- ifelse(is.na(trees$NewPOMHeight), trees$POMHeight, trees$NewPOMHeight)

###############################################################################
# Fix variable name starting with numeric
names(trees) <- gsub('1haPlot', 'OnehaPlot', names(trees))

# Add column with site codes
trees$Site.CodeT <- gsub("-", "", str_extract(trees$OnehaPlotNumber,"-[a-zA-Z]*-"))

# Manually clean data (prior to database cleaning) using corrected spellings from Tropicos
# Duplicate tree genera
trees$Genus[trees$Genus=="Albizia Durazz."] <- "Albizia"
trees$Genus[trees$Genus=="Allophyllus"] <- "Allophylus"
trees$Genus[trees$Genus=="Cassearea"] <- "Casearia"
trees$Genus[trees$Genus=="Cassipourea Aubl."] <- "Cassipourea"
trees$Genus[trees$Genus=="Casspourea"] <- "Cassipourea"
trees$Genus[trees$Genus=="Chrysoclamys"] <- "Chrysochlamys"
trees$Genus[trees$Genus=="Clerodendron"] <- "Clerodendrum"
trees$Genus[trees$Genus=="Corida"] <- "Cordia"
trees$Genus[trees$Genus=="Denrocnide"] <- "Dendrocnide"
trees$Genus[trees$Genus=="Drypetes Vahl"] <- "Drypetes"
trees$Genus[trees$Genus=="Dysoxyllum"] <- "Dysoxylum"
trees$Genus[trees$Genus=="Elaegia"] <- "Elaeagia"
trees$Genus[trees$Genus=="Erythroxylon"] <- "Erythroxylum"
trees$Genus[trees$Genus=="Illex"] <- "Ilex"
trees$Genus[trees$Genus=="Luehopsis"] <- "Lueheopsis"
trees$Genus[trees$Genus=="Melanochylla"] <- "Melanochyla"
trees$Genus[trees$Genus=="Neea/Guapira"] <- "Neea"
trees$Genus[trees$Genus=="Payena Lucida"] <- "Payena"
trees$Genus[trees$Genus=="Pleurothiryum"] <- "Pleurothyrium"
trees$Genus[trees$Genus=="Pleurotyrium"] <- "Pleurothyrium"
trees$Genus[trees$Genus=="Potamea"] <- "Potameia"
trees$Genus[trees$Genus=="Psychotria Mahonii"] <- "Psychotria"
trees$Genus[trees$Genus=="Querqus"] <- "Quercus"
trees$Genus[trees$Genus=="Rhodmnia"] <- "Rhodamnia"
trees$Genus[trees$Genus=="Rinoerocarpus"] <- "Rinoreocarpus"
trees$Genus[trees$Genus=="Ritchea"] <- "Ritchiea"
trees$Genus[trees$Genus=="Rytiginia"] <- "Rytigynia"
trees$Genus[trees$Genus=="Sapotaceae"] <- "Unknown"
trees$Genus[trees$Genus=="Sygygium"] <- "Syzygium"
trees$Genus[trees$Genus=="Sygyzium"] <- "Syzygium"
trees$Genus[trees$Genus=="Symplocoa"] <- "Symplocos"
trees$Genus[trees$Genus=="Tratinickia"] <- "Trattinnickia"
trees$Genus[trees$Genus=="Tremma"] <- "Trema"
trees$Genus[trees$Genus=="Turaea"] <- "Turraea"
trees$Genus[trees$Genus=="Xantophyllum"] <- "Xanthophyllum"
trees$Genus[trees$Genus=="Xymolas"] <- "Xymalos"
trees$Genus[trees$Genus=="Zanthoxylem"] <- "Zanthoxylum"
trees$Genus[trees$Genus=="Zizyphus"] <- "Ziziphus"
trees$Genus[trees$Genus=="Gymnosporia (Ex. Maytenus)"] <- "Gymnacranthera"
trees$Genus[trees$Genus=="Hensenia"] <- "Heinsenia"
trees$Genus[trees$Genus=="Mycronychia"] <- "Micronychia"
trees$Genus[trees$Genus=="Podo"] <- "Podocarpus"
trees$Genus[trees$Genus=="Polycious"] <- "Polyscias"
trees$Genus[trees$Genus=="Scheflera"] <- "Schefflera"
# Tree misspellings
trees$Genus[trees$Genus=="Caloxylon"] <- "Merrillia"
trees$Genus[trees$Genus=="Cavanalesia"] <- "Cavanillesia"
trees$Genus[trees$Genus=="Chysoceton"] <- "Unknown"
trees$Genus[trees$Genus=="Clochidion"] <- "Unknown"
trees$Genus[trees$Genus=="Cytogononia"] <- "Unknown"
trees$Genus[trees$Genus=="Eonimus"] <- "Euonymus"
trees$Genus[trees$Genus=="Escalonia"] <- "Escallonia"
trees$Genus[trees$Genus=="Euricoma"] <- "Eurycoma"
trees$Genus[trees$Genus=="Ferminiana"] <- "Unknown"
trees$Genus[trees$Genus=="Fragraea"] <- "Fagraea"
trees$Genus[trees$Genus=="Graffenriedia"] <- "Unknown"
trees$Genus[trees$Genus=="Hyeronima"] <- "Hieronyma"
trees$Genus[trees$Genus=="Hynocarpus"] <- "Hydnocarpus"
trees$Genus[trees$Genus=="Julbelniadia"] <- "Julbernardia"
trees$Genus[trees$Genus=="Lagerstromia"] <- "Lagerstroemia"
trees$Genus[trees$Genus=="Lauraceae"] <- "Unknown" #Family not genus
trees$Genus[trees$Genus=="Malanochyla"] <- "Unknown"
trees$Genus[trees$Genus=="Mdumbadumba"] <- "Unknown"
trees$Genus[trees$Genus=="Metrododorea"] <- "Unknown"
trees$Genus[trees$Genus=="Meyogine"] <- "Meiogyne"
trees$Genus[trees$Genus=="Microsdesmis"] <- "Microdesmis"
trees$Genus[trees$Genus=="Mnunganunga"] <- "Unknown"
trees$Genus[trees$Genus=="Mollotus"] <- "Mallotus"
trees$Genus[trees$Genus=="Msengela"] <- "Unknown"
trees$Genus[trees$Genus=="Muntinga"] <- "Unknown"
trees$Genus[trees$Genus=="Neouvaria"] <- "Neo-uvaria"
trees$Genus[trees$Genus=="Octodes"] <- "Unknown"
trees$Genus[trees$Genus=="Onychapetalum"] <- "Unknown"
trees$Genus[trees$Genus=="Papilionoidea"] <- "Unknown"
trees$Genus[trees$Genus=="Physocalymna"] <- "Physocalymma"
trees$Genus[trees$Genus=="Rhinorea"] <- "Unknown"
trees$Genus[trees$Genus=="Rhytigynia"] <- "Unknown"
trees$Genus[trees$Genus=="Rnodostemomodaphne"] <- "Unknown"
trees$Genus[trees$Genus=="Saccopethalum"] <- "Saccopetalum"
trees$Genus[trees$Genus=="Schweilera"] <- "Eschweilera"
trees$Genus[trees$Genus=="Staphyllea"] <- "Staphylea"
trees$Genus[trees$Genus=="Ticodendrom"] <- "Ticodendron"
trees$Genus[trees$Genus=="Vuguierenthus"] <- "Unknown"
trees$Genus[trees$Genus=="Yrianthera"] <- "Iryanthera"
# Tree families
trees$Family[trees$Family=="Acanthaceae Juss."] <- "Acanthaceae"
trees$Family[trees$Family=="Annoniaceae"] <- "Annonaceae"
trees$Family[trees$Family=="Apocynaceae Juss."] <- "Apocynaceae"
trees$Family[trees$Family=="Aquifoliaceae Bercht. & J. Presl"] <- "Aquifoliaceae"
trees$Family[trees$Family=="Asteraceae Bercht. & J. Presl"] <- "Asteraceae"
trees$Family[trees$Family=="Berseraceae"] <- "Burseraceae"
trees$Family[trees$Family=="Capparaceae Juss."] <- "Capparaceae"
trees$Family[trees$Family=="Capparidaceae"] <- "Capparaceae"
trees$Family[trees$Family=="Caryocaceae"] <- "Caryocaraceae"
trees$Family[trees$Family=="Celastomataceae"] <- "Melastomataceae"
trees$Family[trees$Family=="Chlorantaceae"] <- "Chloranthaceae"
trees$Family[trees$Family=="Chrysobalanaceae R. Br."] <- "Chrysobalanaceae"
trees$Family[trees$Family=="Cluciaceae"] <- "Clusiaceae"
trees$Family[trees$Family=="Cornaceae Bercht. & J. Presl"] <- "Cornaceae"
trees$Family[trees$Family=="Eleaocarpaceae"] <- "Elaeocarpaceae"
trees$Family[trees$Family=="Euphorbiacae"] <- "Euphorbiaceae"
trees$Family[trees$Family=="Euphorbiacaea"] <- "Euphorbiaceae"
trees$Family[trees$Family=="Euphorbiaceae Juss."] <- "Euphorbiaceae"
trees$Family[trees$Family=="Fabaceae-Caesalpinioideae"] <- "Fabaceae"
trees$Family[trees$Family=="Fabaceae Lindl."] <- "Fabaceae"
trees$Family[trees$Family=="Fabaceae-Mimosoideae"] <- "Fabaceae"
trees$Family[trees$Family=="Fabaceae-Papilionoideae"] <- "Fabaceae"
trees$Family[trees$Family=="Familia Incogn."] <- "Unknown"
trees$Family[trees$Family=="Hippocastenacea"] <- "Hippocastanaceae"
trees$Family[trees$Family=="Labiateae"] <- "Lamiaceae"
trees$Family[trees$Family=="Lacistemaceae"] <- "Lacistemataceae"
trees$Family[trees$Family=="Malvaceae Juss."] <- "Malvaceae"
trees$Family[trees$Family=="Meliaceae Juss."] <- "Meliaceae"
trees$Family[trees$Family=="Melianthaceae Horan."] <- "Melianthaceae"
trees$Family[trees$Family=="Mirsinaceae"] <- "Myrsinaceae"
trees$Family[trees$Family=="Monimiaceae Juss."] <- "Monimiaceae"
trees$Family[trees$Family=="Moraceae Gaudich."] <- "Moraceae"
trees$Family[trees$Family=="Myrtaceae Juss."] <- "Myrtaceae"
trees$Family[trees$Family=="Olaceae"] <- "Olacaceae"
trees$Family[trees$Family=="Olacaceae R. Br."] <- "Olacaceae"
trees$Family[trees$Family=="Oleaceae Hoffmanns. & Link"] <- "Oleaceae"
trees$Family[trees$Family=="Phyllantaceae"] <- "Phyllanthaceae"
trees$Family[trees$Family=="Phytollacaceae"] <- "Phytolaccaceae"
trees$Family[trees$Family=="Podocarpaceae Endl."] <- "Podocarpaceae"
trees$Family[trees$Family=="Primulaceae Batsch Ex Borkh."] <- "Primulaceae"
trees$Family[trees$Family=="Putranjivaceae Meisn."] <- "Putranjivaceae"
trees$Family[trees$Family=="Rhamnaceae Juss."] <- "Rhamnaceae"
trees$Family[trees$Family=="Rhizophoraceae Pers."] <- "Rhizophoraceae"
trees$Family[trees$Family=="Rosaceae Juss"] <- "Rosaceae"
trees$Family[trees$Family=="Rosaceae Juss."] <- "Rosaceae"
trees$Family[trees$Family=="Rubiaceae Juss."] <- "Rubiaceae"
trees$Family[trees$Family=="Rutaceae Juss."] <- "Rutaceae"
trees$Family[trees$Family=="Sapindaceae Juss."] <- "Sapindaceae"
trees$Family[trees$Family=="Sapotaceae Juss."] <- "Sapotaceae"
trees$Family[trees$Family=="Staphylaceae"] <- "Staphyleaceae"
trees$Family[trees$Family=="Torrecilliaceae"] <- "Unknown"
trees$Family[trees$Family=="Urticaceae Juss."] <- "Urticaceae"

# Add decimal KRP plot with inflated diameters until issue is resolved in the database
trees$Diameter[trees$OnehaPlotNumber=="VG-KRP-1" & trees$SamplingPeriod=="2011.01"] <- trees$Diameter[trees$OnehaPlotNumber=="VG-KRP-1"& trees$SamplingPeriod=="2011.01"]/10

# Add decimal to 2012 BCI stems with inflated diameters (all >100) until issue is resolved in the database

# Add decimal to 6 stems in plot VG-COU-5 that have inflated diameters until issue is resolved in the database
trees$Diameter[trees$OnehaPlotNumber=="VG-COU-5" & trees$Diameter==392] <- 39.2
trees$Diameter[trees$OnehaPlotNumber=="VG-COU-5" & trees$Diameter==525] <- 52.5
trees$Diameter[trees$OnehaPlotNumber=="VG-COU-5" & trees$Diameter==564] <- 56.4
trees$Diameter[trees$OnehaPlotNumber=="VG-COU-5" & trees$Diameter==603] <- 60.3
trees$Diameter[trees$OnehaPlotNumber=="VG-COU-5" & trees$Diameter==723] <- 72.3
trees$Diameter[trees$OnehaPlotNumber=="VG-COU-5" & trees$Diameter==1120] <- 112.0
# Add decimal to outlier diameter in plot VG-YAS-1
trees$Diameter[trees$OnehaPlotNumber=="VG-YAS-1" & trees$Diameter==420] <- 42.0

# Check to be sure that no other years had outlier values for these stems
trees$Diameter[trees$OnehaPlotNumber=="VG-COU-5" & trees$Diameter>300]

###############################################################################
# Exclude stems under 10 cm dbh
table(trees$Diameter <= 10)
trees <- filter(trees, Diameter >= 10)

save(trees, file='trees_clean.RData')
#' Very simple cloud detection for imagery with blue and thermal bands
#' 
#' @param x RasterBrick or RasterStack with reflectance and brightness temperature OR the mask of a previous run of \code{cloudMask} with \code{returnDiffLayer=TRUE}. 
#' @param threshold cloud detection threshold. If not provided it will be guessed. Everything *above* this threshold will be considered a cloud pixel (unless it is removed by filtering afterwards).
#' @param minCloudSize minimum number of cloud pixels in window1 
#' @param windowSize1 odd number, rectangular moving window to remove clouds which arre too small (likely artefacts)
#' @param windowSize2 odd number, rectangular buffer around cluster centers
#' @param sanitize logical. Should small clouds (possibly false positives) be removed by filtering? If \code{TRUE} windowSize1 must be specified.
#' @param maskGrowing logical. Applies simple region-growing (rectangular buffering) to the cloud mask. If \code{TRUE} windowSize2 must be specified.
#' @param lowBand bandname or number for the blue band
#' @param tirBand bandname or number for the thermal band
#' @param plot logical. Provides plots of the cloud mask for all sub-steps (sanitizing etc.) Helpful to find proper parametrization.
#' @param verbose logical. Print messages or supress.
#' @param returnDiffLayer logical. If \code{TRUE}, the difference layer will be returned along with the cloudmask. This option allows to re-use the difference layer in cloudMask.
#' @note Typically clouds are cold in the thermal region and have high reflectance in short wavelengths (blue). By differencing the two bands and thresholding a rough cloud mask can be obtained.
#' More sophisticated approaches can be found elsewhere, e.g. \link[https://code.google.com/p/fmask/]{fmask}.
#' 
#' It can make sense to find a suitable threshold on a cropped version of the scene. Also make sure you make use of the \code{returnDiffLayer} argument to save yourself one processing step.
#' Sanitizing and region growing can be seen as final polishing, i.e. as long as the pure cloud centers are not detected properly, you can turn those two arguments off if they take too long to calculate.
#' Once your mask detects obvious cloud pixels properly re-enable sanitizing and regionGrowing for fine tuning if desired. Finally, once a suitable threshold is established re-run cloudMask on the whole scene with this threshold and go get a coffee.
#' @export
#' @examples 
#' \dontrun{
#' ls <- stackMeta("path/to/MTL.txt")
#' ls_cor <- radCor(ls, "path/to/MTL.txt") 
#' ls_cmask <-cloudMask(ls_cor, returnDiffLayer = TRUE)
#' }
cloudMask <- function(x, threshold, minCloudSize, windowSize1 = 5, windowSize2 = 11, maskGrowing = TRUE, sanitize = TRUE, lowBand = "B1", tirBand = "B6", plot = TRUE, verbose = TRUE, returnDiffLayer = FALSE){
	
	## Set-up graphics device
	op <- par(mfrow = c(2, 1 + sum(sanitize, maskGrowing)))
	
	## Calculate or re-reuse cloud difference layer	
	if("CDIFF" %in% names(x)) {
		if(verbose) message("Re-using CDIFF layer from previous run.")
		cdiff <- x[["CDIFF"]]
	} else {
		cdiff <- x[[lowBand]] - x[[tirBand]]
		names(cdiff) <- "CDIFF"
	}
	
	## Guess threshold
	if(missing(threshold)) {
		threshold <- quantile(cdiff@data@max:cdiff@data@min, 0.45)
		if(verbose) {message(paste0("Estimated cloud threshold should be between ", round(cdiff@data@min), " and ", round(cdiff@data@max)) )
			message(paste0("Guessed threshold (rounded): ", round(threshold)))
		}
	}
	if(threshold < cdiff@data@min | threshold > cdiff@data@max) warning("Threshold is not within the estimated data range", call. = FALSE)
	
	if(plot) plot(cdiff, main = "Cloud layer: blue - tir difference")
	
	## Thresholding
	if(verbose) message("Begin thresholding")
	cmask <- cdiff > threshold
	cmask <- mask(cmask, cmask, maskvalue = 0)
	
	if(plot) plot(cmask, main = paste0("Cloud mask\nThreshold: ", threshold))
	
	
	## Remove "clouds" smaller than minCloudSize
	if(sanitize) {
		if(verbose) message("Begin sanitzing")
		if(missing(minCloudSize)) minCloudSize <- windowSize1 ^ 2
		w <- matrix(ncol = windowSize1, nrow = windowSize1, 1)
		if(minCloudSize >= windowSize^2) {
			cmod <- focal(cmask, w, na.rm = FALSE)
		} else {
			cmod <- focal(cmask, w, na.rm = TRUE)	
			cmod[cmod < minCloudSize] <- NA		
		}
		cmod[cmod < minCloudSize] <- NA
		cmod[!is.na(cmod)] <- 1L
		if(plot) plot(cmod, main = "Sanitized cloud mask")
		
	}
	
	## Buffer cloud centers (we could also do a circular buffer, but for now this should suffice)
	if(maskGrowing){
		if(verbose) message("Begin region-growing")
		w <- matrix(ncol = windowSize2, nrow = windowSize2, 1)
		cmod <- focal(cmod, w, na.rm = TRUE )
		cmod[!is.na(cmod)] <- 1L
		if(plot) plot(cmod, main = "Region-grown cloud mask")
		
	}
	
	if(plot){
		plotRGB(x, 1, 2, 3, title = "Final mask", stretch = "lin")
		plot(cmod,  legend = FALSE, add = T, col = "yellow")
	}
	
	## Reset par
	par(op)
	
	## Return
	names(cmod) <- "CMASK"
	if(returnDiffLayer) cmod <- stack(cmod, cdiff)
	return(cmod)	
}# Function to test if ENVI will load in IDL
check_ENVI_IDL <- function(idl) {
    idl_out <- system(paste(shQuote(idl), '-e "e=ENVI(/HEADLESS)"'), 
                      intern=TRUE)
    if (sum(grepl("Restored file: ENVI", idl_out)) > 0) {
        return(TRUE)
    } else {
        return(FALSE)
    }
}

# Function to ensure only character variables handed to IDL are quoted
format_IDL_param <- function(varname, varvalue) {
    if (is.character(varvalue)) {
        param <- paste0(varname, '="', varvalue, '"\n')
    } else if (is.list(varvalue)) {
        param <- paste0(varname, '=[')
        if (length(varvalue) > 0) {
            for (n in 1:length(varvalue)) {
                if (is.numeric(varvalue[n])) {
                    param <- paste0(param, varvalue[n])
                } else {
                    param <- paste0(param, '"', varvalue[n], '"')
                }
                if (n != length(varvalue)) {
                    param <- paste0(param, ', ')
                }
            }
        }
        param <- paste0(param, ']\n')
    } else {
        param <- paste0(varname, '=', varvalue, '\n')
    }
    return(param)
}

#' @importFrom tools file_path_sans_ext
cloud_remove_IDL <- function(cloudy, clear, cloud_mask, out_name,
                             algorithm, num_class, min_pixel, max_pixel, 
                             cloud_nbh, DN_min, DN_max, 
                             verbose, idl, byblock, overwrite,
                             patch_long=1000) {
    if (verbose > 0) {
        warning("verbose not supported with CLOUD_REMOVE and CLOUD_REMOVE_FAST algorithms")
    }
    if (algorithm == 'CLOUD_REMOVE_FAST') {
        script_path <- system.file("idl", "CLOUD_REMOVE_FAST.pro", 
                                   package="teamlucc")
        function_name <- 'CLOUD_REMOVE_FAST'
    } else if (algorithm == 'CLOUD_REMOVE') {
        script_path <- system.file("idl", "CLOUD_REMOVE.pro", 
                                   package="teamlucc")
        function_name <- 'CLOUD_REMOVE'
    } else {
        stop(paste0('unrecognized cloud fill algorithm "', algorithm, '"'))
    }
    
    if (!(file_test('-x', idl) || file_test('-f', idl))) {
        stop('IDL not found - check "idl" parameter')
    }

    if (!check_ENVI_IDL(idl)) {
        stop("Unable to load ENVI in IDL - do you have ENVI and IDL licenses, and ENVI >= 5.0?")
    }

    if (!byblock) {
        patch_long <- max(dim(cloudy)) + 1
    }

    # Save proj4string and extent to ensure the same proj4string and extent is 
    # returned even if they are changed by IDL
    orig_proj <- proj4string(cloudy)
    orig_ext <- extent(cloudy)

    # Write in-memory rasters to files for hand off to IDL. The capture.output 
    # line is used to avoid printing the rasterOptions to screen as they are 
    # temporarily reset.
    dummy <- capture.output(def_format <- rasterOptions()$format)
    rasterOptions(format='ENVI')
    cloudy <- writeRaster(cloudy, rasterTmpFile(), 
                          datatype=dataType(cloudy)[1])
    clear <- writeRaster(clear, rasterTmpFile(), datatype=dataType(clear)[1])
    cloud_mask <- writeRaster(cloud_mask, rasterTmpFile(), 
                              datatype=dataType(cloud_mask)[1])
    cloudy_file <- filename(cloudy)
    clear_file <- filename(clear)
    cloud_mask_file <- filename(cloud_mask)
    dummy <- capture.output(rasterOptions(format=def_format))

    param_names <- c("cloudy_file", "clear_file", "mask_file", "out_name", 
                     "num_class", "min_pixel", "extent1", "DN_min", "DN_max", 
                     "patch_long")
    param_vals <- list(cloudy_file, clear_file, cloud_mask_file, out_name, 
                       num_class, min_pixel, cloud_nbh, DN_min, DN_max, 
                       patch_long)
    idl_params <- mapply(format_IDL_param, param_names, param_vals)
    idl_params <- paste(idl_params, collapse='')

    script_dir <- dirname(script_path)
    idl_script <- tempfile(fileext='.pro')
    idl_cmd <- paste0('CD, "', script_dir, '"\n', idl_params, function_name, ',', 
                      paste(param_names, collapse=','), '\nexit')

    f <- file(idl_script, 'wt')
    writeLines(idl_cmd, f)
    close(f)

    idl_out <- system(paste(shQuote(idl), shQuote(idl_script)), intern=TRUE)

    log_file <- paste0(file_path_sans_ext(out_name), '_idllog.txt')
    idl_out <- gsub('\r', '', idl_out)
    f <- file(log_file, 'wt')
    writeLines(idl_out, f) 
    close(f)

    filled <- brick(out_name)

    filled[filled < DN_min] <- NA

    # Ensure original proj4string and extent are saved and returned
    proj4string(filled) <- orig_proj
    extent(filled) <- orig_ext
    filled <- writeRaster(filled, filename=out_name, overwrite=TRUE, 
                          datatype=dataType(filled)[1])
    return(filled)
}

# Wrapper around C++ cloud fill function, to enable calling the function with 
# rasterEngine
#' @import Rcpp
cloud_fill_rasterengine <- function(cloudy, clear, cloud_mask, algorithm, 
                                    num_class, min_pixel, max_pixel, cloud_nbh, 
                                    DN_min, DN_max, verbose, ...) {
    dims=dim(cloudy)
    # RcppArmadillo crashes when you pass it a cube, so resize and pass 
    # mats
    cloudy <- array(cloudy, dim=c(dims[1] * dims[2], dims[3]))
    clear <- array(clear, dim=c(dims[1] * dims[2], dims[3]))
    cloud_mask <- array(cloud_mask, dim=c(dims[1] * dims[2]))
    filled <- call_cpp_cloud_fill(cloudy, clear, cloud_mask, algorithm, dims, 
                                  num_class,  min_pixel, max_pixel, cloud_nbh, 
                                  DN_min, DN_max, verbose)
    # RcppArmadillo crashes when you return a cube, so resize the returned 
    # mat
    filled <- array(filled, dim=c(dims[1], dims[2], dims[3]))
    return(filled)
}

# This function decides which RcppArmadillo exported function to call: 
# cloud_fill, or cloud_fill_simple
call_cpp_cloud_fill <- function(cloudy, clear, cloud_mask, algorithm, dims, 
                                num_class, min_pixel, max_pixel, cloud_nbh, 
                                DN_min, DN_max, verbose, ...) {
    if (algorithm == "teamlucc") {
        filled <- cloud_fill(cloudy, clear, cloud_mask, dims, num_class, 
                             min_pixel, max_pixel, cloud_nbh, DN_min, DN_max, 
                             verbose)
    } else if (algorithm == "simple") {
        filled <- cloud_fill_simple(cloudy, clear, cloud_mask, dims, num_class, 
                                    cloud_nbh, DN_min, DN_max, verbose)
    } else {
        stop(paste0('unrecognized cloud fill algorithm "', algorithm, '"'))
    }
    return(filled)
}

#' @importFrom spatial.tools rasterEngine
cloud_remove_R <- function(cloudy, clear, cloud_mask, out_name, algorithm, 
                           num_class, min_pixel, max_pixel, cloud_nbh, DN_min, 
                           DN_max, verbose, byblock, overwrite) {
    # Note that call_cpp_cloud_fill uses the algorithm to decide whether to 
    # call cloud_fill or cloud_fill_simple (and call_cpp_cloud_fill is called 
    # by cloud_fill_rasterengine)
    if (byblock) {
        bs <- blockSize(cloudy)
        out <- brick(cloudy, values=FALSE)
        out <- writeStart(out, out_name, overwrite=overwrite)
        for (block_num in 1:bs$n) {
            if (verbose > 0) {
                message("Processing block ", block_num, " of ", bs$n, "...")
            }
            dims <- c(bs$nrows[block_num], ncol(cloudy), nlayers(cloudy))
            cloudy_bl <- array(getValuesBlock(cloudy, row=bs$row[block_num],
                                              nrows=bs$nrows[block_num]),
                               dim=c(dims[1] * dims[2], dims[3]))
            clear_bl <- array(getValuesBlock(clear, row=bs$row[block_num],
                                            nrows=bs$nrows[block_num]),
                            dim=c(dims[1] * dims[2], dims[3]))
            cloud_mask_bl <- array(getValuesBlock(cloud_mask, 
                                                  row=bs$row[block_num], 
                                                  nrows=bs$nrows[block_num]), 
                                   dim=c(dims[1] * dims[2]))
            filled <- call_cpp_cloud_fill(cloudy_bl, clear_bl, cloud_mask_bl, 
                                          algorithm, dims, num_class, 
                                          min_pixel, max_pixel, cloud_nbh, 
                                          DN_min, DN_max, verbose>1)
            out <- writeValues(out, filled, bs$row[block_num])
        }
        out <- writeStop(out)
        # out <- rasterEngine(cloudy=cloudy, clear=clear, 
        # cloud_mask=cloud_mask,
        #                     fun=cloud_fill_rasterengine,
        #                     args=list(algorithm=algorithm, num_class=num_class, 
        #                               min_pixel=min_pixel, max_pixel=max_pixel, 
        #                               cloud_nbh=cloud_nbh, DN_min=DN_min, 
        #                               DN_max=DN_max, verbose=verbose),
        #                     processing_unit='chunk',
        #                     outbands=nlayers(cloudy), outfiles=1,
        #                     verbose=verbose,
        #                     filename=out_name)
    } else {
        dims <- dim(cloudy)
        out_datatype <- dataType(cloudy)[1]
        out <- brick(cloudy, values=FALSE, filename=out_name)
        # RcppArmadillo crashes when you pass it a cube, so resize and pass 
        # mats
        cloudy <- array(getValues(cloudy), dim=c(dims[1] * dims[2], dims[3]))
        clear <- array(getValues(clear), dim=c(dims[1] * dims[2], dims[3]))
        cloud_mask <- array(getValues(cloud_mask), dim=c(dims[1] * dims[2]))
        filled <- call_cpp_cloud_fill(cloudy, clear, cloud_mask, algorithm, 
                                      dims, num_class, min_pixel, max_pixel, 
                                      cloud_nbh, DN_min, DN_max, verbose>1)
        out <- setValues(out, filled)
        out <- writeRaster(out, out_name, datatype=out_datatype, 
                           overwrite=overwrite)
    }

    return(out)
}

#' Remove clouds From Landsat imagery
#'
#' This code uses one of several different algorithms (depending on the 
#' settings, see Details) to fill heavy clouds in a Landsat image.
#'
#' The \code{algorithm} parameter determines what algorithm is used for the 
#' cloud fill. \code{algorithm} must be one of: "CLOUD_REMOVE", 
#' "CLOUD_REMOVE_FAST", "teamlucc", or "simple" (the default). If set to 
#' "CLOUD_REMOVE" the script uses a (slightly modified to be called from R) 
#' version of  Xiaolin Zhu's NSPI IDL code. If set to "CLOUD_REMOVE_FAST", the 
#' algorithm uses the "fast" version of Xiaolin's code. Both of these two 
#' algorithms require an IDL license to run (and therefore \code{idl_path} must 
#' be set).  The "teamlucc" algorithm uses a version of the NSPI algorithm 
#' (based on the CLOUD_REMOVE code) that is coded in C++ and  can be run from R 
#' without an IDL license. The "simple" algorithm uses a cloud fill model that 
#' is based on fitting a linear model to the surface reflectance from the clear 
#' image in a window around each cloud, and using this linear model to predict 
#' reflectance in unobserved (cloudy) areas.
#'
#' @export
#' @param cloudy the cloudy image (base image) as a \code{Raster*}
#' @param clear the clear image as a \code{Raster*} to use for filling 
#' \code{img_cloudy}
#' @param cloud_mask the cloud mask as a \code{RasterLayer}, with each cloud 
#' patch assigned a unique integer code. Areas that are clear in both 
#' \code{cloudy_rast} and \code{clear_rast} should be coded 0, while areas that 
#' are clouded in \code{clear_rast} should be coded -1.
#' @param out_name filename for cloud filled image
#' @param algorithm must be one of: "CLOUD_REMOVE", "CLOUD_REMOVE_FAST", 
#' "teamlucc", or "simple". Default is "simple". See Details.
#' @param num_class set the estimated number of classes in image
#' @param min_pixel the sample size of similar pixels (ignored when 
#' \code{algorithm==TRUE})
#' @param max_pixel the maximum sample size to search for similar pixels 
#' (ignored when \code{algorithm==TRUE})
#' @param cloud_nbh the range of cloud neighborhood (in pixels)
#' @param DN_min the minimum valid DN value (default of 0)
#' @param DN_max the maximum valid DN value (default of 10000 assumes 2 byte 
#' integer imagery)
#' @param idl path to the IDL binary on your machine (on Windows, the path to 
#' idl.exe)
#' @param verbose whether to print detailed status messages. Set to FALSE or 0 
#' for no status messages. Set to 1 for basic status messages. Set to 2 for 
#' detailed status messages.
#' @param byblock whether to process images block by block 
#' (\code{byblock=TRUE}) or all at once (\code{byblock=FALSE}). Use 
#' \code{byblock=FALSE} with caution, as this option will cause the cloud fill 
#' routine to consume a large amount of memory.
#' @param overwrite whether to overwrite \code{out_name} if it already exists
#' @param ... additional arguments passed to the chosen cloud fill routine
#' @return \code{Raster*} with cloud-filled image
#' @references Zhu, X., Gao, F., Liu, D., Chen, J., 2012. A modified
#' neighborhood similar pixel interpolator approach for removing thick clouds 
#' in Landsat images. Geoscience and Remote Sensing Letters, IEEE 9, 521--525.
#' @examples
#' \dontrun{
#' cloudy <- raster(system.file('tests', 'testthat_idl', 'cloud_remove', 
#' 'L20080724_cloudy', package='teamlucc'))
#' clear <- raster(system.file('tests', 'testthat_idl', 'cloud_remove', 
#' 'L20080606', package='teamlucc'))
#' cloud_mask <- raster(system.file('tests', 'testthat_idl', 'cloud_remove', 
#' 'cloud_mask', package='teamlucc'))
#' filled <- cloud_remove(cloudy, clear, cloud_mask, fast=TRUE)
#' }
cloud_remove <- function(cloudy, clear, cloud_mask, out_name=NULL, 
                         algorithm='simple',
                         num_class=4, min_pixel=20, max_pixel=1000, 
                         cloud_nbh=10, DN_min=0, DN_max=10000, 
                         idl="C:/Program Files/Exelis/IDL83/bin/bin.x86_64/idl.exe",
                         verbose=FALSE, byblock=TRUE, overwrite=FALSE, ...) {
    if (!(algorithm %in% c('CLOUD_REMOVE', 'CLOUD_REMOVE_FAST', 'teamlucc', 
                           'simple'))) {
        stop('algorithm must be one of "CLOUD_REMOVE", "CLOUD_REMOVE_FAST", "teamlucc", or "simple"')
    }

    if (verbose > 0) {
        message('Using "', algorithm, '" algorithm.')
    }
    
    if (!(class(cloudy) %in% c("RasterLayer", "RasterStack", "RasterBrick"))) {
        stop('cloudy must be a Raster* object')
    }
    if (!(class(clear) %in% c("RasterLayer", "RasterStack", "RasterBrick"))) {
        stop('clear must be a Raster* object')
    }
    if (!(class(cloud_mask) %in% c("RasterLayer"))) {
        stop('cloud_mask must be a RasterLayer object')
    }
    compareRaster(cloudy, clear)
    if (nlayers(cloudy) != nlayers(clear)) {
        stop('number of layers in cloudy must match number of layers in clear')
    }
    if (nlayers(cloud_mask) != 1) {
        stop('cloud_mask should have only one layer')
    }

    if (is.null(out_name)) {
        out_name <- rasterTmpFile()
    } else {
        out_name <- normalizePath(out_name, mustWork=FALSE)
        if (!file_test('-d', dirname(out_name))) {
            stop('output folder does not exist')
        }
        if (file_test('-f', out_name) & !overwrite) {
            stop('output file already exists - use a different "out_name"')
        }
    }
    
    if (algorithm %in% c('CLOUD_REMOVE', 'CLOUD_REMOVE_FAST')) {
        filled <- cloud_remove_IDL(cloudy, clear, cloud_mask, out_name,
                                   algorithm, num_class, min_pixel, max_pixel, 
                                   cloud_nbh, DN_min, DN_max, verbose, idl, 
                                   byblock, overwrite, ...)
    } else if (algorithm %in% c('teamlucc', 'simple')) {
        filled <- cloud_remove_R(cloudy, clear, cloud_mask, out_name, 
                                 algorithm, num_class, min_pixel, max_pixel, 
                                 cloud_nbh, DN_min, DN_max, verbose, byblock, 
                                 overwrite, ...)
    } else {
        stop(paste0('unrecognized cloud fill algorithm "', algorithm, '"'))
    }

    names(filled) <- names(cloudy)
    return(filled)
}

clouds <- function(band1, band6, level = 0.0014, buffer=5) {
    # simple function to create a cloud mask
    # uses bands 6 and 1 as specified in the arguments
    # cloud target: band6 is low; band1 is high
   
    # clouds have high reflectance in band1 and
    # low temperature (band 6)
    # thus ratio band1/band6 is high for clouds

    if(is.character(band1)) {
        band1 <- read.asciigrid(band1)
        results <- band1
        dims <- band1@grid@cells.dim
        band1 <- band1@data[,1]
    } else {
        if(class(band1) == "SpatialGridDataFrame") {
            results <- band1
            dims <- band1@grid@cells.dim
            band1 <- band1@data[,1]
        } else {
            results <- band1
            dims <- dim(as.matrix(band1))
            band1 <- as.vector(as.matrix(band1))
        }
    } 
    
    if(is.character(band6)) {
        band6 <- read.asciigrid(band6)
        band6 <- band6@data[,1]
    } else {
        if(class(band6) == "SpatialGridDataFrame") {
            band6 <- band6@data[,1]
        } else {
            band6 <- as.vector(as.matrix(band6))
        }
    } 

           
    cloudmask <- ifelse(band1/band6 > level, 1, 0) # 6 low AND 1 high
    ## want to add a buffer around identified areas
    cloudmask <- movingwindow(matrix(cloudmask,  nrow=dims[1], ncol=dims[2]), matrix(rep(1, buffer*buffer), buffer, buffer))
    cloudmask <- ifelse(as.vector(cloudmask) > 0, 1, NA)

    # return the same structure as the input values
    if(class(results) == "SpatialGridDataFrame")
        results@data[,1] <- cloudmask
    else if(is.data.frame(results))
        results <- data.frame(matrix(cloudmask, nrow=nrow(results), ncol=ncol(results)))
    else if(is.matrix(results))
        results <- matrix(cloudmask, nrow=nrow(results), ncol=ncol(results))
    else # return a vector 
        results <- cloudmask
    
    results
}

#' Assign class labels to classification file
#'
#' @export
#' @importFrom rgdal writeGDAL
#' @importFrom sp SpatialPixelsDataFrame
#' @param x classified image as \code{RasterLayer}
#' @param cls two column matrix, where the first column is the class codes 
#' (integers) and the second column is the class names
#' @param outfile the filename to use for the output
#' @examples
#' #TODO: Add examples
color_image <- function(x, cls, outfile) {
    # Reclassify image so it is coded from 0 - length(cls[1]). ENVI and 
    # other classification file formats rely on the codes being sequential, 
    # starting at zero.
    x <- reclassify(x, cbind(cls[, 1], seq(0, length(cls[, 1]) - 1)))
    x.sp <- as(x, "SpatialPixelsDataFrame")
    cls_colors <- t(col2rgb(cls[, 1]))
    # Select appropriate data type and missing value tag depending on data 
    # attributes.
    if (max(x.sp$layer) > 254) {
        gdaltype <- 'Int16'
        gdalmvFlag <- -32768
    } else {
        gdaltype <- 'Byte'
        gdalmvFlag <- 255
    }
    writeGDAL(x.sp, outfile, drivername="ENVI", type=gdaltype,
              colorTables=list(cls_colors), catNames=list(as.character(cls[, 2])),
              mvFlag=gdalmvFlag)
}
#' Calculate a contingency table using the composite operator
#'
#' This function calculates a cross tabulation for a map comparison using the 
#' composite operator recommended by Pontius and Cheuk (2006).
#'
#' @export
#' @return matrix with contingency table
#' @references Pontius, R. G., and M. L. Cheuk. 2006.  A generalized 
#' cross-tabulation matrix to compare soft-classified maps at multiple 
#' resolutions. International Journal of Geographical Information Science 
#' 20:1-30.
compcont <- function() {
    stop('compcont is not yet finished')
    #TODO: finish coding this function
}
# ==================================================================================
# Create Radimoetric and Topographic corrected files from Landsat 8 OLI
# Paulo E. Cardoso 03-03-2014
# Version v8
# R version 3.0.2 rgdal_0.8-16  raster_2.2-16 sp_1.0-14
## Utiliza dados de Digital Elevation Model (DEM) Aster (30m) ou SRTM (90m)
### ASTER GDEM: Usar o Tile ASTGTM2_S12E014
### SRTM v4.1: usar o Tile srtm_39_15
# ==================================================================================

# Packages necessarios
kpacks <- c("raster", "sp", "rgdal", 'rgeos')
new.packs <- kpacks[!(kpacks %in% installed.packages()[ ,"Package"])]
if(length(new.packs)) install.packages(new.packs)
lapply(kpacks, require, character.only=T)
remove(kpacks, new.packs)

sessionInfo() # basics of used session
# R version 3.0.2 (2013-09-25)
# [1] rgeos_0.3-3   rgdal_0.8-16  raster_2.2-16 sp_1.0-14

# Folders --------------------------------------------------------------------------
## Adjust for Local work paths
dir.work <- 'mfolder' # Alterar para Disco Local
dir.rst   <- 'rst' # Criar no dir.work do disco local
dir.tif <- 'tif'  # Criar no dir.work do disco local
dir.shp <- './Shapefiles_StudySite'
dir.aster <- './aster' # Alterar para o Local Folder
dir.srtm <- '/.srtm' # Alterar para o Local Folder
### Data das Landsat. Define as many as necesary.
### dir.fun must be changed for all analysis

dir.landsat <- 'LC81810682014157LGN00' # Folder with Landast 8 TIF files
#' Satelite parameters and data ----------------------------------------------------
## All subsequente analysis is for a single date
dir.fun <- dir.landsat # Change here to perform all subsequent analysis!

#'# Create a tif folder for raster and images outputs
dir.create(file.path(dir.work, dir.fun, dir.tif))

#'# Saving and Loading R data session ------------------------------------------------
save.image(file.path(dir.work, dir.fun, paste0(dir.fun,'.RData')))
load(file.path(dir.work, dir.fun, paste0(dir.fun,'.RData'))

#'# Landsat 8 Scene Metadata: MTL File ---------------------------------------------
#'## Metadata file contains scene acquisition details and correction parameters
#'## Unzip the MTL file from landsat L8 zipped .tar file
#'## MTL must be with Landsat TIF files, at the exact same folder
mtl <- read.delim(file.path(dir.work, dir.fun,  
                            grep('_MTL.txt',
                                 list.files(file.path(dir.work, dir.fun),
                                            all.files = F),
                                 ignore.case = TRUE, value = TRUE)),
                  sep = '=', stringsAsFactors = F)
mtl[grep("DATE_ACQUIRED", mtl$GROUP), 2]
mtl[grep("LANDSAT_SCENE_ID", mtl$GROUP), 2]

#' Local parameters ----------------------------------------------------------------
#'# Projections
#p.utm28n <- CRS("+init=epsg:32628") # UTM 28N Landsat Images
p.utm33n <- CRS("+init=epsg:32633") # UTM 33N Landsat Images
p.utm33s <- CRS("+init=epsg:32733") # UTM 33S Landsat Images
p.wgs84 <- CRS("+init=epsg:4326") # WGS84 Long Lat

#' Study site frame extent ----------------------------------------------------------
#'## Create rectangular area for image cropping
#'## A projected spatialpolydf will be created and projected to utm33N
ae <- readOGR(dsn = file.path(dir.shp), layer = 'layername')
proj4string(ae) <- p.utm33s # Asign projection WGS84
if(!is.projected(ae)) ae <- spTransform(ae, p.utm33n)
ae <- spTransform(ae, p.utm33n) # if UTM projection is South
#'## Create Extent from ae or provide another Shape for a different extent 
roi <- extent(ae) # a rectangular area covering ae polygon extent

#'---------------TEST ONLY ---------------------------
#'## Smaller subarea of ROI for test purposes --------
roi2 <- as(roi - c(3000, 7000), 'SpatialPolygons')
proj4string(roi2) <- p.utm33n
ae2 <- gIntersection(ae, roi2, byid = TRUE)
plot(roi2, axes = T);plot(ae2, add = T)
#'# Morro Moco Study Area --
roimoco <- extent(c(15.15, 15.18, -12.45, -12.40))
moco <- as(roimoco, 'SpatialPolygons')
proj4string(moco) <- p.wgs84 # Asign projection WGS84
mocoutm <- spTransform(moco, p.utm33n)
#'# --------------------------------------------------
#'----------------------------------------------------

#' Build Mask Raster used to crop images to ROI area --------------------------------
#'# Mask file must be created. It is a mandatory step of the process
#'# considering the way it was setup
#'## 1st: Read a single Band to get the desired extent based on satellite images
#'## Change function arguments for the ROI and polygon to apply mask
f.CreateRoiMask <- function(x = x, roi = roi2, maskpoly = ae2){
  x <- grep(".tif$", list.files(file.path(dir.work, dir.fun), all.files = F),
            ignore.case = TRUE, value = TRUE)[1] 
  i.band <- raster(file.path(dir.work, dir.fun, x),
                   package = "raster")
  #dataType(band) # Must be INT2U for Landsat 8. Range of Values: 0 to 65534
  stopifnot(!is.na(i.band@crs)) # Check projection
  # Create Extent object from ae shapefile
  if(is.null(roi)){
    i.roi <- extent(ae2)
  } else i.roi <- extent(roi)
  # Crop Landsat Scene to AE extent
  i.bandae <- crop(i.band, i.roi) # Crop band to AE Extent
  ## 2nd: Create the Mask raster to the croped band extent
  ae.r <- i.bandae # Raster AE: resolucao 30m (Landast)
  ae.r[] <- 1 # Defalt value
  ## Overlay AE poly to AE Extent raster
  ### Mask will have 1 and NA values
  msk.ae <- mask(ae.r, maskpoly, updatevalue=NA)
  #dataType(mask_ae) <- "INT1U" 
  ## Evaluate rasters
  stopifnot(compareRaster(msk.ae, i.bandae)) 
  msk.ae
}

#'## Run the function
#'## Mask will be a c(1, NA) rasterLayer
if(exists('mask.ae')) remove(mask.ae)
mask.ae <- f.CreateRoiMask(x = x, roi = roi, maskpoly = ae)

plot(mask.ae); summary(mask.ae)
writeRaster(mask.ae, filename = file.path(dir.work, dir.landsat, dir.tif,
                                          "mask_ae.asc"),
            overwrite = T)

#'## USER Topographic Correction function: Cosine, CCorrection and Minnaert
#'### Read and Crop ASTER or SRTM DEM
#'### !!! REVER resample do DEM antes de obter slope e aspect !!!
f.ReadDEM <- function(elev = 'srtm', unit = 'radians', roi = mask.ae, lon = lon, lat = lat){
  i.dem <- c('aster', 'srtm', 'SRTM', 'user')
  vdem <- pmatch(elev, i.dem)
  if (is.na(vdem)) 
    stop("invalid dem")
  if (vdem == -1) 
    stop("unnavailable or typo error")
  if(vdem == 1){
    i.dfile <- 'ASTGTM2_S12E014_dem.tif' # For Angola (Kumbira)
    i.dtm <- raster(file.path(dir.aster, i.dfile),
                    package = "raster")
  } else if(vdem == 2){
    i.dfile <- 'srtm_39_15.tif'
    i.dtm <- raster(file.path(dir.srtm, i.dfile),
                    package = "raster")
  } else if(vdem == 3){
    
    i.dtm <- getData('SRTM', lon = lon, lat = lat)
  }
  stopifnot(is.na(projection(i.dtm)) != TRUE)
  ## Extended Area for crop and reproject
  stopifnot(class(roi)[1] %in% c('Extent', 'RasterLayer', "SpatialPolygons",
                                 'SpatialPolygonsDataFrame'))
  if(class(roi)[1] == 'Extent'){
    i.aepol <- as(roi, 'SpatialPolygons')
    proj4string(i.aepol) <- CRS(proj4string(mask.ae)) # Assign projection
  } else if(class(roi)[1] == 'RasterLayer'){
    i.aepol <- as(extent(roi), 'SpatialPolygons')
    proj4string(i.aepol) <- CRS(proj4string(mask.ae)) # Assign projection   
  } else i.aepol <- roi
  # Extent for WGS84 with a buffer
  i.ae2 <- extent(spTransform(i.aepol, CRS(proj4string(i.dtm)))) + 0.005
  # CROP DEM
  i.dtm <- crop(i.dtm, i.ae2) # Crop to expanded area
  ### Calculate slope and aspect
  slpspct <- raster:::terrain(i.dtm, opt=c('slope', 'aspect'),
                              unit=unit)
  stckdem <- stack(i.dtm, slpspct)
  ### Change Projection to Landsat UTM adjusting for 30m resolution
  i.dem_p <- projectRaster(stckdem, crs = CRS(proj4string(mask.ae)),
                           res = 30, method ='ngb')
  i.dem_p <- crop(i.dem_p, mask.ae)
  i.dem_pr <- resample(i.dem_p, mask.ae, method = "ngb")
  ### Resample to match Study Site extent (and Landsat crop images)
  stopifnot(compareRaster(i.dem_pr, mask.ae)) # Evaluate rasters 
  i.dem_pr # Return DEM for the AE extent as defined by v.ae
}
#'## Run it to create the DEM for the AE --
#'## SRTM demands a lat, lon to locate the tile
dem.ae <- f.ReadDEM(elev = 'srtm', unit = 'radians', roi = mask.ae, lon = 15, lat = -12)
# Export ROI DEM
writeRaster(dem.ae[[1]], filename = file.path(dir.work, dir.fun, dir.tif, 'dem_ngb_ae_30m'),
            datatype = 'FLT4S', format = 'RST',
            overwrite = TRUE)

#'# Functions for Radiometric and Topographic calibration Landsat 8 ------------------
### According to http://www.gisagmaps.com/landsat-8-atco-guide/, DOS may perform
#### better under some circumnstances.
#### More info at:
#### http://landsat.usgs.gov/Landsat8_Using_Product.php
#### http://landsat.usgs.gov/L8_band_combos.php : Band References
#### ESUN and OLI: http://landsat.usgs.gov/ESUN.php
## DN to uncorrected TOA reflectance: planetary reflectance
## Define the parent frame from where objects will be called

## Convert DN to TOA Top of Athmosphere or Planetary Reflectances
f.ToarL8 <- function(x=x, i=i){
  # uncorrected TOA reflectance
  i.toar <- (x * as.numeric(mtl[grep(paste0("REFLECTANCE_MULT_BAND_", i),
                                     mtl$GROUP), 2]) +
               as.numeric(mtl[grep(paste0("REFLECTANCE_ADD_BAND_", i),
                                   mtl$GROUP), 2])) 
  # Correct for Sun Elevation sin(Theta_SE)
  i.tse <- as.numeric(mtl[grep("SUN_ELEVATION", mtl$GROUP), 2])*pi/180 # radians
  i.sune <- sin(i.tse)
  i.toa <- i.toar / i.sune  
  i.toa
}

#'# for test purpose only. Do not run outside main function
i.toa <- f.ToarL8(x=i.crop, i=bands[i])
plot(i.toa)

#' Topographic correction -----------------------------------------------------------
# Lu et al 2008. Pixel-based Minnaert Correction..
# Vanonckelen et al 2013. The effect of atmospheric and topographic...

f.TopoCor <- function(x = x, i = i, method = 'minnaert',
                      slope, aspect, il.ae, sun.e, sun.z, sun.a) {
  message('Images will be corrected to planetary TOA reflectances')
  METHODS <- c('none', 'cosine', 'ccorrection', 'minnaert')
  method <- pmatch(method, METHODS)
  itoa <- f.ToarL8(x = x,  i = i)
  if(method == 1){
    message('Message: No Topo correction will be applied')
    xout <- itoa
  } else if(method == 2){
    message('Message: cosine will be applied')
    xout <- itoa * (cos(sun.z)/il.ae)
  } else if (method == 3) {
    message('Message: c_correction will be applied')
    subspl <- sample(1:ncell(itoa), floor(ncell(itoa)*0.50), rep = F)
    band.lm <- lm(as.vector(itoa[subspl]) ~ as.vector(il.ae[subspl]))$coefficients
    #band.lm <- lm(as.vector(i.toa) ~ as.vector(il.ae))$coefficients
    C <- band.lm[1]/band.lm[2]
    xout <- itoa * (cos(sun.z) + C)/(il.ae + C)
  } else if(method == 4) {
    message('Message: Minnaert will be applied')
    targetslope <- atan(0.05)
    if (all(itoa[slope >= targetslope] < 0, na.rm = TRUE)) {
      K <- 1
    } else {
      K <- data.frame(y = as.vector(itoa[slope >= targetslope]), 
                      x = as.vector(il.ae[slope >= targetslope])/cos(sun.z))
      K <- K[!apply(K, 1, function(x) any(is.na(x))), ]
      K <- K[K$x > 0, ]
      K <- K[K$y > 0, ]
      K <- lm(log10(K$y) ~ log10(K$x))
      K <- coefficients(K)[[2]]
      if (K > 1) 
        K <- 1
      if (K < 0) 
        K <- 0
    }
    xout <-(itoa * cos(sun.z))/((il.ae * cos(sun.z))^K)
  }
  xout
}

#'# for test purpose only. Do not run outside main function
ltest <- f.TopoCor(x = i.crop, i = 1, method = 'minnaert') # Test only

# ----------------------------------------------------------------------------------
# Main Function to create radiometric corrected Files ------------------------------
# For original Landsat Product only
# Function arguments:
## write: (TRUE/FALSE): Export RST raster file
## demcorr: ('none', cosine, ccorrection, minnaert). Topographic correction algorithm 
## mask: (TRUE/FALSE) Aply a mask to the ROI extent. Mask will be a polygon.
### Resulting in a 1/NA rasterLayer.
## dem: rasterStack with DEM, slope and aspect layers.

f.idrisidata <- function(write = F, demcorr = 'none', mask = T,
                         dem = dem.ae, wrformat = 'RST') {
  i.allfiles <- list.files(file.path(dir.work, dir.fun), all.files = F)
  # List of TIF files at dir.fun folder
  i.listtif <- grep(".tif$", i.allfiles, ignore.case = TRUE, value = TRUE) 
  bands <- as.numeric(substr(i.listtif, (nchar(i.listtif) - 4),
                             (nchar(i.listtif) - 4)))
  i.stk.toar <- stack()
  #i.stk.toart <- stack() # topocorr
  i.lstk <- list()
  # SUN Parameters ---
  ## Sun elev in radians
  sun.e <- as.numeric(mtl[grep("SUN_ELEVATION", mtl$GROUP), 2]) * (pi/180) 
  ## Sun Zenit in radians
  sun.z <- (90 - as.numeric(mtl[grep("SUN_ELEVATION", mtl$GROUP), 2])) * (pi/180)
  ## Sun Azimuth
  sun.a <- as.numeric(mtl[grep("SUN_AZIMUTH", mtl$GROUP), 2])* (pi/180)
  # DEM Parameters for Topo Correction ---
  #  if(topocor != 'none'){
  il.epsilon <- 1e-06
  # DEM slope and Aspect
  slope <- dem[['slope']]
  aspect <- dem[['aspect']]
  il.ae <- cos(slope) * cos(sun.z) + sin(slope) *
    sin(sun.z) * cos(sun.a - aspect)
  # stopifnot(min(getValues(il.ae), na.rm = T) >= 0)
  il.ae[il.ae <= 0] <- il.epsilon
  #  }
  for (i in 1:length(bands)) {
    message(bands[i])
    # Name
    i.fname <- paste0('b',bands[i],'_ae')
    # Read Geotif raster
    i.tmp <- raster(file.path(dir.work, dir.fun, i.listtif[i]),
                    package = "raster", varname = fname, dataType = 'FLT4S')
    # Crop and apply mask
    i.crop <- crop(i.tmp, extent(mask.ae))
    # uncorrected TOA Reflectance with Topographic correction with mask overlay
    i.toar <- f.TopoCor(x = i.crop, i = bands[i], method = demcorr,
                        slope, aspect, il.ae, sun.e, sun.z, sun.a)
    if(mask == T) {
      i.toar <- i.toar * mask.ae
    } else i.toar <- i.toar
    i.toar@data@names <- i.fname  # Add band name  
    # Create Stack
    if(i < 8) {
      i.stk.toar <- addLayer(i.stk.toar, i.toar)
      #i.stk.toart <- addLayer(i.stk.toart, i.toartmsk)
    }
    # Write IDRISI raster group rgf for uncorrected TOA Reflectance
    if(write == T) {
      dire <- file.path(dir.work, dir.fun, dir.tif)
      stopifnot(file_test("-d", dire))
      ## gdal
      ##writeGDAL(as(i.l8, "SpatialGridDataFrame"),
      ##fname = "D:\\idri.rst", drivername = "RST") 
      message(wrformat, 'raster will be created for ', i.fname, ' at: ',
              file.path(dir.work, dir.fun, dir.tif))
      writeRaster(i.toar, filename = file.path(dir.work, dir.fun, dir.tif,
                                               i.fname),
                  datatype = 'FLT4S', format = wrformat, #'RST',
                  overwrite = TRUE)
      fileConn <- file(file.path(dir.work, dir.fun, dir.tif, "ae_toar.rgf"))
      writeLines(c(length(i.listtif),
                   paste0('b', bands, '_ae')),
                 fileConn)
      close(fileConn)
    }
  }
  i.stk.toar
}

# RUN function to get rasterStack with processed bands -----------------------------
l8files <- f.idrisidata(write = T, wrformat = 'ENVI', demcorr = 'minnaert', mask = T)

# Export rasterStack to a BSQ TIF File
writeRaster(l8files, filename=file.path(dir.work, dir.landsat, dir.tif,
                                        "stack201400606.tif"),
            options="INTERLEAVE=BAND", overwrite=TRUE)
# Plot it
plotRGB(l8files, 6, 4, 2, stretch = 'hist')
ddate <-
function(year, month, day)
{
    # convert year, month, day to decimal date
    if(length(year) > 1) {
	month <- year[2]
	day <- year[3]
	year <- year[1]
    }
    year + julian(as.Date(paste(year, month, day, sep="-")), origin=as.Date(paste(year-1, "12", "31", sep="-")))[[1]]/365
    
}

‹      Õ	œd×uÖëŞ’ã%qâ€°Ì2qê½ªêîÊàNÛ‘å(İi+;ÏvkFš‘¢DÒI!6k³ï`ö°‹}‡°ïPì;„}‡aß!ì[ˆ8İõN»ßío¨søéè}·ó»¹VÍ”êèû÷yïİïœ{ë±‡>Ò¾í#o›L&y2Mòÿ§ò?ÈòÿÒäÉ[eşÌÛwâÎ'^¹óÂ+/OÒô;üÀ“OŞı„¼ï³Oÿ÷úé“ÉéÏúølšôÿ|ĞÿsñŞéígŸ—éígŸ~>å-òW^¸õü—åÛşï¼ãlŞüyú„şO2}şÙÎÿç-	qò­åÙ‹/İıšÅË¯¼ôìÏ€ÿ²·œşñ­—y¹ÿëë_ôîÓ?xÏsw_xæ¹[¯<øîÛ·^ùºçßÓ}àÆŞâÁw¿p÷‰Ûw~ùÁwßyî¹_ÖW_¹ûõÏ¼¼·xÏìäÿŠyÓSÏİzY>#_üŒé?v£ø{o~ñÖS_{ë™;E4ùå7ÿi—şóŞòâİç>ùÌİzé¦_şüyıòßyÓs·|ñù§ˆóÁ=Ú+é·^ºs«ÿ»é`…ÿÎWß}îô?ãmg¯]şó7ŸyèÙ—ú_´ƒ¿òOİ½ûÒí—ûß»·MŠŸƒwçK¯¿«˜/ııÓO›¾6ùô/ç›ú¹WùÍ¯ÄŞÜ+¹ùã·¼fÆôÖŸ»ûÊ_º}ü÷~ækFÙó#˜šÍ_ù,ıW4.ı\øUØú_pşKò¶Ó¿Uüâ€ ¯^üäûEôöÓ×‹ß‰ÏVDùœ×ÜØóµb.~Ş ìï ÿAÛûÜòßšZ"¶ÍÉ…O­²\sÁš‡í@°ËlçDlw.†:[e¹*æ‚5Û`—Ù.ˆØ^¿êl•å~1¬yØ»ÌvIÄöààÂ'ÀVY¾÷d8¬yØ»Ìv‡ˆíûÖ>y¶Êò¡Ép.Xó°v™í.Û÷\øäØ*Ë‡O†sÁš‡í@°Ël÷ˆØ~`}á“G`«,™ç‚5Û`—Ù®ˆØ~éÁ…O­²<<Îk¶Á.±ÍÍŒîÑúÂ' Waª§z\¼ŞÏ<p‚¸DÎÔzäÌ]÷ğÖ}†Ïú:Üõ¶Ìm	àöéú‹ŸüúÁ-aŞÏM^°õg}éÏÇ†p¼©>Öƒ‘áêÏı2ûÓ?$p¶Áe0§4Ö+?ù‡KT'2Âà2¸SëÕ‘áVS‚‚¸ö”ÆT2ÃU˜ôå (€ËàOi¬Aõ 3ÜjêAP0 —Á ÒX¯Waîs›î@0 —Á¡ÒXƒ*Bf¸
“¾"»·Á*	™áVS‚‚¸•ÆT2Ã­¦&p["¸AE!3ÜjŠBP0 —É¡
ò–Ípfï!óV… ` .“Cu´®Â<ç6Ü` .ƒCÕ-a…ƒfaJèëõ ` .ƒCÕÇV80ÂÕşÂÀep¨4Ö+?ù‡[Oá 
à28TëÕ‘áVS8€‚¸•ÆU8°ÂÓ¦/@Á.ÃÏˆàF¬p«)@Á \‡Jc½>2\…¹_Ìl¸Á Ü–nTáÀ
WaÒ ` .ƒC¥±F¬p«)@Á \‡Jc*XáVS8€‚¸DUXáÀ
·šÂÀer¨¢
V¸
óğd8°yàn)Ì™ª£õ¸pæñd8°yàp‰ªõ•‹Ÿ<Bá ( œÏÅŸÓÀ
à9Të«#Ã½†a¿~î@°Ëp3"¸#Ûë†yş:›CµŞb?.ˆªõÈöãz…a¿ÎæPp["¸×G†»a¿¾Ïw €ËàPõW—°NŒæ}ª>õtb@Á \‡ª5¬ÃWø;1 ` .ƒC¥±F-…ŒpëéÄ€‚¸•Æµ²Â­¦
à28TkÔRÈ
×øç<p·-…*5j)d…WM'Àep¨4Ö¨¥®ÂÜ/æ6Ü-K¡åŒnT'†®Â¤ïÄ€‚¸•ÆÕ‰a…[M'Àm‰àFubXáVÓ‰p‰ª°N+Üj:1 ` .“CÕ‰a…«0O†s›î–NŒ%“Cu´®Â<ç6Ü` .‘CÖ‰a„[O'À%r¨Â:1¬pKÈ´P0 —È¡
ëÄ°Â­¦
à9TaV¸+“¯
vîÎŒîÈöc=P0 —È¡Zl?®{ª„yş:›C5Àm	àöW—°6›æª˜õò[M›Àep¨úXÃÚlŒpõ‡¿Í
à28TkÔ:×·6(€ËàPi¬Që\+ÜjÚl ` .ƒC¥±F­s­p·<pñ´Ù@Á \‡JcZçZáVÓfp*5jk…§ÿ¼_ÌÅßç»mËàPi¬Që\+\…Ißf»wwF7ªÍÆ
·š6(€ËàPi¬Qm6V¸Õ´Ù@Á Ü–nT›n5m6P0 —É¡Šj³±ÂU˜‡'Ã¹€ÍwK›Í.“Cu´®Â<ç6Ü` .‘CÖfc-hçEQ(àk³‚¸DUX›î5“¯Í
à9Tam6V¸%äkÅëlÕ¶6›]"‡*¬ÍÆ
w…aòµÙ@Á \"‡*¬ÍÆ
wÃäk³‚]†»7#‚;²ıXO›Àep¨ú_À°6›æ~1k†ã‡?|m6P0 ·åÖfc„«?üm6P0 —Á¡ÒX£Ö¹F¸õ´Ù@Á \‡JcZçZá*ÌkÅ\Àæ»e»ÇàPi¬Që\+\…Ißfp*5jk…[M›Àep¨4Ö¨u®®ñÏyàn[ç28TkÔ:×
Oÿ™¾Í
à®ˆàFµÙXáVÓf»w5#‚Õfc…[M›À%r¨ÂÚl¬p«i³‚¸-Ü¨6+\…yx2œØ<p·´Ù¬˜ª£õ¸pæñd8°yàp‰ª°6#ÜzÚl ` .‘CÖfc…{Ãäk³‚¸DUX›n5m6P0 —È¡
k³±Â-!Ó¶Ù@Á \"‡j}}d¸û&_›À]ÁÙ~¬§Í
v	î´™ÍèöA†õÙ”4ïc×ÓgCt<ª>Ø°F#]ıáo´‚!º-İ¨¥®‘n=6P0D—Á¥Ò`£ÖºVºÕ´Ú@Á]›JƒZìZé*Mú^(¢ËàSi°Q«]+İjšm `ˆ.ƒQ¥ÁF-w­t·¬˜xºm `ˆ.ƒS¥ÁF­w­tÎCwëz—ÁªÒ`£úm¬ôªé·‚!º^•Õpc¥[MÃĞmfDt£:n¬t«é¸‚!ºL^UTË•®Ò<<Îmºÿï–¡ÛÑ=ZKWiO†sA›‡î@0D—È«
kº1Ò­§é
†èyUa]7Vº×0M¾®(¢KäU…µİXéVÓvCt‰¼ª°¾+İ¦É×wCt‰¼ª°Æ+İ’òªx}Ÿî6'²!òªÂ:o¬t«é¼‚!º^Uo¨…uŞ”4ïã4ÖÓytÛİ°Î#]ıáï¼‚!º^•µŞ5Ò­§ó
†è¶Dt£Ö»VºÕtŞ@Á]¯JƒZïZé*MúÎ(¢ËàUi°Që]+İj:o `ˆ.ƒW¥ÁF­w­t•f¹‚*hóĞİ¶Şm¼*6j½k¥»e=ÌÓyCt¼*¶ŸG£[MçÑeğª4Ø¨Î+½j:o `ˆ.‘WÖyc¥[MçĞÏˆèFuŞXé*ÍÃ“á\Ğæ¡»­ófÎäU­Ç¥«4'Ã¹ ÍCw ¢ÛòĞë¼±Vªé¼‚!ºD^UXç•î5L“¯ó
†èyUa7VºÕtŞ@Á]"¯*¬óÆJw…iòuŞ@Á]"¯j=òŠhİ?¯û•Ïù¬¯³=3Ct¼ª~QÖ›QÒ¼[QOoÑeğªú`Ãz3Œtõ‡¿7
†è2xUlÔŠÈH·Ş( »˜ÑZYéVÓ›Ct¼*6jEd¥«4é{3 `ˆnKD7jEd¥[MoÑeğª4Øë#ÓUšûÅ\Ğæ¡;Ñeğª4Ø¨Ş+]¥Iß›Ct¼*6ª7ÃJ·šŞ(¢ËàUi°ı<]ãŸóĞ†èyUa½VzúÏ½ãÈÛ›Ct™¼ª¨Ş+]¥yx2œÚ<t·õf,˜¼ª£õ¸t•æñd8´yèt—3ºaÕ{#İzª÷P0D—È«
«Ş[é.0M¾ê=Ñm‰èìUÕS½‡‚!ºD^UØ¹Vºû˜&ß¹P0D—Á«êìÃê»%Íû¬xô‡¿¾Ct¼*öÊÅO~ãéÖSß…‚!º^•õÌl¥«4¯sA›‡îÖgf¯Jƒzf¶ÒUš‹b.hóĞİúÌÌàUi°QÏÌVºJsUÌmº[Ÿ™¼*6ê™ÙJWiîsA›‡î¶gæİ¨ú®•®Ò¤¯ïBÁ]¯JƒªïZéVSß…‚!º-İA¹rºÛª]Œƒî@0D—Á«Ò`åÊèÿœ‡î@0D—É«ŠªïZéé?çâïóĞİVßİaòªÖãÒUšÇ“á\Ğæ¡;Ñ%òªÂöŞéÖ³÷
†èyUaÕ{+İk˜&_õ
†èyUaÕ{+]­ç4ùª÷P0D—È«
«Ş[é®0M¾ê=ĞİÑ½~1Øèîcš|Õ{(¢ËàUõöëA1úu¤[Ò¼ÏŠ§½÷P0D·å¡V½7Ò­§zCt¼*6ê™ÙJ·šê=Ñeğª4Ø¨gf+]¥I_½‡‚!º^•õÌl¥[Mõ
†è2xUlÔ3³•®ÒÜ/æ‚6İ­ÏÌ^•U½·ÒUšôÕ{(¢ËàUi°QÕ{+İjª÷P0DwED7ªzo¥[Mõ
èîÍˆèFUï­t«©ŞCÁ]"¯*¬zo¥küsºÛª÷{-İ£õ¸tõŸ'Ã¹øû<t‚!ºD^UXõŞH·ê=Ñ%òªÂª÷Vº×0M¾ê=Ñ%òªÂª÷VºÕTï¡`ˆ.‘Wµù™y­³Ö{u.şœ‡îÖgf¯ª8«ï–4ïóÔTO}
†è2xU}°a5"#İzjDP0DwED7ªFd¥[M
è®fDt¯_vºJs¿˜Ú<t‚!º^•U#²ÒUšô5"(¢ÛÑªYéVS#‚‚!º^•U#²Ò­¦FCt¼*6j½k¥[M
†è2xUlTÈJw‹ÛÁS#‚‚!º^•ÛÏ£Ñ5ş9İ`ˆ.“Wuåâ'àUUS#‚‚!ºD^UXÈJ÷¦ÉW#‚‚!º+"º#;‘õÔˆ `—é¶³İ‘ÈzvxBÁ]¯êƒ•¯Ñ-i>z0œõõşsøH(¢ÛÑr"­t•&½	Ct¼*6Ê‰´Ò­Æ‰„‚!º^•åDZéVãDBÁ]¯Jƒr"­t«q"¡`ˆ.ƒW¥ÁF9‘VºJ³|Æ.hóĞİâD¶3¯JƒíôG£»eÅÄãDBÁ]¯ª6ÌÍ0Ò­ÇÍ€‚!º^Õ£÷6*G¹%Í/?Îúz5}UP0@·™Ñr3¬t«q3 `ˆ.ƒW¥ÁF¹VºJ“ŞÍ€‚!º-İ(7ÃJ·7
†è2xUl”›a¥[›Ct¼*6ÊÍ°Ò­ÆÍ€‚!º^•åfXéVãf@Á]¯Jƒ,ÎG [›Ct¼ª>Ø°Fºõìğ„‚!º^•;ò•y}ˆi¿Î–»ë­Wf¯ê±7ÁFíß-i>¶Îıëõìß…‚ºíŒ‡îÁÈtõ‡ÿ»³¡`ˆ.ƒW¥Á^¹øÉo<İzNß†‚!º-İ«#ÓUš×Š¹ ÍCw ¢ËàUi°Q@+]¥I²Ñeğª4Ø¨
 •n5@(¢ËàUi°×G¦«4÷‹¹ ÍCw ¢ËàUi°Q@+]¥I_„‚!º^•U´Ò­¦Ct¼*6ªh¥[M
†èyUa@+İj*€P0@w>#¢å3[é*ÍŞOæ­ BÁ]&¯êh=.]¥y<ÎmºÁİ–‡nØÉ
Ö*B5'+@Á]"¯*¬¾k¥Û_yùë»P0D—È«
«ïZébš|õ](¢ËàUİ¸¹Q9êÊ\ÒüĞd8ëëÕÔˆ `ˆ.ƒW¥Á^™n55"(¢ËàUi°Q5"+]¥I_#‚‚!º^•U#²Ò­¦FCt¼*öúÈt•æ~1´yèt3"ºQ5"+]¥I_#‚‚!º^•U#²Ò­¦FCt["ºQ5"+İjjDP0D—Á«Ò`£¼*+İjjDP0D—Á«Ò`£¼*+]¥Ù{R¼5"(¢ËäU­Ç¥«4'Ã¹ ÍCw ¢KäU­G~ªZ÷÷Õuÿôt>ëël÷İ¡`ˆ.ƒWõ¡W7*lÓ×‘nIóÃÃY_¯Æg†‚!º^•å3[éVã3CÁ]¯Jƒò™­t•&½Ït—3"ºQ>³•n5>3Ñeğª4Ø(ŸÙJ·Ÿ
†è¶Dt£|f+]¥Iï3CÁ]¯JƒZYéVã3CÁ]¯Jƒò™­t«ñ™¡`ˆ.ƒW¥ÁFùÌVºÕøÌP0D—Á«Ò`£|f+İj|f(¢ËäU­Ç¥[ÏCt¼ªßÛåf”4¿âd8ëëÕ¸P0D—Á«Ò`£Ü+İjÜ( »3#¢åfXéVãf@Á]¯Jƒr3¬t«q3 `ˆnKD7ÊÍ°Ò­ÆÍ€‚!º^•åfXéVãf@Á]¯Jƒr3¬t«q3 `ˆ.ƒW¥ÁF¹VºÕ¸P0D—Á«Ò`‹óèVãf@Á]¯ª{pì•‹Ÿü:Ò-ivëá¬¯¿³§ù®b~çpŸ.Ñeğª4Ø«#ÓUš…7Å×WCt¼*6Ê‰´Ò­Æ‰„‚º»3"ºQN¤•n5N$Ñeğª4Ø('ÒJ·'
†è¶Dt£œH+İjœH(¢ËàUi°QN¤•n5N$Ñeğª4Ø('ÒJ·'
†è2xUl”i¥[	Ct¼*6Ê‰´Ò­Æ‰„‚!ºL^ÕÑz\ºÕ8‘P0D—Á«úÈÍ³Ï[?pñ“_Gº%ÍN†sÿz=ßCt¼ª>Øƒ‘éêÿ7ÅAÁ İ½İ+?ù§[O
†è2xUìÕ‘éVSE€‚!º-İ¨*‚•®Ò¤¯"@Á]¯Jƒª"XéVSE€‚!º^•{}dºJs¿˜Ú<t‚!º^•UE°ÒUšôU(¢ËàUi°QU+İjªP0D—Á«Ò`£ªVºÕT `ˆ.‘WVE°Ò­¦Š Ct™¼ª¨*‚•®Ò<<ÎmºÛª«İ£õ…O®Ò<ç‚6İ`ˆ.‘WµX/#Tz·¢üf¸ó×ÙÜŒ¡`ˆnK@÷£¯n‚ª"”4?v0œû×ë©AÁ]¯ª6¬Fd¤«?ü5"(¢ËàUi°W.~òO·Ñeğª4Ø¨û®•n55"(¢ËàUi°Q5"+]¥I_#‚‚!º^•U#²Ò­¦FCt¼*öúÈt•æ~1´yèCtWDt£jDVºJ“¾F»Lw>›ÑªYéVS#‚‚!º^•U#²Ò­¦FCt["ºQ5"+İjjDP0D—É«ŠªYé*ÍÃ“á\Ğæ¡»¥F4Ÿ1yUGëqé*ÍãÉp.hóĞ†èyUë‘ÈóšPQ5(kF4t‡‚!º^ÕÇîm‚ª"”4¿òd8÷¯×S#‚‚!º^UlXÈHWøkDP0D—Á«Ò`£®ÌFºõÔˆ `ˆ.ƒW¥Á^™n55"( ÛÌˆèFÕˆ¬t•&}
†è2xUlTÈJ·šÑm‰è^™®ÒÜ/æ‚6İ`ˆ.ƒW¥ÁFÕˆ¬t•&}
†è2xUlTÈJ·šÑeğª4Ø¨‘•n55"(¢KäU…Õˆ¬t«©AÁ]&¯*ªFd¥«4O†sA›‡î¶QÃäU­Ç¥«4'Ã¹ ÍCw ¢»â¡V#²Vª©AÁ İvFD÷êÅOî5L“o—ÑeğªplT¨¤ùøz8÷¯×S„‚!º-İ°
 ‘®şğW ¡`ˆ.ƒW¥ÁFİwtë© BÁ]¯JƒºïZéVS„‚!º^•U´ÒUšô@(¢ËàUi°Q@+İj*€P0D—Á«Ò`¯LWiîsA›‡î@0D—Á«Ò`£*€VºJ“¾CtWDt£*€VºÕT ¡`€î|FD7ªh¥[M
†èyUa@+İj*€P0D·%¢U´ÒUš‡'Ã¹ ÍCw[pÎäU­Ç¥«4'Ã¹ ÍCw ¢KäU…U të© BÁ]"¯j=²›±î?g½(æÕpæ¡»ÍÍ˜yUë‘İŒõ>¦yş:Ûzw(¢KäU­Gv3Öız¶¤yş:Ûzw(¢ËàU}ÕÍM°Qõİ’æÇ'Ã¹½ê=Ñ]ñĞ«Şéêõ
è.fDt£™të©ŞCÁ]¯Jƒ½:2İjª÷P0D·%¢U½·ÒUšôÕ{(¢ËàUi°Që]+İjª÷P0D—Á«Ò`£Ö»VºJs¿˜Ú<t·­w^•µŞµÒUšôÕ{(¢ËàUi°QÕ{+İjª÷P0D—Á«Ò`£ª÷VºÕTï¡`ˆ.‘WV½·Ò­¦zCt™¼ª¨ê½•®Ò<<ÎmºÛª÷Ëİ£õ…O®Ò<ç‚6İ`ˆ.‘WV½7Ò­§zCt["º#;‘õìß…‚!ºD^Õzd'ò¼£ YöjğĞİæD.‰¼ª°Î+İ¦É×yCt‰¼ª°Î+İ}L“¯ó
†èyUa7VºÕtŞ@Á]"¯j=²¹~Ó<Í«
†è2xUulTçMIó‰ƒáÜ¿^O_Ñeğªú`ÃúªŒtõ‡¿¯

èîÌˆèF¹FºõôUAÁ]¯Jƒr3¬t«é«‚‚!º-İ(7ÃJWiÒ÷UAÁ]¯Jƒr3¬t«é«‚‚!º^•åfXé*Íıb.hóĞİæfì0xUl”›a¥«4éûª `ˆ.ƒW¥ÁF¹VºÕôUAÁ]¯Jƒê«²Ò­¦¯

†èyUa}UVºÕôUAÁ]&¯*ª¯ÊJWiç‚6İm}U»3"ºGëŸ<]¥y<ÎmºÁ]"¯*¬¯ÊH·¾*(¢ÛÑÙ‰¬§¯

†èyUa}UVºÕôUAÁ]"¯*¬¯ÊJw…iòõUAÁ]"¯*¬¯ÊJwÓäë«‚‚!ºD^UX_••n5}UP0D—Á«zâŞ&Ø¨ŞŒ’æÍbî_¯§ó
†è2xU}°a7FºúÃßyCt¼*6j½k¤[OçĞİ›ÑZïZéVÓyCt¼*6jEd¥[MoÑm‰èF­ˆ¬t•æ~1´yèn[í1xUlÔŠÈJWiÒ÷f@Á]¯JƒêÍ°Ò­¦7
†è2xUlTo†•n5½P0D—Á«Ò`£z3¬t«éÍ€‚!ºD^UXo†•®Ò<<ÎmºÛz3ö˜¼ª£õ¸t•æñd8´yèCt‰¼ª°Ş#İzz3 `€îjFDwd¯ªŞ(¢KäU…õfXéVÓ›Ct["º#;‘õôf@Á]"¯*¬7ÃJwÓäëÍ€‚!ºD^UXo†•n5½P0D—Á«ºù©M°QÕû’æÍ{ÅÜÓ­¦7
†è2xU}°a½FºúÃß›Ct¼*6j½k¤[OoÑeğª4Ø¨gf+İjª÷P0D—Á«Ò`£™­t•æ~1´yènyf^ÌfDt£™­t•&}õ
†è2xUì =İjª÷P0D·%¢U½·Ò­¦zCt¼*6ªzo¥[Mõ
†è2xUlTõŞJWiç‚6İ-ÕûÅŒÈ«
«Ş[é*ÍãÉp.hóĞ†èyUaÕ{«YMõ
†èyUaÕ{+İk˜&_õ
†èyUaÕ{+İjª÷P0D—È«
«Ş[é®0M¾ê=ĞmfDtGv"ë©ŞCÁ]"¯*¬zo¥[Mõ
†è¶toÍ6Á†Õwš·N†sÿz=Õ{(¢ËàUõÁÆÕwï(Şn=õ](¢ËàUi°a+"#İjöŞCÁ]¯Jƒ{f6Ò­¦zCt¼*6ì©ÊH·Šâ¯ïBÁ]¯Jƒ«ïéVSß…‚!º^•Vß5Ò­¦¾Ct¼*6¬¾k¤[M}
è¶3"ºaõİ{CŠ÷£«4O†sA›‡î¶únËàUi°Gëqé*ÍãÉp.hóĞ†è¶<tãê»6ºõÔw¡`ˆ.‘WWß5Ò½†iòÕw¡`ˆ.‘WWß5Ò­¦¾Ct‰¼ª¸ú®‘î
Óä«ïBÁ]"¯*®¾k¤»iòÕw¡`ˆ.‘WWß5Ò­¦¾Ct‰¼ªõÈNäú!Lóüu6¯j(¢KáU}Ó&Ø¨ê}IóÉ‡sÿzEÕ{$ ;ŸñĞ«ŞéVT½G‚!º^UlÔz×J·ê=Ñm‰èF­ˆ¬të©Ş#Á]
¯ª6jEd¥«4÷‹¹ ÍCwÛŠhNáUõÁF­ˆ¬t•&oÑ¥ğªú`£VDVºõôf Á]
¯ª6ª7ÃJ·Ş$¢KáUõÁFõfXéÖÓ›Ct)¼ª>Ø¨Ş+]¥yx2œÚ<t·õfÌ‰¼ª°Ş+]¥y<ÎmºÁ İÅŒ‡nXo†Õ‰¬§7	†èyUa½Vº×0MÂŞ$¢ÛÑêÍ°Ò­§7	†èyUa½Vº+L“°7	†èyUa½Vºû˜&aoÑ%òªÂz3¬tëéÍ@‚!ºD^UXo†•n=½H0D—È«ZìD®Æ4Ï_góªÖÛœÈƒWõä‹›`£:oJšO®‹yóz=7P0D—Á«êƒë¼1Ò­§ó
è.gDt£Ü+İj:o `ˆ.ƒW¥ÁF¹VºJ³è´)ióĞİæf,["ºQn†•n5}UP0D—Á«Ò`£™­t«©ŞCÁ]¯JƒªŞ[éVS½‡‚!º^•{42İÃâñd8ëë‡lt‚!º^•.ßF^ÕSß…‚!º^•;òŠ¨ú.ÑeòªF^ÕSß…‚!ºD^UX}×Jw…iòÕw¡`€îÎŒˆîõ‹Á@wÓä«ïBÁ]"¯*¬¾k¥[M}
†è¶Dt£ê»VºÕÔw¡`ˆ.‘WVßµÒ­¦¾Ct¼ª§Ş±	6¬¾[Ğ|êæpî_¯§¾Ct¼ª>Øƒ‘éêÏåznùCB÷`+]¯JƒóªÖŠ÷£[Oõ
†è2xUlØz×H·š
 Ñeğª4Ø°‘‘n55"(¢ËàUi°Q;<­t•æáÉp.hóĞİ¶ÃswFD÷h}á“G «4'Ã¹ ÍCw ¢ËàUéşÈOUõT ¡`ˆnKD÷êÈt¯aš|@(¢ËàUi°a@#İj*€P0D—É«yETO
†èyUq@#İ}L“¯Ct‰¼ª¸
 ‘n5@(¢KäUÅU t«© BÁ]
¯ê7ÁFUJš·'Ã¹½¢Ñ¥ğª6Á†UŒt+ª" Á İ½İ('ÒJ·'	†èRxU}°GëqéÖãD"Áİ–‡n˜i½ïÖãD"Á]
¯ª6Ê‰´Ò­Ç‰D‚!º^Ul”i¥[‰Ct)¼ª>Ø('ÒJ·'	†èRxU}°Q^••n=^Ñ¥ğªú`G^­ûgâu¿ò9Ÿõu¶gæõÖ“W5òŠh}Œi¿~|1º[WD^ÕíG7*G­ˆJš·_-æşõj¼*( »šÑZYé*ÍkÅ\Ğæ¡»mE´bğª4Ø¨û®•n5N$Ñm‰èFİw­t«q"¡`ˆ.ƒWÕæDéÖãDBÁ]¯Jƒù¾[	Ct¼*6Ê‰´Ò­Æ‰„‚!º^•åDZéVãDBÁ]¯êö7o‚ê¼)iŞ9ÎıëõtŞ@Á]¯ªö`dºúÃ¿;
†èRxU}°aÏÌ¯(ŞnE^ì2İålFD7ì™ùÕ!ÅûÑ­Ç«B‚!º^UlØ3³‘®Ò\sA›‡î–gæå¬%¢æDéÖãD"Á]
¯ªöh=.İzœH$¢KáUm‚s"mt+r"‘`ˆ.…WÕ;òSUEN$Ñ¥ğªú`G~ªªÈ‰D‚!ºL^U˜i¤[‰Ct‰¼ª°İÙVºû˜&ßîl(¢KäU…ñj¥ûğF,ş3^¡`€n3#¢ûõ…Oî#˜fÙËCw ¢KäU…õ3[ébš|ıÌP0D·% {çS›`£jD%Í;÷Šyóz=@(¢ËàUõÁ†U tõ‡¿Ct¼*6Ê«2Ò­§Ct¼*6Ê«²Ò­¦Ct¼*6ê©ÊJ·Šâ¯AÁ]¯JFÎİz|f(¢ËàUi°Q>³•n5>3Ñeğª4Ø(ŸÙJWGzŸ
è¶3"º>yºÕì½‡‚!º^•û¾õ¸tÕW¦?'
†è¶Dt£ªVºÕT `ˆ.‘WVE°ÒU_™¾Š Ct¼ª§g›`ÃœÈ‚æÓ'Ã¹½Ÿ
†è2xU}°qnÆ½ÅûÑ­ÇÍ€‚!º^•ş*lÓr·šH(¢ËàUi°a^•‘n5^Ñeğª4Ø0¯ÊH·¯

†è2xUlØz×H·šõ.ĞÏˆè†­wï(Ş—n5ë](¢ËàUi°aë]#İjÖ»P0D·Ÿ®.Û¢:o.¯W‡?åëô7X0D—À«:6hEd¥[MçÑ%ğªÎƒZ™éÖÒyƒCt	¼ªó`ƒVDfºJsQÌmºÛVDs¯ê<Ø ê½™®Ò\sA›‡î¶êıœÀ«:öúÅ`G «4÷‹¹ ÍCw ¢KàUÔ›a¦«4{ã|.hóĞİÖ›1'ğªÎƒòªÌt•fïbœÏmºÛ¼ªÅŒˆnWe¦«4{ã|.hóĞİæU-¼ªó`ƒ¼*3]¥Ù{RçsA›‡î6¯jÑÑªŞ›é*ÍÃ“á\Ğæ¡»­z¿`òªÖ>yºJóx2œÚ<t‚!º^Uo¨…ùÌe5¾ ıé‹>zŸ
†è2xUì•‹ŸüÆÓ­Çg†‚!º^•å3[éVã3CÁ]¯Jƒò™­t•&½ÏCt¼*6Êg¶Ò­Æg†‚!º^•;°MG «4÷‹¹ ÍCw  »œÑò™­t•&½ÏCt¼*6Êg¶Ò­Æg†‚!º-İ(ŸÙJ·Ÿ
†è2xUl”Ïl¥[ÏCt™¼ª(ŸÙJ·Ÿ
†è2yUGëqéVã3CÁ]"¯*j‘•n5ûˆ°`ˆ.‘WµÓÄL·¿¯Òï4Á‚!º^Uo—†UÊ\,i÷¯×SE€‚!º^•{udºÕT `€îÎŒˆnÔ}×JWgú*Ñeğª4Ø¨*‚•n5U(¢ÛÑ½>2]¥¹_ÌmºÁ]¯Jƒr3¬t«q3 `ˆ.ƒW¥Áç#Ğ­ÆÍ€‚!º^UlÔ‰FVºÕœh„Ct¼*6ªh¥«îû©(X0D—Á«Ò`£¼*+İj¼*(¢ËàUõË¶07£ğ‰ïç#×ãf@Á]¯Jƒr3¬t«q3 `€îîŒˆn”›a¥[›Ct¼*6ÊÍ°Ò­ÆÍ€‚!º-İ¨H+]¥Iß	Ct¼ªşöTÕ¿ÿ|.iknVóTCt¼*6ê©ÊJ·š§*(¢ËàU]¦Û_Q·}óy=t¡`ˆ.ƒW¥ÁFyUVºÕt«CÁ]¯ª6ê»Ä¬t·]¹×ƒ8ènı.±]¯ªô[?pñ“_Gº¥o\ÒÖ×Ïû¦†?tßi‚Ct¼ª>Ø°+³‘n=Wf( »7ã¡V´æn5@(¢ËàUi°Gëqé÷ÿ|x2œõõc6ºÁİ–€n	»ï–½ûÕo«¹ïBÁ]¯Jƒº2[éVse†‚!º^•åDZé^Ã4é¾m
†è2xUl”We¥»¥ÊÀ³K
†è2xUlTÇ«•®>E=2ÎÅSİm¯{^UßöTUôEİ·oªš§*(¢ËàUõÁ†õ3éÖÓÏCt¼*ıUŒzf¶æn5ÏÌP0@w5#¢õÌl¥[Í33Ñeğª4Ø¨gf+İj™¡`ˆnKD7ªh¥»Â4ù*€P0D—Á«úÀzlÔ3s¹Â)iëëÕ<3CÁ]¯ªö`dºúÃ¶:Ñeğª4Ø¨‘‘n=+"(¢ËàUé…&jEd½2W³"‚‚!º^•µ"²Ò­fECt¼*6jEd¥[ÍŠ
†è2xUlÔŠÈJ·šì2İÙŒˆîõ‹Á@wÓ,;uxèCt¼ª¾¶ŞíŸu·VøªYïBÁİ–‡nØz×HWø×»P0D—Á«Ò`£VDFºõœ
Ct¼*¶¿ŒFWë¼ıë¼'çCÁ]¯Jo##çn=nÑeğª4ØÁâ|ºÕ¸P0D—Á«Ò`£Ü+İjÜ(¢ËàUi°Qn†•®®oéİ(¢»"¢;Xœ@·7
è63"º>yºÛv‘Ñìğ„‚!º^ÕÑzl”›qÜSÒ¹¤­¯WãUAÁİ–‡n˜We¤«?ü^Ñeğª4Ø¨õ®‘n=^Ñeğª4Ø¨õ®•®Ò¼VÌmºÛÖ»ƒW¥Áöœ£Ñí;`ù¿¡
†è2xUl”Ïl¥[ÏCt¼*6j‡§•®Òì_çıN(¢ËàUi°ı<]¥©ÿ\¾ŞÏ<t‚!º+ºa5"ëz·šĞmgDtG~f®§FCt™¼ª¨‘•n55"(¢ÛÑªYé®0M¾Ñ%òªÂjDVºû˜&_
†èyUa5"+İjjDP0D—È«
û¦8+İ‡0M¾oŠƒ‚!ºD^UØyUVºúÏ½kq©şË¶Şİv^ÕNËàU}pdX}÷Ñşß¯sI[_¯¦¾Ct¼ª>Ø°ú®‘®şğ×w¡`ˆ.ƒW¥Á^¹øÉo<İzê»P0@w>#¢åUYéVSß…‚!º^•åUYé*ÍE1´yènóªæ-İ¨õ®•n¿¾åÿ@(¢ËàUi°Q½VºÕôf@Á]¯JƒêÍ°Ò­¦7
†è2xUl”›a¥«4{×‚·7
†è2xUìÑz\ºJ³·7
†è2yU#¯wëéÍ€‚!ºD^UXo†•î5L“¯7
†èyUa½VºÕôf@Á İÅŒˆnTo†•î
ÓäëÍ€‚!ºD^UXo†•î>¦É×›Ct["º#{Uõôf@Á]"¯*¬7ÃJ·šŞ(¢KäU­Gv"·~o›WµŞæD.ˆ¼ª°Î+İŞ­àï¼‚!º^Õ£÷6ÁFõf|yOGç’¶¾^MçÑeğªú`Ã:oŒtõ‡¿ó
†è2xUì•‹ŸüÆÓ­§ó
†è2xUl”i¥[MçĞ]ÎˆèF9‘VºJ“¾ó
†è2xUlÔŠÈJ·šŞ(¢ÛÑêÍ°Ò­¦7
†è2xUlÔz×JWiöëZŞŞ(¢ËàUi°Gëqé*ÍŞåàíÍ€‚!º^•.ÎG^ÕÓ›Ct™¼ª‘WDõôf@Á]"¯*¬7ÃJ·šŞ(¢KäU…õfXé®0M¾Ş(¢KäU…õfXéîcš|½P0@wgFDwĞj0İjz3 `ˆ.‘WÖ›a¥[MoÑm‰èìDÖÓ›Ct‰¼ªõÈNäÖopeóªÖÛœÈ"¯*¬óÆJ÷Óäë¼‚!ºL^ÕÑz\ºÇ˜fÙ‡ÅCw ¢ËàU=öà&Ø¨Î›Ç6"œÏ%m}½š¾*(¢ËàUõÁ†õUéê_Ñeğª4Ø+?ù§[O_Ñeğª4Ø«#Ó­¦¯

èîÎˆèFU¬t•&}_Ñeğª4Ø¨*‚•®Ò\sA›‡î¶*ÂnKD7Ê‰´ÒíÇ²K®<¿Š‡î6'r—Á«Ò`£œH+İjz"¡`ˆ.ƒW¥ÁF9‘VºÕôDBÁ]¯Jƒr"­t•fïQñöDBÁ]"¯*¬'ÒJWiö#oO$Ñ%òªÂz"­Nd5=‘P0D—È«
ë‰´Ò½†iòõDBÁ]"¯*¬'ÒJ·šH( »7#¢;²›QOO$Ñ%òªÂz"­t÷1M¾H(¢ÛÑ=8—n5=‘P0D—È«
ë‰´Ò­¦'
†èyUa=‘VºÕôDBÁ]"¯*¬'ÒJ÷L“¯'
†èyUa=‘Vº‡˜&_O$ÑeòªÖãÒ=Æ4ùz"¡`ˆ.ƒWuãæ&Ø¨®¹õtt.iëëÕôDBÁ]¯ª6¬'ÒHWø{"¡`€îjFD÷ÊÅO~ãéÖÓ	Ct¼*öêÈt«é‰„‚!º-İ¨*‚•®Ò¤ï‰„‚!º^•UE°Ò­¦'
†è2xUìõ‘é*Íıb.hóĞ†è2xUlTÁJWi®|ßĞ
Ct¼*6ªŠ`¥[M?3Ñeğª4Ø¨*‚•n5ıÌP0D—È«
ëg¶Ò­¦Ÿ
†è2yUQU+]¥ÙûË¼ıÌP°Ëtwg3"ºGëŸ<]¥y<ÎmºÁ]"¯*¬ŸÙZE¨¦Ÿ
†è¶DtGv"ëég†‚!ºD^UX?³•n5ıÌP0D—È«
ëxµÒíİ
şW(¢KäU…u¼ZéVÓñ
Ct‰¼ª°H+İjz"¡`ˆ.‘WÖi¥û¦É×	Ct‰¼ª°H+İCL“¯'
†è2xUzulT_Õ‡7"œÏ%m}½š®9( ÛÌxè†uÍéê×Ñeğª4Ø(¯ÊH·®9(¢ÛÑòª¬t•æµb.hóĞİæU5^•åUYé*Í¢Š¯k
†è2xUì 	lºJsUÌmºÁ]¯Jƒr"­t•æ~1´yèns"¯Jƒr"­t•&}×Ñeğª4Ø÷­Ç¥[M×Ñeğª4Ø(ŸÙJWiö$o×Ñ%òªÂºæ¬t•fï@òvÍAÁ İvFD7Êg¶ÒUš½ŸÌÛ5Ct™¼ª£õ¸t•æñd8´yèCt[ºa]sÖ*BïEñwÍAÁ]"¯*¬óÆJ·_ñğwŞ@Á]"¯*¬óÆJ·šÎ(¢ËàU}øŞ&Ø¨
àWôtt.iëëÕÔw¡`ˆ.ƒWÕVß5ÒÕşú.Ñeğª4Ø¨§*#İzê»P0D—Á«Ò`¯L·šú.Ñeğª4Ø¨ú®•®Ò¤¯ïBÁ İùŒˆnT}×J·šú.Ñeğª4Ø¨õ®•®ÒÜ/æ‚6İmëİyKD7j½k¥«4éë»P0D—Á«Ò`£ê»VºÕÔw¡`ˆ.ƒW¥ÁFÕw­t«©ïBÁ]"¯*¬¾k¥[M}
†è2yUQõ]+]¥yx2œÚ<t·ÕwçL^ÕÑz\ºJóx2œÚ<t‚!º^U÷àFå(Ÿ¹Ûˆp>—´õõjªP0D—Á«êƒ«"éê
è.fDt¶éO·Ÿ
†è2xUl”Ïl¥«4é}f(¢ÛÑò™­t«ñ™¡`ˆ.ƒW¥ÁFùÌVºÕøÌP0D—Á«Ò`£|f+]¥Iï3CÁ]¯Jƒò™­t«ñ™¡`ˆ.ƒW¥ÁFùÌVºÕøÌP0D—Á«Ò`£|f+İj|f(¢KäU…ùÌVºÕøÌP0D—É«:ZKWiO†sA›‡î@0@w9# û‘››`£ÜŒnşæù\Òî_¯ÇÍ€‚!º^•åfXéVãf@Áİ–ˆn”›a¥[›Ct¼*6ÊÍ°Ò­ÆÍ€‚!º^•åfXéVãf@Á]¯Jƒr3¬t«q3 `ˆ.ƒW¥ÁF¹VºÕ¸P0D—Á«Ò`£Ü+İjÜ(¢ËàUi°ƒÅùt«q3 `ˆîŠ€îG_İåf|ì`CQç’vÿz=nĞİ™Ñr3¬t«q3 `ˆ.ƒW¥ÁF¹VºÕ¸P0D·%¢åfXéVãf@Á]¯Jƒr3¬t«q3 `ˆ.ƒW¥ÁF¹VºÕ¸P0D—Á«Ò`£Ü+İjÜ(¢ËàUi°Qn†•n5nÑeğª4ØÁâ|ºÕ¸P0D—Á«úØ½M°W/~òëH÷+7TÎç’vÿz=ûˆ `ˆîŠˆn”We¥[WtwgDt£¼*+İj¼*(¢ËàUi°ëeºJ“Ş«‚‚!º-İ(¯ÊJ·¯

†è2xUl”We¥[WCt¼*6Ê«²Ò­Æ«‚‚!º^•åUYéVãUAÁ]¯Jƒòª¬t•fá^ğyUP0D—Á«Ò`ÖãÒUšô^Ñ%òªÖ#¯ˆêùÆ(¢»" ûøƒgŸæU=¾Şˆ¡sI»½¯

èîÍˆèFyUVºÕxUP0D—Á«Ò`£®ÌVºJ“Ş«‚‚!º-İ(¯ÊJ·¯

†è2xUl”We¥[WCt¼*6Ê«²Ò­Æ«‚‚!º^•åUYéVãUAÁ]¯Jƒòª¬t•fá^ğyUP0D—Á«Ò`ÖãÒUšô^Ñeğª¾êæ&Ø¨õîÇ7ó|.i÷¯×³Ş…‚!º^•µŞµÒ­f½tW3"ºQë]+İjÖ»P0D—Á«Ò`£Ö»VºÕ¬w¡`ˆnKD7j½k¥[Íz
†è2xUlÔz×J·šõ.Ñeğª4Ø¨õ®•n5ë](¢ËàUi°ƒåÛt«YïBÁ]¯êã¯n‚zf~bóï=ŸKÚıëõ<3CÁ]¯Jƒzf¶Ò­æ™
†è2xUlÔ3³•n5ÏÌP0D—Á«Ò`£™­t«yf†‚]¦»7›Ñzf¶Ò­æ™
†è2xUlÔ3³•n5ÏÌP0D·å¡»ªé÷//Šy5œièCt¼ª'î}^Ø}÷fOOç’vÿz=÷](¢ËàUi°Q÷]+İjî»P0D—Á«Ò`£î»VºÕÜw¡`ˆ.ƒWÕvß5Ò­ç¾Ct¼ª›Ÿ:û¼¸+s/Æù\Pí_¯çÊCt¼*6ìÊl¤[Í•
†è2xU}°qWfİz®ÌP0@·™Ñ½~1Øèîcš|ûw¡`ˆ.ƒWukvöyaWæ[Îç’vÿz=Wf(¢ÛòĞ]_¹øÉo<İuäúÅü®áLCw(¢ËàUi°WG¦{Ó<ıİ`ˆ.ƒW¥ÁF¬`¥«OQÍò)‹‡î–“ö¯Jƒzf¶Ò­æ™
†è2xU·¾éìóÂª|p#†Î%íşõŠª`ˆ.ƒWÕöTe¤[ÑSÑeğª4Ø¨§*+İzª`ˆ.ƒW¥ÁF=UYéÖóT…tÛİ¨§*+İzª`ˆ.…WÕåDZéîcš|N$Ñm	è>ùâÙçÅ=3¯{Š:?XÌ›×ëyf†‚!º^UlÜS•n=OUP0D—Á«Ò`ÃªŒt«yª‚‚!º^•öTe¤[ÍSÑeğªzÇ&Ø.~òëH÷©››¿Î%m}ıøSºãÓ…‚!º^UlØùÌFºõœÏCt¼*6ê™ÙJ·šgf(¢ËàUé…æÊÅOáÊ\Ïtç3"ºQ+"+İjVDP0D—Á«Ò`G¾ïÖ³"‚‚!º-İ¨‘•n5+"(¢ËàU=õgŸwµ"ºİÓÑ¹¤}{2ø9xçp¾ü3:]$¢ËàUi°QWf#İŠVDH0D—Á«Ò`£®ÌVºÕœàCt)¼ª>Ø¨]bVºôs±¾åÛ%Ct)¼ª>ØÁâ|ºõ¸H0D—Â«ÚæféVäf Á]
¯ªvä§ªŠÖ»H0@w1#¢;òSUEë]$¢ËàUİ~ôìóâÖ»¯öuó«“‹?üë](¢ÛÑ»ï¾: x?ºçTßUÌ—h³Ğİvß]0xUlØ•ÙH·šõ.Ñeğª4Ø°õ®‘n5ë](¢ËàUi°aë]#İjÖ»P0D—Á«êƒ[ïÚèÖ³Ş…‚!º^•{udºº¾¥¯ŞCÁ]¯Jƒs3Œt«q3 `ˆ.ƒWuû›Ï>/¬xç`#†Î%íşõz*€P0@w9#¢µ"²Ò­gE„Ct)¼ª>Ø¨‘•n=+"$¢ÛÑZYéÖ³"B‚!º^Õ&Ø°gf#İŠ™‘`ˆ.…WÕ;òSUEÏÌH0D—Á«ºó©M°Q5¢;÷zŠ:sÿú}jB|»Ä `ˆ.ƒWÕ{pıäÂ'¿ñtÏŸ‘÷‹¹x†¦¡;Ñeğª4Ø°gf#İj™¡`ˆ.ƒW¥Á†=3éVóÌCt¼*½\¹øÉ#Üw«©"@Á İİ°‘RÕù ˜û×«YAÁ]¯Jƒ[éV³"‚‚!º-İ0ŸÙHw…iòõDBÁ]¯êéÙÙç…õD>½á|.i?­"m~ø{"¡`ˆ.ƒW¥ÁF­wtëYïBÁ]¯JƒZïZéV³Ş…‚!º^UlØ3³‘n=ÏÌP0D—Á«Ò`£™­t«yf†‚!º^•õÌl¥[Í33Ñeğª4Ø‘Ÿªê9Ÿ
èîÎˆè\øäè¾÷Ò<ı½Åd ;Ñeğª4Ø÷L÷á~~h2œş9İ`ˆnHwèô«{¦§ÿê^‘Ï8ı/?ı·ÊxkOí3e|VÿwÑçôŸğ¹2¾Ídó[rê3|;ß^Æwñy2ä·fòd|gßEÆw•qEÆw“ñ Œï.ã{Èø2NŸ†¿—Œï-ãûÈø¾2N@?_Æ»e|Œï'ãô¹êûË8USò`"jMæ2NïÈ²–œÈŠc"Ï¥yz™œêö…2®Ëø2Ş#ã4û¿HÆ)…÷ÊxŸŒ/–qú{ó~Ëø€Œ/‘qºŞúR‡2d|™Œc”ñ¨Œ/—ñ˜Œ2>$ãÃ2¾BF'ã#2>*ãc2¾RÆã2¾JÆÇe<!ã¦Œ[2”ñ”ŒÓNî;2–ñŒŒS6ÏÊø_+ã9ÏË_ÇÉ]§L Œ—dùÉ+2¾NÆ’ñõ2>!ã“2~°Œ"ã‡Êøa2~¸Œ!ãDÆ”ñ£düh?FÆ•ñãdüx?AÆO”ñ“düd?EÆO•ñÓdütŸ’ñ3düL?KÆÏ–ñsdü\?OÆ7Èøù2~Œ_(ãÉøÅ2~‰Œ_*ãU¿LÆ/—ñ+düJ¿JÆ¯–ñkdüZ¿NÆ¯—ñdüF¿IÆo–ñ[d|£Œß*ã·Éøí2~‡Œß)ãwÉøİ2~Œß+ã÷Éøı2ş€Œ?(ãÉøÃ2Ö2şˆŒ?*ãÉøã2ş„Œ?)ãOÉøÓ2şŒŒ?+ãÏÉøó2ş‚Œ¿(ã/Éø&YÆ_‘ñWeü5]Æßñ7eü-[Æß‘ñweü=_Æ?ñeÜ“ñdücÿDÆ?•ñÏdüsÿBÆ¿”ñ¯dükÿFÆ¿•ñïdü{ÿAÆ7Ëø2ş“Œÿ,ã¿Èø¯2ş›Œÿ.ãÈøŸ2ş—Œÿ-ã[düŸÉæ:ğZ’ÿ—$ÿ“ä’üO’ÿIò?Iş'Éÿ$ùŸ$ÿ“ä’üO’ÿIò?Iş§wÈüO’ÿIò?Iş'Éÿ$ùŸ$ÿ“ä’üO’ÿIò?Iş'Éÿ$ùŸ$ÿÓƒ2$ÿ“ä’üO’ÿIò?Iş'Éÿ$ùŸ$ÿ“ä’üO’ÿIò?Iş'Éÿ4“!ùŸ$ÿ“ä’üO’ÿIò?Iş'Éÿ$ùŸ$ÿ“ä’üO’ÿIò?}ÑéEU†ä’üO’ÿIò?Iş'Éÿ$ùŸ$ÿ“ä’üO’ÿIò?Iş'Éÿ$ùŸ•!ùŸ$ÿ“ä’üO’ÿIò?Iş'Éÿ$ùŸ$ÿ“ä’üO’ÿIò?Iş§›2$ÿ“ä’üO’ÿIò?Iş'Éÿ$ùŸ$ÿ“ä’üO’ÿIò?Iş'Éÿô¢Éÿ$ùŸ$ÿ“ä’üO’ÿIò?Iş'Éÿ$ùŸ$ÿ“ä’üO’ÿIò?ÈüO’ÿIò?Iş'Éÿ$ùŸ$ÿ“ä’üO’ÿIò?Iş'Éÿ$ùŸ$ÿÓ§dHş'Éÿ$ùŸ$ÿ“ä’üO’ÿIò?Iş'Éÿ$ùŸ$ÿ“ä’üO’ÿIò?Iş'Éÿ$ùŸ$ÿ“ä’üO’ÿIò?Iş'Éÿ$ùŸ$ÿ“ä’üO’ÿIò?Iş'Éÿ$ùŸ$ÿ“ä’üO’ÿIò?Iş'Éÿ$ùŸ$ÿ“ä’üO’ÿIò?Iş'Éÿ$ùŸ$ÿ“ä’üO’ÿIò?Iş'Éÿ$ùŸ$ÿ“ä’üO’ÿIò?Iş'Éÿ$ùŸ$ÿ“ä’üO’ÿIò?Iş'Éÿ$ùŸ$ÿ“ä’üO’ÿIò?Iş'Éÿ$ùŸ$ÿ“ä’üO’ÿIò?Iş'Éÿ$ùŸ$ÿ“ä’üO’ÿIò?Iş'Éÿ$ùŸ$ÿ“ä’üO’ÿIò?Iş'Éÿ$ùŸ$ÿ“ä’üO¯mnûYò?ËÿÈ’ÿYò?KşgÉÿ,ùŸ%ÿ³ä–üÏ’ÿYò?¿}ót–?gó’%ÿ³ä–üÏ’ÿYò?KşgÉÿ,ùŸ%ÿ³ä–üÏ’ÿYò?KşgÉÿ,ùŸ%ÿ³ä–üÏ’ÿYò?KşgÉÿ,ùŸ%ÿ³ä–üÏ’ÿYò?KşgÉÿ,ùŸ%ÿ³ä–üÏ’ÿYò?KşgÉÿ,ùŸ%ÿ³ä–üÏ’ÿYò?KşgÉÿ,ùŸ%ÿ³ä–üÏ’ÿYò?KşgÉÿ,ùŸ%ÿ³ä–üÏ’ÿYò?KşgÉÿ,ùŸ%ÿ³ä–üÏ’ÿYò?KşgÉÿ,ùŸ%ÿ³ä–üÏ’ÿYò?KşgÉÿ,ùŸ%ÿ³ä–üÏ’ÿYò?KşgÉÿ,ùŸ%ÿ³ä–üÏ’ÿYò?KşgÉÿ,ùŸ%ÿóK§x2$ÿ³ä–üÏ’ÿYò?KşgÉÿ,ùŸ%ÿ³ä–üÏ’ÿYò?KşgÉÿ,ùŸ%ÿ³ä–üÏ’ÿYò?KşgÉÿ,ùŸ%ÿ³ä–üÏ’ÿYò?KşgÉÿ,ùŸ%ÿ³ä–üÏ’ÿYò?KşgÉÿ,ùŸ%ÿ³ä–üÏ’ÿYò?KşgÉÿ,ùŸ%ÿ³ä–üÏ’ÿYò?KşgÉÿ,ùŸ%ÿ³ä–üÏ’ÿYò?KşgÉÿ,ùŸ%ÿ³ä–üÏ’ÿYò?KşgÉÿ,ùŸ%ÿ³ä–üÏ’ÿYò?KşgÉÿ,ùŸ%ÿ³ä–üÏ’ÿYò?KşgÉÿ,ùŸ%ÿ³ä–üÏ’ÿYò?KşgÉÿ,ùŸ%ÿ³ä–üÏ’ÿYò?KşgÉÿ,ùŸ%ÿ³ä–üÏ’ÿYò?KşgÉÿ,ùŸ%ÿ³ä–üÏ’ÿYò?KşgÉÿ,ùŸ%ÿ³ä–üÏ’ÿYò?KşgÉÿ,ùŸ%ÿ³ä–üÏ’ÿYò?KşgÉÿ,ùŸ%ÿ³ä–üÏ¯mù§’ÿÓÓU‘üÃTò*ù?•üŸJşO%ÿ§’ÿSÉÿ©äÿTòzºfüŸJşO%ÿ§’ÿSÉÿ©äÿTò*ù?•üŸJşO%ÿOWSÉÿ©äÿTò*ù?•üŸJşO%ÿ§’ÿSÉÿ©äÿTò*ù?•üŸJşO%ÿ§’ÿSÉÿ©äÿTò*ù?•üŸJşO%ÿ§’ÿSÉÿ©äÿTò*ù?•üŸî®£dHşO%ÿ§’ÿSÉÿ©äÿTò*ù?•üŸJşO%ÿ§’ÿSÉÿ©äÿTò*ù?•üŸJşO%ÿ§’ÿSÉÿ©äÿTò*ù?•üŸJşO%ÿ§’ÿSÉÿ©äÿTò*ù?•üŸJşO%ÿ§’ÿSÉÿ©äÿTò*ù?•üŸJşO%ÿ§’ÿSÉÿ©äÿTòúL¿6ë‘ç+¹nßzåÖFï³¸wœı½ÍŸ}ş—|áãÉ?şÅxä½=qã±}ÙãO=óì­—xù¥Wâı³Ùì‰ãfví•gŸv½cé}GëşŒÖıs÷gÌİŸ±pÆÂıK÷g,İŸ±ãıŒÎ¨–OÏüïğıw,İ¿»K÷ïîÒı»»tÿî.İ¿»K÷ïîÒı»»tÿî.İ¿»K÷ïîÒû»ÛÌ¼¿‰gïpEÕ¸¯¢û*Ú¸¯¢û*Ú¸¯¢û*Ú¸¯¢û*Ú¸¯¢û*Ú¸¯¢òîß«îß«îß«îß«Îß+÷İ qß÷İ qß÷İ qß÷İ qß÷İ qß÷İ qßÿİ`éÎÁ¥;—î\ºspùÿ‘ƒ7|¿W­ûÎÙºïœ­ûÎÙºïœ­ûÎÙºïœ­ûÎÙºïœ­ûÎÙºïœ­ûÎÙºïœ­ûÎÙºïœ­ûÎÙºïœ­ûÎÙºïœ§ïpş^¹ïµ­û^Ûºïµ­û^Ûºïµ­û^Ûºïµ­û^Ûºïµ­û^Ûºïµ­û^Ûºïµ­û^Ûºïµ­û^Ûºïµ­û^Ûºïµs÷½vî¾×Îİ÷Ú¹û^;wßkçî{íÜ}¯»ïµs÷½vî¾×Îİ÷Ú¹û^;wßkçî{íÜ}¯»ïµs÷½vî¾×Îİ÷Ú¹û^;wßkçî{íÜ}¯»ïµs÷½vî¾×Îİ÷Ú¹û^;wßkçî{íÜ}¯»ïµs÷½vî¾×Îİ÷Ú¹û^{ú…ûÎ¹pß9î;çÂ}ç\¸ïœ÷sá¾s.ÜwÎ…ûÎ¹pß9î;çÂ}ç\¸ïœ÷}pá¾.Ü÷Á…û>¸pß£î{ÔÂ}Z¸ïQ÷=já¾G-Ü÷¨…ûµpß£î{ÔÂ}Z¸ïQ÷=já¾G-Ü÷¨…ûµp¯—î{ÔÒ}ÇYºï8K÷gé¾ã,İwœ¥û³tßq–î;ÎÒ}ÇYºï8K÷gé^«-İ÷¨¥ûuú§ºîuÔÒ}7XºïK÷İ`é¾,İwƒ¥ûn°tß–î»ÁÒ}7XºïK÷İ`é¾,İwƒ÷nÇ}…Ûq_ávÜW¸÷nÇ}…Ûq_ávÜWŸwï¸ó|Çç;î<ßqçù;ÏwÜy¾ãÎó÷Šşô¾ÿ]÷óÕ®ÛØu?‘íºŸÈvİOd»î'²]÷õj×}½Úu_¯vİ×«]÷õj×}½Úu_¯vİOd»î§¥]w7î®{}¾ë^ŸïºŸÈvİWê]÷•z×}¥Şu_©wİWê]÷•z×}¥Şu_©wO«–®ÏØs_÷Ü×Ä=÷5qÏ}MÜs_÷Ü×Ä=÷5qÏ}MÜs_÷Ü×Ä=÷5qÏ}eØs_öÜW†=÷•aÏ}eØs_öÜW†=÷•aÏ}eX¹ó|åÎó•;ÏWî<_¹ó|åÎó•;ÏWî<_¹ó|åÎó•;ÏWngå®4¬ÜO2+÷“ÌÊ}½Z¹¯W+÷õjå¾^­Ü×«•ûzµr_¯VîëÕÊ½ò:EÍÌ»‚Ü¼ÃóßÑ¸÷Z6î½–{¯eãŞkÙ¸÷Z6î½–{¯eãŞkÙ¸÷Z6î½–{¯åé;|+ÈÆ½×²qïµlÜ{-÷^ËÆ½×²qïµlÜ{-÷^ËÆ½×²qïµlÜ{-÷^ËÆ½×òôşß]ç•Ú½Ÿ³qïçlÜû9÷~ÎÆ½Ÿ³qïçlÜû9÷~ÎÆ½Ÿ³qïçlÜû9÷~ÎÆ½Ÿ³qïçlÜ{-÷^Ë³w8™»ïîİ™{wfãŞÙ¸wg6îİ™{wfãŞÙ¸wg6îİ™{wfãŞÙ¸wg6îİ™{wfãŞÙ¸wg6îİ™{wææ®ÏpïçlÜû9÷~ÎÆ½Ÿ³qïçlÜû9÷~ÎÆ½Ÿ³qïçlÜû9÷~ÎÆ½Ÿ³qïçlÜû9÷~ÎÆ½Ÿ³qïçlÜû9÷~ÎÆ½Ÿ³qïçlÜû9÷~ÎÆ½;³qïÎlÜ»3÷îÌÆ½;³qïÎlÜ»3÷îÌÆ½;³qïÎlÜ»3÷îÌÆ½;³qïÎlÜ»3÷îÌÆ½;³qïœlÜ;'÷ÎÉÆ½s²qïœlÜ;'÷ÎÉÆ½s²qïœlÜ;'÷ÎÉÆ½s²qïœlÜ;'÷ÎÉÆ½²qïƒlÜ»÷ÅÆ½G±qïQlÜ{÷ÅÆ½G±qïQlÜ{÷ÅÆ½Gqó÷g¸óÃ¹ºsïƒlÜ»÷®ÆÆ½G±qïQlÜ{÷ÅÆ½G±qïQlÜ{÷ÅÓw8sĞ½«±qïjlÜ»÷®Æ³w¸¯ü9xÃù{å¾º÷Z6î½–{¯eãŞkÙ¸w56î]{bãŞ£Ø¸÷(6î=Š{bãŞ£xög~øïQKo~¸wNnŞáŠÊ½×²qïƒlÜû÷şÁÆ½°qïlÜû÷şÁ³wø~Û—î»ÁÒ}7Xºïî]gïpªë~nwïÔkÜ;õ÷N½Æ½S¯qïÔ;{‡“¹w'Òæ®ÿ÷ÕgÇıîŞ©×¸wê¾Ãù[²ã¾–¸w½Ã÷[²ãÎÚ·ß¾ãÎó÷3ÜûÎ½°qï<}‡÷·Ä}-Ùq_KvÜ×’÷óÕûÙÇ½s²qïœlÜ;'÷ÎÉÆ½s²qïœlÜ{ûNßáü½Úu_}vİWŸ]÷sÉ®û¹d×½JuïQlvİWQ÷·Ów8¯%»îkÉ®ûZâŞEwö'ç•¡s»Jçşv•Îıí*ûÛU:÷·«tîoWéÜß®Ò¹¿]¥s»Jçşv•Îıí*ûÛU:÷w¥tîïJéÜß•Ò¹¿+¥sWJçş®”Îı])û»R:÷w¥tîïJéÜß•Ò¹¿+¥sÉæN­ÜW8oOjçşÆÎı!ûC:÷7mtîoÚèÜß´±y‡3?œwÎÎıíûÛ#:÷·GtîoèÜßÑ¹OàïÜ'ğwîÓ¿;÷9Ûû$èÎ}Jsç>G¸sŸ¿»y‡/½Ş³w8£òVû:÷¦›wøî8Ş*Yç>²sŸNÙ¹O§ìÜ§SvîÓ);÷Y“ı;\Qy+û¤ÂÎ}Raç>©pó·º.ß§sŸmØ¿ÃıN­Ü«	o sŸ××¹ÏëëÜçõõïp†ïî¼ãÎÁwzëı;ÜŸá[MxOìÜ§vîSÏŞáüİõÖ&:÷I…›wø~wwÜWŸ§ŸØ¿ÃıNæîk¢·Æ²y‡“ û	Ù[céÜ'Gvîs ;÷9ûÈÎ}äÙ;¼9è¾x+?›w¸	zsĞÿp¯„½Õ¥Î}Êfç>e³sŸ²Ù¹OÙìÜ§lvîS6;÷é”ûtÊÎ}:eç>²sŸN¹y‡›‡3½U²şîÏğå ·®¶y‡/½uµÎ}ögÿßg¸ïÎ»î»³÷DÒÎ}"iç>‘´sŸHÚ¹OíÜ§…vîÓB;÷i¡û´Ğ³wx¯%îû¹·úºy‡›¹÷Zâ¾Ÿ{kÂ›w¸¯¼×ÿ€Û³Üs?3ì¹Ÿ¼§ĞvîSh;÷)´ûÚÎ}
mç>…¶sŸBÛ¹O¡íÜ§ĞvîSh;÷)´gïp^¯öÜÏ>{îgŸ=÷“ÌûIfÏı\²ç~.Ùs?—ì¹Ÿ2öÜO{î§Œ=÷S†÷tãÎ}ºqç>İ¸sŸnÜ¹O7îÜ§wîÓ;÷éÆûtã³wx¯îg†=÷3ÃÊ}ç\¹ïœŞs;÷¹Îû\çÎ}®sç>×¹sŸëÜ¹ÏuîÜç:wîs;÷¹Îû\çÎ}®sç>×¹sŸÒÜ¹OiîÜ§4wîSš;÷)Íû”æÎ}Jsç>¥¹sŸÒÜ¹OiîÜgwî³Š;÷YÅû¬âÎ}Vqç>«¸sŸUÜ¹Ï*îÜgwî{;÷‰½ûÄŞÎ}boç>±·sŸØÛ¹OìíÜ'övî{;÷¹µûÜÚÎ}nmç>·¶sŸ[Û¹Ï­íÜçÖvîsk;÷¹µûÜÚÎ}¦lç>YµsŸ¬Ú¹OVíÜ'«vî“U;÷ÉªûdÕÎ}²jç>YµsŸ€Ù¹OÀìÜ'`vî0;÷	˜ûÌÎ}fç>³sŸTØ¹O*ìÜ'vî“
;÷I…û¤ÂÎ}&`ç>®sŸ×¹Ï‡ëÜ'±uîS¹:÷©\ûü«Î}şUç>§¨sŸSÔ¹Ï)êÜçuîsŠ:÷	4ûšÎ}Mç>¦sŸ@Ó¹O éÜç—tîÓH:÷i$û4’Î}îÇæşÏpjåş½r÷ïºOñèÜ§xt»SöìşÏğıîº»Xİ'ltî6:÷yû¼ŒÎ}úEç>ıbó'÷ÓİÑè>Ë¢sŸeÑ¹Ï²èww[ãıéÎ}ÚÂæ>uİgîó:÷)›wøVEî^œÆİ‹ã>	¡sŸ„Ğ¹OBØ¼Ã÷›èîıhÜ=»× q÷4î>€³w¸µr>_¹;š“²¼#õï|Ó·¿ó²üw\xñ-O?ûÜÓ?(şò[_ºûõ×ôo—‘Oäÿ½öÚ·|Ãæ/¾ùµâ_ô¶ÛÔµ§_:ûWM¾õÓ+_ü[ŸwãÅ[¯<{ë¹Gï>÷Égî¾ğòéÉÃ/ü[Êk~ùÅOÿÛÎ_}óSwŸşÎ¯ùM¿÷èÆû7ò­ÿ”´r¢? #' Double-Window Flexible Pace Search (DFPS) threshold determination
#'
#' @export
#' @importFrom rgeos gDifference gBuffer
#' @param chg_polys \code{SpatialPolygonsDataFrame} with polygons of change 
#' areas surrounded by windows of no-change
#' @param chg_mag change magnitude \code{RasterLayer} from \code{CVAPS}
#' @param radius radius of no-change surrounding change area polygons
#' @param delta the minimum difference between Lmax and Lmin that will allow 
#' another pass through the search loop
#' @param m number of potential thresholds per search iteration (see Chen et 
#' al., 2003). Recommended to leave at default value.
#' @param maxiter maximum number of iterations of the main search process
#' @references Chen, J., P. Gong, C. He, R. Pu, and P. Shi. 2003.
#' Land-use/land-cover change detection using improved change-vector analysis.
#' Photogrammetric Engineering and Remote Sensing 69:369-380.
#' 
#' Chen, J., X. Chen, X. Cui, and J. Chen. 2011. Change vector analysis in
#' posterior probability space: a new method for land cover change detection.
#' IEEE Geoscience and Remote Sensing Letters 8:317-321.
DFPS <- function(chg_polys, chg_mag, radius=100, delta=.01, m=10, maxiter=20) {
    chg_pixels <- unlist(extract(chg_mag, chg_polys))
    nochg_polys <- gDifference(gBuffer(chg_polys, width=radius), chg_polys)
    nochg_pixels <- unlist(extract(chg_mag, nochg_polys))
    # Calculate total number of pixels (used later)
    A <- length(chg_pixels) + length(nochg_pixels)
    # Set initial values for Lmax and Lmin that ensure the below while loop 
    # will run.
    Lmax <- 1
    Lmin <- Lmax - 2 * delta
    min_threshold <- min(chg_pixels)
    max_threshold <- max(chg_pixels)
    n <- 0
    while ((Lmax - Lmin) > delta && n < maxiter) {
        p <- (max_threshold - min_threshold) / m
        thresholds <- seq(min_threshold, max_threshold, p)
        L <- c()
        for (threshold in thresholds) {
            A1 <- sum(chg_pixels > threshold)
            A2 <- sum(nochg_pixels > threshold)
            # Calculate A according to equation 4 in Chen et al. 2003
            L <- c(L, ((A1 - A2) * 100) / A)
        }
        kmax <- thresholds[match(max(L), L)]
        min_threshold <- kmax - p
        max_threshold <- kmax + p
        Lmin <- min(L)
        Lmax <- max(L)
        n <- n + 1
    }
    return(kmax)
}
#' creates a raster with the difference of Normalized Burn Ratio (NBR) index
#' @description Creates a raster with the difference of Normalized Burn Ratio (NBR) index 
#'
#' @param prefire name of the prefire product, e.g. LC80522102014165LGN00. It must be in the working directory.
#' @param postfire name of the postfire product, e.g. LC80522102014165LGN00. It must be in the working directory.
#' @param sun angle correction, default is.suncorrected = FALSE
#' @return difference of Normalized Burn Ratio (NBR) indexes raster
#' @examples \dontrun{
#' prels8 <- ReadLandsat8("LC81880342014174LGN00")
#' postls8 <- ReadLandsat8("")
#' r <- dNBR(prels8, postls8)
#' }
#'
#' @export
#' @import raster

dNBR <- function(prefire, postfire, is.suncorrected = FALSE) {

  dnbr <- ToNBR(prefire, is.suncorrected) - ToNBR(postfire, is.suncorrected)

  return(dnbr)

}
DOS <-
function(sat=5, scattering.coef=c(-4, -2, -1, -.7, -.5), SHV, SHV.band, gain, offset, Grescale, Brescale, sunelev, edist, Esun=c(198.3, 179.6, 153.6, 103.1, 22, 8.34), blackadjust = 0.01)
{

### Improved Dark Object Subtraction method from Chavez 1989
### Implements the 1% adjustment (can be changed with blackadjust argument)

### Dark Object Subtraction method from Chavez 1988
### with some modifications 
### calculates DN to subtract for each band for a range of
### scattering.coef: scattering coefficients. Default values:
###	-4.0: Very Clear	SHV <= 55
###	-2.0: Clear		SHV 56-75
### 	-1.0: Moderate		SHV 76-95
###	-0.7: Hazy		SHV 96-115
###	-0.5: Very Hazy		SHV >115
### sat: 5 or 7 for Landsat platform
### sunelev: sun elevation in degrees
### satzenith: satellite zenith angle in degrees (= 0 for Landsat)
### edist: Earth-Sun distance (calculated from DOY)
### Esun: extrasolar radiation
### gain and offset or gain and bias
### SHV: lowest DN value; from a black object
### SHV.band: band from which SHV was taken
### blackadjust: lowest DN not from an absolutely black object; reduce it by 1% or other value

### returns the mean version of Chavez 1988 and a slightly more
### sophisticated approximation over the entire band's wavelengths

    if(sat == 5)
        bands <- data.frame(
            lmin=c(0.45, 0.52, 0.63, 0.76, 1.55, 2.08),
            lmax=c(0.52, 0.60, 0.69, 0.90, 1.75, 2.35))
    else if(sat == 7)
        bands <- data.frame(
            lmin=c(0.45, 0.52, 0.63, 0.77, 1.55, 2.09),
            lmax=c(0.52, 0.60, 0.69, 0.90, 1.75, 2.35))
    else stop("Unknown satellite.\n")

    rownames(bands) <- c("band1", "band2", "band3", "band4", "band5", "band7")


    ### Chavez 1988 Table 1
    ### Values of specific functions for the Landsat bands

    scattering.mean <- matrix(apply(bands, 1, mean), byrow=FALSE, nrow=nrow(bands), ncol=length(scattering.coef))
    rownames(scattering.mean) <- rownames(bands)
    colnames(scattering.mean) <- paste("coef", scattering.coef, sep="")
    scattering.mean <- sweep(scattering.mean, 2, scattering.coef, "^")
    scattering.mean.pct <- sweep(scattering.mean, 2, apply(scattering.mean, 2, sum), "/")

    # alternate version using curve approximation
    scattering.approx <- matrix(NA, nrow=nrow(bands), ncol=length(scattering.coef))
    rownames(scattering.approx) <- rownames(bands)
    colnames(scattering.approx) <- paste("coef", scattering.coef, sep="")

    grain <- 0.0001
    for(i in 1:nrow(bands)) {
        thisband <- seq(bands[i, 1], bands[i, 2], by=grain)
        for(j in 1:length(scattering.coef)) {
            scattering.approx[i, j] <- mean(thisband ^ scattering.coef[j])
        }
    }
    scattering.approx.pct <- sweep(scattering.approx, 2, apply(scattering.approx, 2, sum), "/")

    ### Chavez 1988 Table 2
    ### Multiplication factors to predict haze values in other spectral 
    ### bands given a starting haze value and band

    corrband.mean <- scattering.mean[SHV.band, ]
    corrband.mean <- sweep(scattering.mean, 2, corrband.mean, "/")

    corrband.approx <- scattering.approx[SHV.band, ]
    corrband.approx <- sweep(scattering.approx, 2, corrband.approx, "/")

    ### Chavez 1988 Table 3; Chavez 1989 Table 1
    ### Gain, offset and normalization factors

    # most new references provide gain and bias
    # need gain and offset

    # most new references provide gain and bias
    # want gain and offset
    if(missing(offset)) {
        offset <- -1 * Brescale / Grescale
        gain <- 1/Grescale
    }

    NORM <- gain / gain[SHV.band]

    ### convert sunelev in degrees to sun zenith angle in radians
    suntheta <- (90-sunelev) * pi / 180
    suntheta <- cos(suntheta)

    ### Calculate Eo from Esun and edist
    Eo <- Esun[SHV.band]/edist^2
        
    # subtract 1% - assume that black in the image is not really black
    SHV <- SHV - gain[SHV.band] * blackadjust * Eo * suntheta / pi


    ## from here on, follow DOS algorithm of Chavez 1988


    SHV <- SHV - offset[SHV.band]

    DNfinal.mean <- SHV * corrband.mean 
    DNfinal.mean <- sweep(DNfinal.mean, 1, NORM, "*")
    DNfinal.mean <- sweep(DNfinal.mean, 1, offset, "+")

    DNfinal.approx <- SHV * corrband.approx 
    DNfinal.approx <- sweep(DNfinal.approx, 1, NORM, "*")
    DNfinal.approx <- sweep(DNfinal.approx, 1, offset, "+")


    list(DNfinal.mean = DNfinal.mean, DNfinal.approx = DNfinal.approx)

}

#' creates a raster with the brightness temperature extrcted from Landsat tirs1 band
#' @description Creates a raster with the brightness temperature extrcted from Landsat tirs1 band.
#'
#' @param product name of the product, e.g. LC80522102014165LGN00. It must be in the working directory.
#' @return brightness temperature raster
#' @examples \dontrun{
#' DownloadLandsat("http://earthexplorer.usgs.gov/download/4923/LC81370362014185LGN00/STANDARD/INVSVC", "LC80522102014165LGN00")
#' }
#'
#' @export
#' @import RCurl

DownloadLandsat <- function(url, output.name) {
  # todo: use RCurl instead of the system call
  
  command.args <- paste0("-c cookies.txt -d 'username=", usgs.username, "&password=", usgs.password,"' https://earthexplorer.usgs.gov/login")
  
  # invoke the system call to curl.
  # I'd rather have done this with RCurl but couldn't get it working ;-(
  ret <- system2("curl", command.args, stdout=TRUE, stderr=TRUE)
  
  command.args <- paste0("-b cookies.txt -L ", url," -o ", output.name)
  ret <- system2("curl", command.args, stdout=TRUE, stderr=TRUE)
  
  file.remove(cookies.txt)
  
  return(ret)
  
}
#' Plot EarthExplorer scene list
#'
#' This function can produce two different types of plots from a USGS 
#' EarthExplorer Landsat CDR Surface Reflectance scene list.
#'
#' @export
#' @import ggplot2
#' @importFrom dplyr group_by summarize
#' @importFrom lubridate new_interval %within%
#' @param x a \code{data.frame} with a list of Landsat scenes as output by
#' \code{\link{ee_read}}
#' @param start_date starting date as a \code{Date} object
#' @param end_date end date as a \code{Date} object
#' @param min_clear the minimum percent clear to plot (calculated as 1 - 
#' percent cloud cover). Images with less than \code{min_clear} fraction of the 
#' image area free of clouds will be ignored.
#' @param exclude a list of sensors to exclude (for example, set 
#' \code{exclude=c('LE7', 'LT4')} to exclude Landsat 7 ETM+ and Landsat 4 TM 
#' images.
#' @param normalize if \code{TRUE}, plot as a normalized line plot
#' @param title title for plot (or \code{NULL} for no title)
#' @return used for side effect of producing a plot
ee_plot <- function(x, start_date, end_date, min_clear=.7, exclude=list(), 
                    normalize=FALSE, title=NULL) {
    if (!class(start_date) == 'Date') {
        stop('start_date must be a "Date" object')
    }
    if (!class(end_date) == 'Date') {
        stop('end_date must be a "Date" object')
    }
    x <- x[!(x$Sensor %in% exclude), ]
    x$Sensor <- factor(x$Sensor)

    x <- x[order(x$WRS.Path, x$WRS.Row), ]
    x <- x[x$Frac_Clear >= min_clear, ]

    if ((!missing(start_date) && missing(end_date)) ||
        (missing(start_date) && !missing(end_date))) {
        stop('both start_date and end_date must be provided')
    } else if (!missing(start_date) && !missing(end_date)) {
        sel_interval <- new_interval(start_date, end_date)
        x <- x[x$Date.Acquired %within% sel_interval, ]
    }
    if (nrow(x) == 0) {
        stop('no data to plot - try different start/end dates')
    }

    if (!normalize) {
        YearMonth=Month=Cum_Month=Path_Row=Sensor=Frac_Clear=NULL # Keep R CMD CHECK happy
        x <- transform(group_by(x, YearMonth),
                       Cum_Month=cumsum(rep(1, length(Month))))
        p <- ggplot(x, aes(xmin=Month,
                           xmax=Month + 1, 
                           ymin=Cum_Month - 1, 
                           ymax=Cum_Month,
                           colour=Sensor,
                           fill=Path_Row,
                           alpha=Frac_Clear)) +
            geom_rect() + facet_grid(Year ~ ., scales='free_y', space='free_y') +
            xlab('Month') +
            scale_x_continuous(breaks=c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12),
                               labels=c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 
                                        'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec')) +
            theme(panel.grid.minor.y=element_blank(), 
                  panel.grid.major.y=element_blank(),
                  panel.grid.major.x=element_blank()) +
            theme(axis.ticks.y=element_blank(),
                  axis.text.y=element_blank()) +
            scale_colour_brewer(type='qual', palette='Set1', drop=FALSE, name='Sensor') +
            scale_fill_brewer(type='qual', palette='Set2', drop=FALSE, name='Path/Row') +
            scale_alpha(name='Fraction Clear')
    } else {
        # Keep R CMD CHECK happy:
        YearMonth=Path_Row=Year=Month=Max_Frac_Clear=Frac_Clear=Sum_Max_Frac_Clear=NULL
        Frac_Clear_Stats <- summarize(group_by(x, YearMonth, Path_Row),
                                      Year=Year[1], Month=Month[1],
                                      Max_Frac_Clear=max(Frac_Clear))
        Frac_Clear_Stats <- summarize(group_by(Frac_Clear_Stats, YearMonth), 
                                      Year=Year[1], Month=Month[1],
                                      Sum_Max_Frac_Clear=sum(Max_Frac_Clear))
        p <- ggplot(Frac_Clear_Stats, aes(xmin=Month + .05,
                                          xmax=Month + 1-.05, 
                                          ymin=0, 
                                          ymax=Sum_Max_Frac_Clear)) +
            geom_rect() + facet_grid(Year ~ .) +
            xlab('Month') + ylab('Total Fraction Clear') +
            scale_x_continuous(breaks=c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12),
                               labels=c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 
                                        'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec')) +
            theme(panel.grid.minor.y=element_blank(), 
                  panel.grid.major.y=element_blank(),
                  panel.grid.major.x=element_blank()) +
            theme(axis.ticks.y=element_blank(),
                  axis.text.y=element_blank()) +
            geom_hline(yintercept=seq(1,length(unique(x$Path_Row))), colour='white', linetype='dashed')
    }

    if (!is.null(title)) {
        p <- p + ggtitle(title)
    }

    return(p)
}
#' Read EarthExplorer CSV format scene list
#'
#' This function reads in a CSV file of Landsat CDR Surface Reflectance images 
#' as output from USGS EarthExplorer. param x a \code{data.frame} with a list 
#' of Landsat scenes as output from the save metadata function on 
#'
#' @export
#' @param x path to a CSV file with a list of Landsat scenes as output from the 
#' save metadata function on http://earthexplorer.usgs.gov
#' @return x a \code{data.frame} with a list of Landsat scenes and their 
#' associated metadata
ee_read <- function(x) {
    scenes <- read.csv(x, stringsAsFactors=FALSE, quote="", 
                          na.strings=c('NA', ' '))

    scenes$Sensor <- substr(scenes$Landsat.Scene.Identifier, 1, 3)
    scenes$Sensor <- factor(scenes$Sensor)

    # Dates are formatted as either: 1999/12/31 or 12/31/1999
    yr_first <- grepl('^[0-9]{4}/', scenes$Date.Acquired)
    yr_last <- grepl('/[0-9]{4}$', scenes$Date.Acquired)
    if ((sum(yr_first) + sum(yr_last)) < nrow(scenes)) {
        stop('unrecognized date format in Date.Acquired column')
    }
    acq_date <- as.Date(scenes$Date.Acquired)
    acq_date[yr_first] <- as.Date(scenes$Date.Acquired[yr_first], '%Y/%m/%d')
    acq_date[yr_last] <- as.Date(scenes$Date.Acquired[yr_last], '%m/%d/%Y')
    scenes$Date.Acquired <- acq_date

    scenes$Year <- as.numeric(format(scenes$Date.Acquired, '%Y'))
    scenes$Month <- as.numeric(format(scenes$Date.Acquired, '%m')) - .5
    scenes$MonthFactor <- factor(format(scenes$Date.Acquired, '%m'))
    scenes$Path_Row <- factor(paste(scenes$WRS.Path, scenes$WRS.Row, sep='/'))
    scenes$YearMonth <- paste(scenes$Year, scenes$MonthFactor, sep='/')
    scenes$Frac_Clear <- (100 - scenes$Cloud.Cover) / 100
    scenes <- scenes[order(scenes$WRS.Path, scenes$WRS.Row), ]

    if (nrow(scenes) == 0) {
        stop(paste0('no data found in', x,
                    ' - is scenes an EarthExplorer CSV export?'))
    }

    # Drop ENGINEERING, TEST, EXCHANGE, and VALIDATION data. See the Landsat 
    # data dictionary at: https://lta.cr.usgs.gov/landsat_dictionary.html
    scenes <- scenes[scenes$Data.Category == 'NOMINAL', ]

    return(scenes)
}
ESdist <-
function(adate)

{
    # estimate Earth-Sun distance for date adate
    # in format "YYYY-MM-DD"
    # result is in AU

    edist <- julian(as.Date(adate), origin=as.Date(paste(substring(adate, 1, 4), "12", "31", sep="-")))[[1]]
    edist <- 1 - 0.016729 * cos((2*pi) * (0.9856 * (edist - 4)/360))
    
    edist
}

verify_download <- function(espa_url, local_path) {
    cksum_file <- tempfile()
    ret_code <- download.file(gsub('\\.tar\\.gz$', '.cksum', espa_url), 
                              cksum_file, mode="w", quiet=TRUE)
    if (ret_code != 0) {
        message(paste('Warning: problem downloading cksum for', local_path))
        return(1)
    } else {
        # TODO: Check return code and handle accordingly
        # The first element is the cksum, second is the expected file size in 
        # bytes, and the third is the filename
        espa_checksum <- scan(cksum_file, what=c('integer', 'integer', 
                                                 'character'), quiet=TRUE)
        unlink(cksum_file)
        local_size <- file.info(local_path)$size
        # TODO: Figure out how to compute a checksum in R that matches the checksum 
        # output ESPA gives. It appears the ESPA checksum is a CRC from 'cksum' 
        # command run on Linux. This is not a CRC-32 checksum, so the R digest 
        # package won't work for computing it. bitops has a function, 'cksum' that 
        # might work.
        #local_crc <- strtoi(digest(, algo="crc32", file=TRUE), base=16L)

        # f = file(local_path,"rb")
        # local_crc <- cksum(rawToChar(readBin(f, raw(), n=local_size)))
        # close(f)

        # if (espa_checksum[1] != local_crc) {
        #     return(2)
        # } else if (espa_checksum[2] != local_size) {
        if (espa_checksum[2] != local_size) {
            return(3)
        } else {
            return(0)
        }
    }
}

download_ESPA_file <- function(espa_url, output_path) {
    ret_code <- download.file(espa_url, output_path, mode="wb")
    if (ret_code != 0) {
        message(paste('Warning: problem downloading', output_path))
        return(1)
    } else if (verify_download(espa_url, output_path) != 0) {
        message(paste("Warning: checksum mismatch on", output_path))
        return(2)
    } else {
        return(0)
    }
}

#' Download a completed ESPA order
#'
#' Function to download a set of Landsat images from ESPA given a valid order 
#' ID and the email that placed the ESPA order.
#' 
#' @export
#' @importFrom RCurl getURL getCurlHandle postForm
#' @importFrom stringr str_extract
#' @param email address used to place the order
#' @param order_ID the ESPA order ID
#' @param output_folder the folder to save output data in
#' @param username your USGS EarthExplorer username
#' @param password your USGS EarthExplorer password
#' @return used for the side effect of downloading Landsat scenes
espa_download <- function(email, order_ID, output_folder, username,
                          password) {
    stop("Due to changes in the ESPA system, espa_download is not working as of 7/1/2014")
    email_re <- '^[A-Z0-9._%+-]+@[A-Z0-9.-]+\\.[A-Z]{2,4}$'
    if (!grepl(email_re, email, ignore.case=TRUE)) {
        stop(paste(email, 'does not appear to be a valid email address'))
    }
    # Below is for old format ESPA order IDs (through end of 2013)
    # ESPA generates order IDs with below Python code:
    #    d = datetime.datetime.now()
    #    return '%s-%s%s%s-%s%s%s' % (email,d.month,d.day,d.year,d.hour,d.minute,d.second)
    # Note that the above does not guarantee unique ESPA order IDs. See
    # https://code.google.com/p/espa/issues/detail?id=123 for details.
    mth_re <- '([1-9]|(1[0-2]))'
    day_re <- '([1-9]|([1-2][0-9])|(3[0-1]))'
    yr_re  <- '20[0-9][0-9]'
    hr_re  <- '([0-9]|(1[0-9])|(2[0-3]))'
    min_re <- '([0-9]|([1-5][0-9]))'
    sec_re <- '([0-9]|([1-5][0-9]))'
    order_id_re <- paste0('^', mth_re, day_re, yr_re, '-',  hr_re, min_re, sec_re, '$')
    if (!grepl(order_id_re, order_ID)) {
        stop(paste(order_ID, 'does not appear to be a valid ESPA order ID'))
    }
    
    if (!file_test('-d', output_folder)) {
        stop(paste(output_folder, 'does not appear to be a valid directory'))
    }

    # Parse ESPA page for download links
    # TODO: Rewrite using httr - see http://bit.ly/1m8ZWXf
    email_noat <- gsub('@', '%40', email)
    options(RCurlOptions=list(cainfo=system.file("CurlSSL", "cacert.pem", 
                                                 package="RCurl")))
    curl=getCurlHandle()
    login_page <- unlist(strsplit(getURL('https://espa.cr.usgs.gov/login/', curl=curl), '\n'))
    csrfmiddlewaretoken <- login_page[grepl("csrfmiddlewaretoken", login_page)]
    csrfmiddlewaretoken <- gsub("(value=)|(')", '',
                                str_extract(csrfmiddlewaretoken, 
                                            "value='[a-zA-Z0-9]*'"))
    params <- list('username'=username,
                   'password'=password,
                   'submit'="Log In",
                   'next'="",
                   'csrfmiddlewaretoken'=csrfmiddlewaretoken)
    post_res <- postForm('https://espa.cr.usgs.gov/login',
                         .params=params, style="POST", curl=curl)
    tryCatch(espa_page <- getURL(paste0("http://espa.cr.usgs.gov/ordering/status/", email_noat, 
                             "-", order_ID, curl=curl)),
             error=function(e) stop('error loading order - check order ID and email'))
    url_re <- paste0('http://espa\\.cr\\.usgs\\.gov/orders/', email, '-', 
                     order_ID, '/L[ET][0-9]{14}-SC[0-9]{14}\\.tar\\.gz')
    espa_urls <- espa_page[grepl(url_re, espa_page)]
    espa_urls <- str_extract(espa_urls, url_re)
    if (length(espa_urls) == 0) {
        stop('no download links found')
    }

    successes <- 0
    failures <- 0
    skips <- 0
    message(paste('Found', length(espa_urls), 'ESPA downloads.'))
    for (n in 1:length(espa_urls)) {
        espa_url <- espa_urls[n]
        img_file <- basename(espa_url)
        output_path <- file.path(output_folder, img_file)
        if (file.exists(output_path)) {
            if (verify_download(espa_url, output_path)) {
                message(paste(img_file, 'exists but has bad checksum - re-downloading file'))
            } else {
                message(paste(img_file, 'exists and has good checksum - skipping download'))
                skips <- skips + 1
                next
            }
        }
        if (download_ESPA_file(espa_url, output_path) == 0) {
            successes <- successes + 1
        } else {
            failures <- failures + 1
        }

    }
    message(paste(successes, "file(s) succeeded,", skips, "file(s) skipped,", 
                failures, "file(s) failed."))
}
#' Extract a set of Landsat tarballs into a folder tree organized by image date
#'
#' Each image .tar.gz file will be extracted into a subfolder within 
#' \code{output_folder}. The subfolders will be named according to the year and 
#' Julian date of capture, and the sensor type (LT4, LT5 or LE7 for Landsat 4 
#' TM, Landsat 5 TM, and Landsat 7 ETM+ respectively). For example, 
#' "LT50150531986037-SC20130816144215.tar.gz" would be extracted into a 
#' subfolder named "1986_037_LT5', for 1986, Julian day 37, and Landsat 5 TM.
#'
#' Zip files for images from sensors not included in \code{sensors}, from 
#' path/rows not included in \code{pathrows} or with acquisition dates
#' outside of the period defined by \code{start_date} and \code{end_date} will 
#' be ignored.
#'
#' @export
#' @importFrom stringr str_extract
#' @param in_folder Path to a folder of .tar.gz Landsat surface reflectance 
#' images
#' @param out_folder output folder
#' @param pathrows a list of paths/rows to include. Each path/row should be 
#' specified as a six digit string. For example, path 231, row 62 would be 
#' specified as "231062".
#' @param start_date start date of period from which images will be extracted
#' to (as \code{Date} object).
#' @param end_date end date of period from which images will be extracted
#' to (as \code{Date} object)
#' @param sensors a list of the sensors to include (can be any of "LT4", "LT5", 
#' "LE7", or "LC8")
#' @return nothing (used for side effect of unzipping Landsat CDR tarballs)
#' @examples
#' \dontrun{
#' # Don't filter:
#' espa_extract('D:/Landsat_Originals', 'D:/Landsat_Out')
#'
#' # Filter by start and end date:
#' start_date <- as.Date('2010/1/1')
#' end_date <- as.Date('2010/12/31')
#' espa_extract('D:/Landsat_Originals', 'D:/Landsat_Out',
#'              start_date=start_date, end_date=end_date)
#'
#' # Filter by start and end date, sensor, and pathrow:
#' espa_extract('D:/Landsat_Originals', 'D:/Landsat_Out', 
#'              start_date=start_date, end_date=end_date, sensors='LE7',
#'              pathrows='231062')
#' }
espa_extract <- function(in_folder, out_folder, pathrows=NULL, start_date=NULL, 
                         end_date=NULL, sensors=NULL) {
    if (!file_test('-d', in_folder)) {
        stop(paste(in_folder, 'does not exist'))
    }
    if (!file_test('-d', out_folder)) {
        stop(paste(out_folder, 'does not exist'))
    }

    zipfiles <- dir(in_folder, pattern='^.*.tar.gz(ip)?$')

    # Filter by date
    img_dates <- as.Date(gsub('-', '', str_extract(zipfiles, '[0-9]{7}-')), '%Y%j')
    if (!is.null(start_date)) {
        stopifnot(class(start_date) == 'Date')
        inc_dates <- which(img_dates >= start_date)
        zipfiles <- zipfiles[inc_dates]
        img_dates <- img_dates[inc_dates]
    }
    if (!is.null(end_date)) {
        stopifnot(class(end_date) == 'Date')
        inc_dates <- which(img_dates < end_date)
        zipfiles <- zipfiles[inc_dates]
        img_dates <- img_dates[inc_dates]
    }

    # Filter by pathrow
    img_pathrows <- gsub('(LT[45])|(LE7)|(LC8)', '', str_extract(zipfiles, '((LT[45])|(LE7)|(LC8))[0-9]{6}'))
    if (!is.null(pathrows)) {
        stopifnot(!is.na(str_extract(pathrows, '[0-9]{6}')))
        inc_pathrows <- img_pathrows %in% pathrows
        zipfiles <- zipfiles[inc_pathrows]
        img_pathrows <- img_pathrows[inc_pathrows]
        img_dates <- img_dates[inc_pathrows]
    }

    # Filter by sensor
    img_sensors <- str_extract(zipfiles, '^((LT[45])|(LE7)|(LC8))')
    if (!is.null(sensors)) {
        stopifnot(!is.na(str_extract(sensors, '^((LT[45])|(LE7)|(LC8))$')))
        inc_sensors <- img_sensors %in% sensors
        zipfiles <- zipfiles[inc_sensors]
        img_pathrows <- img_pathrows[inc_sensors]
        img_sensors <- img_sensors[inc_sensors]
        img_dates <- img_dates[inc_sensors]
    }

    img_paths <- str_extract(img_pathrows, '^[0-9]{3}')
    img_rows <- str_extract(img_pathrows, '[0-9]{3}$')

    if (length(zipfiles) == 0) {
        stop('No images found')
    }

    for (n in 1:length(zipfiles)) {
        zipfile_path <- file.path(in_folder, zipfiles[n])
        # Figure out which satellite the image is from
        year <- format(img_dates[n], '%Y')
        julian_day <- format(img_dates[n], '%j')
        this_out_folder <- file.path(out_folder,
                                     paste0(img_paths[n],'-', img_rows[n], '_', year, 
                                            '-', julian_day, '_', img_sensors[n]))
        if (!file_test('-d', this_out_folder)) {
            dir.create(this_out_folder)
        } else {
            message(paste('Skipping', zipfiles[n], '- output dir', 
                          this_out_folder, 'already exists.'))
            next
        }
        message(paste0(n, ' of ', length(zipfiles), '. Extracting ', zipfiles[n], ' to ', this_out_folder))
        ret_code <- untar(zipfile_path, exdir=file.path(this_out_folder))
        if (ret_code != 0) {
            message(paste('WARNING: error extracting', zipfiles[n], '- return 
                          code', ret_code))
        }
    }
}
#' Save scenelist from EarthExplorer metadata for upload to ESPA
#'
#' @export
#' @importFrom lubridate new_interval %within%
#' @param x a \code{data.frame} with a list of Landsat scenes as output from 
#' the save metadata function on http://earthexplorer.usgs.gov
#' @param start_date starting date as a \code{Date} object
#' @param end_date end date as a \code{Date} object
#' @param out_file filename for output text file for later upload to ESPA
#' @param min_clear the minimum percent clear to plot (calculated as 1 - 
#' percent cloud cover). Images with less than \code{min_clear} fraction of the 
#' image area free of clouds will be ignored.
#' @param exclude a list of sensors to exclude (for example, set 
#' \code{exclude=c('LE7', 'LT4')} to exclude Landsat 7 ETM+ and Landsat 4 TM 
#' images.
#' @return used for side effect of producing ESPA scene list
espa_scenelist <- function(x, start_date, end_date, out_file, min_clear=.7, 
                         exclude=list()) {
    if (!class(start_date) == 'Date') {
        stop('start_date must be a "Date" object')
    }
    if (!class(end_date) == 'Date') {
        stop('end_date must be a "Date" object')
    }
    if ((!missing(start_date) && missing(end_date)) ||
        (missing(start_date) && !missing(end_date))) {
        stop('both start_date and end_date must be provided')
    } else if (!missing(start_date) && !missing(end_date)) {
        sel_interval <- new_interval(start_date, end_date)
        x <- x[x$Date.Acquired %within% sel_interval, ]
    }
    if (nrow(x) == 0) {
        stop('no data to download - try different start/end dates')
    }
    x <- x[!(x$Sensor %in% exclude), ]
    x <- x[x$Frac_Clear >= min_clear, ]
    write.table(x$Landsat.Scene.Identifier, out_file, row.names=FALSE, 
                col.names=FALSE, quote=FALSE, sep='\n')
}
#' Estimate image haze for dark object subtraction procedures
#' 
#' @param x raster object or a previous result from \code{estimateSHV(x , returnTables = TRUE} from which to estimate haze
#' @param band character. Band or bandname from which to estimate SHV (optinal if x contains only one layer)
#' @param darkProp proportion of pixels estimated to be dark
#' @param plot display histograms and haze values
#' @param returnTables return the frequency table per layer. Only takes effect if x is a Raster* object. If x is a result of estimateSHV tables will always be returned.
#' @export 
estimateSHV <- function(x, hazeBand, darkProp = 0.02, plot = FALSE, returnTables = TRUE) {
	
	## Initial or repeated run?
	if(inherits(x, "Raster")) {
		preCalc <- FALSE
	} else {
		if(is.list(x) & "table" %in% names(x)) {
			preCalc <- TRUE 
		} else {
			stop("x must be a Raster* object or the result of a previous run of estimateSHV(Raster*, ) with argument 'returnTables = TRUE'", call. = FALSE)
		}	
	}
	
	if(!preCalc){
		if(missing(hazeBand)){ 
			if(nlayers(x) == 1) {
				hazeBand <- names(x)        
			} else {
				stop("Please specify the band from which you want to estimate the haze dn")
			}	
			if(is.numeric(hazeBand)) hazeBand <- names(x)[hazeBand]
		}
		
	} else {
		
		if(is.numeric(hazeBand)) hazeBand <- names(x$table)[hazeBand]
		preCalcAvail <- hazeBand %in% names(x$table)
		if(!any(preCalcAvail)) 	stop("Cannot estimate SHV because tables are missing for all specified bands", call. = FALSE)
		
		if(any(!preCalcAvail)) {
			warning(paste0("Cannot estimate SHV for >> ", hazeBand[!preCalcAvail], " << because tables are missing."), call. = FALSE)
			hazeBand <- hazeBand[preCalcAvail] 				
		}	
	}
	
	## Decide whether we open multiple devices
	multiple <- if(length(hazeBand) > 1) TRUE else FALSE
	
	## Run estimation for each band separately
	out   <- lapply(hazeBand, function(bi) {
				if(inherits(x, "Raster")) {
					tf <- freq(x[[bi]], useNA = "no") 
				} else {
					if(is.list(x) & "table" %in% names(x)) {
						preCalc <- TRUE
						tf <- x$table[[bi]]
					} else {
						stop("x must be a Raster* object or the result of a previous run of estimateSHV() with argument 'returnTables = TRUE'", call. = FALSE)
					}
				}
				tf <- tf[tf[,1] > 0,]
				tf[,2] <- tf[,2]/sum(tf[,2])
				dtf <- c(diff(tf[,2]),0) / c(diff(tf[,1]),0)
				
				SHV <- tf[which(dtf > darkProp)[1], 1] 
				if(is.na(SHV)) warning(paste("darkProp for band", bi, "was chosen too high. It exceeds the value range."), call. = FALSE)
				
				if(plot){
					if(multiple) x11()
					par(mfrow = c(1,2))
					
					plot(tf, xlab = "DN", ylab = "Frequency", type = "l", main = bi)
					abline(v = tf[tf[,1]==SHV,1], col="red")
					text(SHV, max(tf[,2]), pos=4, label = paste0("SHV_DN = ", SHV), col ="red")
					
					plot(dtf, type="l", xlab = "DN", ylab = "diff(Frequency)", main = bi)
					abline(v = tf[tf[,1]==SHV,1], col="red")
					abline(h = darkProp, col = "#00000070", lty = 2)
					text(max(tf[,1]), darkProp, label = paste0("darkProp = ", darkProp), col = "#00000070")
					text(SHV, max(dtf, na.rm = TRUE), pos=4, label = paste0("SHV_DN = ", SHV), col ="red")
					
				}
				
				return(list(table = tf, SHV = SHV))
			})
	
	SHV <- unlist(sapply(out, "[", 2))
	names(SHV) <- hazeBand
	
	if(!preCalc){
		table <- sapply(out, "[", 1)
		names(table) <- hazeBand
	} else {
		table <- x$table
	}
	return( if(!returnTables) SHV else list(SHV=SHV, table = table))
}sitecode,path,row,zoi coverage
CSN,229,56,redundant
CSN,229,58,none
CSN,230,57,none
CAX,226,61,redundant
CAX,225,62,redundant
COU,3,68,redundant
COU,3,69,none
BIF,172,61,redundant
BBS,125,63,none
BBS,123,64,redundant
MAS,232,61,none
MAS,230,61,none
NNN,182,59,none
NNN,181,59,none
PSH,127,57,redundant
PSH,127,58,slight
VB,15,52,redundant
VB,14,53,redundant
YAN,6,68,none
RNF,159,74,redundant
RNF,159,75,slight
#' Perform SLC-off gap fill of Landsat 7 ETM+ image
#'
#' Calls GNSPI.pro IDL script by Xiaolin Zhu to fill gaps in SLC-off Landsat 7 
#' ETM+ image. The script requires \code{fill} to be a TM (Landsat 5) image.  
#' \code{slc_off} must be a Landsat 7 SLC-off image.
#'
#' If supplied, \code{timeseries} should be a list of TM images.  Performing 
#' gap fill using SLC-Off ETM+ images as the input is not yet supported.
#' 
#' Pixels in gaps, background, and/or clouds in \code{slc_off}, 
#' \code{input_image}, and the images in \code{timeseries} should be coded as 
#' 0.
#'
#' @importFrom tools file_path_sans_ext
#' @param slc_off the SLC-off Landsat 7 file to gap fill, as a \code{Raster*}
#' @param fill the first TM image to use to fill in the gaps, as a 
#' \code{Raster*}
#' @param timeseries a timeseries of TM images as \code{Raster*} objects to use 
#' as additional inputs to the gap fill algorithm (optional)
#' @param out_base path and base filename for the output file. The script will 
#' save the output files by appending "_GNSPI.envi" and 
#' "_GNSPI_uncertainty.envi" to this base filename.
#' @param ext file extension to use when and when saving output rasters 
#' (determines output file format). Must be supported by 
#' \code{\link{writeRaster}}.
#' @param algorithm the algorithm to use, as a string ("GNSPI_IDL" is currently 
#' the only supported algorithm)
#' @param sample_size the sample size of sample pixels
#' @param size_wind the maximum window size
#' @param class_num the estimated number of classes
#' @param DN_min the minimum DN value of the image
#' @param DN_max the maximum DN value of the image
#' @param patch_long the size of block, to process whole ETM scene, set to 1000
#' @param idl path to the IDL binary
#' @param verbose whether to print detailed status messages
#' @param overwrite whether to overwrite output files if they already exist
#' @return a list of two rasters: 1) "filled", the gap filled image, and 2) 
#' "uncertainty", the uncertainty image.
#' @export
#' @references Zhu, X., Liu, D., Chen, J., 2012. A new geostatistical approach 
#' for filling gaps in Landsat ETM+ SLC-off images. Remote Sensing of 
#' Environment 124, 49--60.
#' @examples
#' \dontrun{
#' slc_off <- brick(system.file('tests', 'testthat_idl', 'fill_gaps', 
#' 'TM20100429_toaR_gap', package='teamlucc'))
#' fill <- brick(system.file('tests', 'testthat_idl', 'fill_gaps', 
#' 'TM20100515_toaR', package='teamlucc'))
#' timeseries <- c(brick(system.file('tests', 'testthat_idl', 'fill_gaps', 
#' 'TM20100208_toaR', package='teamlucc')))
#' filled <- fill_gaps(slc_off, fill, timeseries)
#' }
fill_gaps <- function(slc_off, fill, timeseries=c(), out_base=NULL, ext="tif",
                      algorithm="GNSPI_IDL", sample_size=20, size_wind=12, 
                      class_num=4, DN_min=0.0, 
                      DN_max=1.0, patch_long=1000,
                      idl="C:/Program Files/Exelis/IDL83/bin/bin.x86_64/idl.exe",
                      verbose=FALSE, overwrite=FALSE) {
    if (!(class(slc_off) %in% c("RasterLayer", "RasterStack", "RasterBrick"))) {
        stop('slc_off must be a Raster* object')
    }
    if (!(class(fill) %in% c("RasterLayer", "RasterStack", "RasterBrick"))) {
        stop('fill must be a Raster* object')
    }
    if (nlayers(slc_off) != nlayers(fill)) {
        stop('number of layers in slc_off must match number of layers in fill')
    }
    compareRaster(slc_off, fill)
    if (!(algorithm %in% c('GNSPI_IDL'))) {
        stop('algorithm must be "GNSPI_IDL" - no other algorithms are supported')
    }

    ext <- gsub('^[.]', '', ext)

    for (timeseries_img in timeseries) {
        if (!(class(timeseries_img) %in% c("RasterLayer", "RasterStack", "RasterBrick"))) {
            stop('each timeseries image be a Raster* object')
        }
        if (nlayers(slc_off) != nlayers(timeseries_img)) {
            stop('number of layers in slc_off must match number of layers of each image in timeseries')
        }
        compareRaster(slc_off, timeseries_img)
    }

    if (is.null(out_base)) {
        out_base <- file_path_sans_ext(rasterTmpFile())
    } else {
        out_base <- normalizePath(out_base, mustWork=FALSE)
        if (!file_test('-d', dirname(out_base))) {
            stop('output folder does not exist')
        }
        if (!overwrite && file_test('-f', file.path(out_base, paste0('_GNSPI.', ext)))) {
            stop('output file already exists - use a different "out_base"')
        }
        if (!overwrite && file_test('-f', file.path(out_base, paste0('_GNSPI_uncertainty.', ext)))) {
            stop('output uncertainty file already exists - use a different "out_base"')
        }
    }
    
    if (algorithm == "GNSPI_IDL") {
        filled <- fill_gaps_idl(slc_off, fill, timeseries, out_base, 
                                sample_size, size_wind, class_num, DN_min, 
                                DN_max, patch_long, idl, algorithm, ext, 
                                verbose)
    } else {
        stop("Native R gap filling not yet supported")
    }

    return(filled)
}

fill_gaps_idl <- function(slc_off, fill, timeseries, out_base, sample_size, 
                          size_wind, class_num, DN_min, DN_max, patch_long, 
                          idl, algorithm, ext, verbose) {
    if (verbose) {
        warning('verbose=TRUE not supported when algorithm="GNSPI_IDL"')
    }

    script_path <- system.file("idl", "GNSPI.pro", package="teamlucc")
    if (!(file_test('-x', idl) || file_test('-f', idl))) {
        stop('IDL not found - check "idl" parameter')
    }

    if (!check_ENVI_IDL(idl)) {
        stop("Unable to load ENVI in IDL - do you have ENVI and IDL licenses, and ENVI >= 5.0?")
    }

    # Save proj4string and extend to ensure the same proj4string and extent is 
    # returned even if they are changed by IDL
    orig_proj <- proj4string(slc_off)
    orig_ext <- extent(slc_off)
    orig_datatype <- dataType(slc_off)[1]

    # Write in-memory rasters to files for hand off to IDL. The capture.output 
    # line is used to avoid printing the rasterOptions to screen as they are 
    # temporarily reset.
    dummy <- capture.output(def_format <- rasterOptions()$format)
    rasterOptions(format='ENVI')
    slc_off <- writeRaster(slc_off, rasterTmpFile(), datatype=dataType(slc_off)[1])
    slc_off_file <- filename(slc_off)
    fill <- writeRaster(fill, rasterTmpFile(), datatype=dataType(fill)[1])
    fill_file <- filename(fill)
    timeseries_files <- c()
    if (length(timeseries) == 0) {
        # Ensure timeseries_files equals an empty matrix, in IDL format
        timeseries_files <- list()
    } else {
        for (timeseries_img in timeseries) {
            timeseries_img <- writeRaster(timeseries_img, rasterTmpFile(), datatype=dataType(fill)[1])
            timeseries_files <- c(timeseries_files, filename(timeseries_img))
        }
    }
    temp_dir <- tempdir()
    dummy <- capture.output(rasterOptions(format=def_format))

    # Save IDL output to a temp folder - it will be copied over and saved with 
    # writeRaster later to ensure the extents and projection are not modified 
    # from those of the original files.
    temp_out_base <- file_path_sans_ext(rasterTmpFile())
    param_vals <- list(slc_off_file, fill_file, timeseries_files,
                       temp_out_base, sample_size, size_wind, class_num,
                       DN_min, DN_max, patch_long, temp_dir)
    param_names <- list('slc_off_file', 'input_file', 'timeseries_files', 
                        'out_base', 'sample_size', 'size_wind', 'class_num', 
                        'DN_min', 'DN_max', 'patch_long', 'temp_dir')
    idl_params <- mapply(format_IDL_param, param_names, param_vals)
    idl_params <- paste(idl_params, collapse='')

    script_dir <- dirname(script_path)
    idl_script <- tempfile(fileext='.pro')
    idl_cmd <- paste0('CD, "', script_dir, '"\n', idl_params, 'GNSPI,', 
                      paste(param_names, collapse=','), '\nexit')

    f <- file(idl_script, 'wt')
    writeLines(idl_cmd, f)
    close(f)

    idl_out <- system(paste(shQuote(idl), shQuote(idl_script)), intern=TRUE)

    log_file <- paste0(out_base, '_GNSPI_idllog.txt')
    idl_out <- gsub('\r', '', idl_out)
    f <- file(log_file, 'wt')
    writeLines(idl_out, f) 
    close(f)

    filled <- brick(paste0(temp_out_base, '_GNSPI.envi'))
    filled_out_file <- paste0(out_base, paste0('_GNSPI.', ext))
    proj4string(filled) <- orig_proj
    extent(filled) <- orig_ext
    filled <- writeRaster(filled, filename=filled_out_file, overwrite=TRUE, 
                          datatype=orig_datatype)

    uncertainty <- brick(paste0(temp_out_base, '_GNSPI_uncertainty.envi'))
    uncertainty_out_file <- paste0(out_base, paste0('_GNSPI_uncertainty.', ext))
    proj4string(uncertainty) <- orig_proj
    extent(uncertainty) <- orig_ext
    uncertainty <- writeRaster(uncertainty, filename=uncertainty_out_file, 
                               overwrite=TRUE, datatype=orig_datatype)

    return(list(filled=filled, uncertainty=uncertainty))
}
georef <-
function(target, tofix, maxdist = 1000, startx = 0, starty = 0)
{
    ## find best-match shift of tofix to target
    ## returns coefficients, but geoshift() must be used to 
    ## actually adjust the matrix/dataframe/SpatialGridDataFrame

    # minimum-path fitting of tofix matrix to target matrix

    target <- as.matrix(target)
    tofix <- as.matrix(tofix)

    # if startx or starty != 0, shift matrices initially
    # this is intended to get away from local minima
    if( startx != 0 | starty != 0) {
	padx <- pady <- max(abs(startx), abs(starty))
	target <- geoshift(target, padx, pady, 0, 0)
	tofix <- geoshift(tofix, padx, pady, startx, starty)
    }

    if(!all(dim(target) == dim(tofix))) stop("target and tofix must be the same size.\n")

    # initial configuration
    thisx <- thisy <- 0
    currrmse <- sqrt(sum((as.vector(target) - as.vector(tofix))^2, na.rm=TRUE) / (sum(!is.na(as.vector(target)) & !is.na(as.vector(tofix)))))
    prevrmse <- currrmse + 1
    maxx <- maxy <- 1
    newx <- newy <- 0
    initrmse <- currrmse

    while(currrmse < prevrmse) {
#        cat(newx, ", ", newy, ": ", currrmse, "\n")

        results <- matrix(NA, nrow=9, ncol=3)
        colnames(results) <- c("x", "y", "RMSE")

        target2 <- geoshift(target, maxx, maxy, 0,  0)
        target2 <- as.vector(target2)

        currrow <- 1

        for(x in seq(newx-1, newx+1, by=1)) {
            for(y in seq(newy-1, newy+1, by=1)) {
                tofix2 <- geoshift(tofix, maxx, maxy, x, y)
                tofix2 <- as.vector(tofix2)
                results[currrow, 1:2] <- c(x, y)
                results[currrow, 3] <- sqrt(sum((target2 - tofix2)^2, na.rm=TRUE) / (sum(!is.na(target2) & !is.na(tofix2))))
                currrow <- currrow + 1
            }
        }

        prevrmse <- currrmse
        currrmse <- min(results[, "RMSE"])

        newx <- results[results[,"RMSE"] == currrmse, "x"]
        newy <- results[results[,"RMSE"] == currrmse, "y"]
        maxx <- max(abs(newx-1), abs(newx+1))
        maxy <- max(abs(newy-1), abs(newy+1))
	
	# check to see if the loop should stop anyway
	if(abs(newx) > maxdist | abs(newy) > maxdist) currrmse <- 9999
    
    }
    list(shiftx=newx, shifty=newy, initrmse=initrmse, currrmse=currrmse)
}

geoshift <-
function(mat, padx, pady, shiftx, shifty, nodata=NA)
{

    results <- mat

    if(is.data.frame(mat)) mat <- as.matrix(mat)
    if(is.matrix(mat)) {
        # pad a matrix and slide it over a given amount
        newmat <- matrix(nodata, nrow=(nrow(mat) + 2 * padx), ncol=(ncol(mat) + 2 * pady))
        newmat[(shiftx + padx + 1):(nrow(mat) + shiftx + padx), (shifty + pady + 1):(ncol(mat) + shifty + pady)] <- mat
    }
    if(class(mat) == "SpatialGridDataFrame") {
        mat.data <- as.matrix(mat)
        mat.data <- geoshift(mat.data, padx, pady, shiftx, shifty, nodata=nodata)
        mat@data <- data.frame(as.vector(mat.data))

        # S4 class
        mat.grid <- mat@grid
        mat.grid@cellcentre.offset[1] <- mat.grid@cellcentre.offset[1] - (padx * mat.grid@cellsize[1])
        mat.grid@cellcentre.offset[2] <- mat.grid@cellcentre.offset[2] - (pady * mat.grid@cellsize[2])
        mat.grid@cells.dim[1] <- as.integer(mat.grid@cells.dim[1] + 2*padx)
        mat.grid@cells.dim[2] <- as.integer(mat.grid@cells.dim[2] + 2*pady)
        mat@grid <- mat.grid

## obsoleted by sp_0.9-94
#-#        # not S4 class
#-#        mat.coords <- coordinates(mat)
#-#        mat.coords[1, 1] <- mat.coords[1, 1] - (padx * mat.grid@cellsize[1])
#-#        mat.coords[2, 1] <- mat.coords[2, 1] + (padx * mat.grid@cellsize[1])
#-#        mat.coords[1, 2] <- mat.coords[1, 2] - (pady * mat.grid@cellsize[2])
#-#        mat.coords[2, 2] <- mat.coords[2, 2] + (pady * mat.grid@cellsize[2])
#-#        mat@coords <- mat.coords
         
        # not S4 class
        mat.bbox <- bbox(mat)
        mat.bbox[1, "min"] <- mat.bbox[1, "min"] - (padx * mat.grid@cellsize[1])
        mat.bbox[1, "max"] <- mat.bbox[1, "max"] + (padx * mat.grid@cellsize[1])
        mat.bbox[2, "min"] <- mat.bbox[2, "min"] - (pady * mat.grid@cellsize[2])
        mat.bbox[2, "max"] <- mat.bbox[2, "max"] + (pady * mat.grid@cellsize[2])
        mat@bbox <- mat.bbox
    }
    
    # return the same structure as the input values
    if(class(results) == "SpatialGridDataFrame")
        results <- mat
    else if(is.data.frame(results))
        results <- data.frame(newmat)
    else if(is.matrix(results))
        results <- newmat
    else # no result
        results <- NA
    
    results

}

#' Extract the band names listed in an ENVI format header file (.hdr)
#'
#' @export
#' @param hdr_file an ENVI format header file with a .hdr extension
#' @return A /code{list} of band names extracted from the /code{hdr_file}
get_band_names_from_hdr <- function(hdr_file) {
    txt <- readLines(hdr_file)
    line_num <- which(grepl('^band names', txt)) + 1
    band_names <- c()
    for (n in line_num:length(txt)) {
        band_names <- c(band_names, gsub('[,}][[:space:]]*$', '', txt[n]))
        if (grepl('}', txt[n])) {
            break
        }
    }
    return(band_names)
}
#' Generate a SpatialPolygonDataFrame of raster extents
#'
#' Also includes the filename associated with each raster object. Useful for 
#' providing the \code{dem_extents} argument to the 
#' \code{\link{auto_setup_dem}} function.
#'
#' @export
#' @importFrom maptools spRbind
#' @param rast_list a \code{Raster*} object, or \code{list} of\code{Raster*} objects
#' @return \code{SpatialPolygonDataFrame} with the extent of each raster object 
#' as a polygon, with a "filename" attribute giving the filename for the raster 
#' object from with each extent is derived.
get_extent_polys <- function(rast_list) {
    if (!is.list(rast_list)) rast_list <- list(rast_list)

    proj4strings <- lapply(rast_list, function(x) proj4string(x))
    if (!all(proj4strings == proj4strings[[1]])) {
        stop('every raster in rast_list must have the same projection')
    }

    extents <- lapply(rast_list, function(x) extent(x))
    filenames <- lapply(rast_list, function(x) filename(x))
    # Convert extents to a list of SpatialPolygons objects
    extent_sps_list <- lapply(extents, function(x) as(x, 'SpatialPolygons'))

    # Convert from list of SpatialPolygons objects to a single SpatialPolygons 
    # object
    extent_sps <- extent_sps_list[[1]]
    if (length(extent_sps_list) > 1) {
        for (n in 2:length(extent_sps_list)) {
            extent_sps <- spRbind(extent_sps, spChFIDs(extent_sps_list[[n]], 
                                                 as.character(n)))
        }
    }

    # Finally convert the SpatialPolygons object into a 
    # SpatialPolygonsDataFrame that also includes the filename of the raster 
    # associated with each extent polygon as an attribute
    extent_polys <- SpatialPolygonsDataFrame(extent_sps, 
                                             data=data.frame(filename=unlist(filenames)))
    proj4string(extent_polys) <- proj4strings[[1]]

    extent_polys$filename <- as.character(extent_polys$filename)

    return(extent_polys)
}
#' Extract a metadata item from a metadata file in GDAL PAM format
#'
#' GDAL PAM format metadata files end in ".aux.xml".
#'
#' @export
#' @importFrom raster extension
#' @importFrom XML xmlInternalTreeParse xpathApply xmlValue
#' @param x an image file that has an accompanying GDAL PAM format metadata 
#' file (ending in .aux.xml)
#' @param key a string giving the name of the metadata item to extract
#' @return The metadata item (as a string)
get_metadata_item <- function(x, key) {
    metadata_file <- paste0(x, '.aux.xml')
    if (!file.exists(metadata_file)) {
        stop(paste('Could not find metadata file', metadata_file))
    }
    doc <- xmlInternalTreeParse(metadata_file)
    xpath_exp <- paste0("//MDI[@key='", key, "']")
    value <- unlist(xpathApply(doc, xpath_exp, xmlValue))
    if (length(value) > 1) {
        stop('multiple elements found')
    }
    if (length(value) == 0) {
        stop('no elements found')
    }
    return(value)
}
#' Draw a random sample from a grid laid out on a RasterLayer or matrix
#'
#' This function is used to subsample a \code{RasterLayer} or \code{matrix} by 
#' dividing the dataset into a grid of \code{horizcells} x \code{vertcells}, 
#' and by then drawing a sample of size \code{nsamp} from within each grid 
#' cell.
#'
#' @export
#' @param x a matrix or RasterLayer to draw sample from
#' @param horizcells how many cells to break the raster in horizontally (over 
#' the columns)
#' @param vertcells how many cells to break the raster in vertically (over 
#' the rows)
#' @param nsamp how many samples to draw from each grid cell
#' @param rowmajor whether to return indices in row-major format (default is 
#' column-major). Row-major format is useful in conjunction with \code{Raster*} 
#' objects.
#' @param replace whether to sample with replacement (within each grid cell)
#' @return vector of sample indices
#' @note TODO: Recode in C++ for speed.
#' @examples
#' # Make a 100x100 matrix
#' x <- matrix(1:10000, nrow=100)
#' # Subsample the matrix by breaking it up into a 10 x 10 grid, and take 10
#' # random samples from each grid cell without replacement (these are the
#' # default parameters).
#' y <- gridsample(x)
gridsample <- function(x, horizcells=10, vertcells=10, nsamp=10, 
                       rowmajor=FALSE, replace=FALSE) {
    # horizstart is a vector of column numbers of the first column in each cell 
    # in the grid
    horizstart <- round(seq(1, ncol(x), ncol(x) / horizcells))
    # horizend is a vector of column numbers of the last column in each cell in 
    # the grid
    if (length(horizstart) > 1) {
        horizend <- c((horizstart - 1)[2:length(horizstart)], ncol(x))
    } else {
        horizend <- c(ncol(x))
    }
    # vertstart is a vector of row numbers of the first row in each cell in the 
    # grid
    vertstart <- round(seq(1, nrow(x), nrow(x) / vertcells))
    # vertend is a vector of row numbers of the last row in each cell in the
    # grid
    if (length(vertstart) > 1) {
        vertend <- c((vertstart - 1)[2:length(vertstart)], nrow(x))
    } else {
        vertend <- c(nrow(x))
    }
    # retvalues is a vector to store the sample values chosen from x
    # sampindices is a vector to store the column major indices of the locations 
    # of each sampled value in x
    sampindices <- vector('numeric', horizcells * vertcells * nsamp)
    # retval_index tracks position in the vector storing the sampled values 
    # returned from this function
    retval_index <- 1
    # cell1row is used in calculating the column-major index of the first entry 
    # (top left) of each grid cell
    cell1row <- 1
    for (vertcellnum in 1:length(vertstart)) {
        # cell1col is used in calculating the column-major index of the first 
        # entry (top left) of each grid cell
        cell1col <- 1
        # cell_nrows is the number of rows in this particular cell (cells may 
        # have varying numbers of rows due to rounding)
        cell_nrows <- vertend[vertcellnum] - vertstart[vertcellnum] + 1
        for (horizcellnum in 1:length(horizstart)) {
            # cell1colmajindex is the column-major index of the first (top 
            # left) value in this particular grid cell
            cell1colmajindex <- cell1row + nrow(x) * (cell1col - 1)
            # cell_ncols is the number of rows in this particular cell (cells 
            # may have varying numbers of columns due to rounding)
            cell_ncols <- horizend[horizcellnum] - horizstart[horizcellnum] + 1
            # cell_colmaj_indices is a matrix of column-major indices of the 
            # position of each value in this grid cell, within the larger 
            # matrix x
            cell_colmaj_indices <- matrix(rep(1:cell_nrows, cell_ncols),
                                          nrow=cell_nrows) +
                                   matrix(rep(seq(cell1colmajindex, by=nrow(x), 
                                                  length.out=cell_ncols), 
                                              cell_nrows), nrow=cell_nrows, 
                                          byrow=TRUE) - 1
            samp_indices <- sample(cell_colmaj_indices, nsamp, replace=replace)
            sampindices[retval_index:(retval_index + length(samp_indices) - 1)] <- samp_indices
            retval_index <- retval_index + length(samp_indices)
            cell1col  <- cell1col + cell_ncols
        }
        cell1row <- cell1row + cell_nrows
    }
    if (rowmajor) {
        sampindices <- ((sampindices - 1) %% nrow(x)) * ncol(x) + ((sampindices - 1) %/% nrow(x) + 1)
    }
    return(sampindices)
}
histmatch <-
function(master, tofix, mask, minval=0, maxval=255, by=1)
{
	# simple histogram matching function
    # mask should contain NA for values to use; all other values will be omitted
	results <- tofix # want to return results in same format
	master <- as.vector(as.matrix(master))
	tofix <- as.vector(as.matrix(tofix))

    if(missing(mask)) mask <- rep(NA, length(master))
    else mask <- as.vector(as.matrix(mask))
    results.final <- rep(NA, length(mask))

    master <- master[is.na(mask)]
    tofix <- tofix[is.na(mask)]

	breaks <- seq(minval, maxval, by=by)
	master.cdf <- hist(master, breaks=breaks, plot=FALSE) 
	master.cdf <- c(0, cumsum(master.cdf$counts/sum(master.cdf$counts)))
	tofix.cdf <- hist(tofix, breaks=breaks, plot=FALSE) 
	tofix.cdf <- c(0, cumsum(tofix.cdf$counts/sum(tofix.cdf$counts)))

    # fixed 2012-07-16 to work with continuous data
    # originally written to work with integer data
	results.recode <- breaks
    results.values <- rep(NA, length(tofix))
    # original #	for(i in 1:length(breaks)) {
    # original #        testvals <- breaks[master.cdf < tofix.cdf[i]]
    # original #        if(length(testvals) > 0)
    # original #            results.recode[i] <- max(testvals)
    # original #        results.values[tofix == breaks[i]] <- results.recode[i]
    # original #    }

    for (i in 2:length(breaks)) {
        testvals <- breaks[master.cdf < tofix.cdf[i]]
        if (length(testvals) > 0) 
            results.recode[i] <- max(testvals)
        results.values[tofix > breaks[i-1] & tofix <= breaks[i]] <- results.recode[i]
    }

    results.final[is.na(mask)] <- results.values

    if(class(results) == "SpatialGridDataFrame")
        results@data[,1] <- results.final
    else if(is.data.frame(results))
        results <- data.frame(matrix(results.final, nrow=nrow(results), ncol=ncol(results)))
    else if(is.matrix(results))
        results <- matrix(results.final, nrow=nrow(results), ncol=ncol(results))
    else
        results <- results.final

    list(recode=results.recode, newimage=results)
}

sitecode,path,row
BBS,124,63
BBS,124,64
BCI,12,53
BCI,12,54
BIF,173,60
BIF,173,61
CAX,225,61
COU,4,68
CSN,229,57
KRP,187,56
KRP,187,57
MAS,231,61
MAS,231,62
MAS,230,62
NAK,128,47
NAK,127,47
NAK,127,48
NNN,182,58
PSH,126,58
RNF,158,75
UDZ,168,65
UDZ,168,66
UDZ,167,65
UDZ,167,66
VB,15,53
YAN,7,67
YAN,6,67
YAS,9,60
YAS,9,61
YAS,8,61
#' Estimates Earth-Sun distance (in AU) for a given date 
#' 
#' Function taken from the landsat package: S. Goslee (2012)
#' 
#' @param adate character. date in format "YYYY-MM-DD"
#' @keywords internal
.ESdist <- function(adate){	
	edist <- julian(as.Date(adate), origin=as.Date(paste(substring(adate, 1, 4), "12", "31", sep="-")))[[1]]
	 1 - 0.016729 * cos((2*pi) * (0.9856 * (edist - 4)/360))
}


#' Extract numbers from strings
#' 
#' @param x string or vector of strings
#' @param returnNumeric logical. should results be formatted \code{as.numeric}? If so, "05" will be converted to 5. Set returnNumeric to \code{FALSE} to keep preceeding zeros.
#' @note decimal numbers will be returned as two separate numbers
#' @keywords internal
.getNumeric <- function(x, returnNumeric = TRUE) {
	sapply(x, function(xi){
				d <- strsplit(xi, "[^[:digit:]]")[[1]]
				d <- if(returnNumeric) as.numeric(d[d!=""]) else d[d!=""]
				d
			})
}

.do_stretch <- function(x, pct, max_val) {
    lower <- quantile(x, prob=0 + pct/100, na.rm=TRUE)
    upper <- quantile(x, prob=1 - pct/100, na.rm=TRUE)
    x[x > upper] <- upper
    x[x < lower] <- lower
    x <- ((x - lower) / (upper-lower)) * max_val
    return(x)
}

#' Apply a linear stretch to an image
#'
#' Applies a linear stretch to an image (default linear 2% stretch), and 
#' returns the image with each band individually stretched and rescaled to 
#' range between zero and \code{max_val} (default of \code{max_val} is 1).
#'
#' @export
#' @param x image to stretch
#' @param pct percent stretch
#' @param max_val maximum value of final output (image will be rescaled to 
#' range from 0 - \code{max_val})
#' @return image with stretch applied
linear_stretch <- function(x, pct=2, max_val=1) {
    # Applies linear stretch (2 percent by default). Assumes image is arranged 
    # with bands in columns. Returns the image with stretch applied and bands 
    # rescaled to range from 0 - max_val.
    if ((pct < 0) | pct >= 50) {
        stop('pct must be > 0 and < 50')
    }
    if (class(x) %in% c('RasterLayer')) {
        x <- setValues(x, .do_stretch(getValues(x), pct, max_val))
        return(x)
    } else if (class(x) %in% c('RasterStack', 'RasterBrick')) {
        for (n in 1:nlayers(x)) {
            x <- setValues(x, .do_stretch(getValues(raster(x, layer=n)),
                                          pct, max_val), layer=n)
        }
        return(x)
    } else if (is.null(dim(x)) || (length(dim(x)) == 2) || (dim(x)[3] == 1)) {
        # Handle a vector, matrix, or n x m x 1 array
        return(.do_stretch(x, pct, max_val))
    }
}
#' Catalog a folder of Landsat images
#'
#' This function is used to produce a \code{data.frame} of Landsat images 
#' stored locally after download from the USGS. The images should be in a 
#' series of subfolders named following the naming scheme of 
#' \code{\link{espa_extract}}.
#'
#' @export
#' @importFrom stringr str_extract
#' @param in_folder path to a folder of Landsat surface reflectance images (for 
#' example, as extracted by the \code{espa_extract} function).
#' @return a \code{data.frame} with a list of the Landsat images found within 
#' in_folder
ls_catalog <- function(in_folder) {
    if (!file_test('-d', in_folder)) {
        stop(paste(in_folder, 'does not exist'))
    }
    out <- c()
    for (outer_item in dir(in_folder)) {
        outer_item_full <- file.path(in_folder, outer_item)
        # First cycle through the yearly folders
        if (!file_test('-d', outer_item_full)) {
            next
        }
        for (inner_item in dir(outer_item_full)) {
            inner_item_full <- file.path(outer_item_full, inner_item)
            # Check to ensure inner item is a Landsat HDF file
            if (!file_test('-f', inner_item_full) ||
                !grepl('^(lndsr.)?((LT4)|(LT5)|(LE7)|(LC8))[0-9]{13}[A-Z]{3}[0-9]{2}.hdf$', 
                       inner_item)) {
                next
            }
            metadata_string <- str_extract(inner_item, 
                                           '((LT4)|(LT5)|(LE7)|(LC8))[0-9]{13}')
            if (grepl('^LT4', metadata_string)) {
                sensor <- 'LT4'
            } else if (grepl('^LT5', metadata_string)) {
                sensor <- 'LT5'
            } else if (grepl('^LE7', metadata_string)) {
                sensor <- 'LE7'
            } else if (grepl('^LC8', metadata_string)) {
                sensor <- 'LC8'
            } else {
                message(paste('Skipping', inner_item,
                              '- cannot determine sensor from filename.'))
                next
            }
            year <- substr(metadata_string, 10, 13)
            julian_day <- substr(metadata_string, 14, 16)
            img_path <- substr(metadata_string, 4, 6)
            img_row <- substr(metadata_string, 7, 9)
            img_date <- as.Date(paste0(year, julian_day), '%Y%j')
            out <- c(out, list(list(img_path,
                                    img_row,
                                    year,
                                    julian_day,
                                    sensor,
                                    format(img_date, '%m'), 
                                    format(img_date, '%d'),
                                    inner_item)))

        }
    }
    if (is.null(out)) {
        stop('no images found')
    }
    out <- data.frame(matrix(unlist(out), nrow=length(out), byrow=T))
    names(out) <- c('path', 'row', 'year', 'julian', 'sensor',
                    'month', 'day', 'filename')
    out <- out[order(out$path, out$row, out$year, out$julian, out$sensor), ]
    return(out)
}
lssub <-
function(filename, outname, centerx, centery, centerepsg, widthx, widthy)
{

### subset a landsat image 

    ## get information about the landsat image
    ## assuming that gdalinfo always provides the same format
    lsinfo <- system(paste("gdalinfo ", filename, sep=""), intern=TRUE)

    lsorigin <- lsinfo[23]
    lsorigin <- strsplit(lsorigin, " ")[[1]][3]
    lsorigin <- gsub("\\(", "", lsorigin)
    lsorigin <- gsub("\\)", "", lsorigin)
    lsorigin <- as.numeric(strsplit(lsorigin, ",")[[1]])

    lspixelsize <- lsinfo[24]
    lspixelsize <- strsplit(lspixelsize, " ")[[1]][4]
    lspixelsize <- gsub("\\(", "", lspixelsize)
    lspixelsize <- gsub("\\)", "", lspixelsize)
    lspixelsize <- as.numeric(strsplit(lspixelsize, ",")[[1]])
    lspixelsize <- abs(lspixelsize)

    lsepsg <- lsinfo[22]
    lsepsg <- strsplit(lsepsg, '"')[[1]][4]

    # reproject target point if necessary
    if(!missing(centerepsg)) {
        if(centerepsg != lsepsg) {
            cat("reprojecting...\n")
            newcenter <- system(paste("echo ", centerx, " ", centery, " | gdaltransform -s_srs EPSG:", centerepsg, " -t_srs EPSG:", lsepsg, 
                sep=""), intern=TRUE)
            newcenter <- as.numeric(strsplit(newcenter, " ")[[1]][1:2])
            centerx <- newcenter[1]
            centery <- newcenter[2]
        }
    }

    # shift target point the minimum necessary to match pixel boundaries
    shiftval <- centerx - lsorigin[1]
    shiftval <- ((shiftval / lspixelsize[1]) - floor(shiftval / lspixelsize[1])) * lspixelsize[1]
    if(shiftval != 0) {
        #    if(shiftval < lspixelsize[1]) shiftval <- shiftval - lspixelsize[1]
        centerx <- centerx - shiftval
    }
    shiftval <- centery - lsorigin[2]
    shiftval <- ((shiftval / lspixelsize[2]) - floor(shiftval / lspixelsize[2])) * lspixelsize[2]
    if(shiftval != 0) {
        #    if(shiftval < lspixelsize[2]) shiftval <- shiftval - lspixelsize[2]
        centery <- centery - shiftval
    }

    system(paste("gdal_translate -projwin ", centerx - (lspixelsize[1]*widthx), " ", centery + (lspixelsize[2]*widthy), " ", centerx + (lspixelsize[1]*widthx), " ", centery - (lspixelsize[2]*widthy), " ", filename, " ", outname, sep=""), intern=TRUE)

    invisible()

}

#' Match the coordinate system and extent of two rasters
#'
#' @export
#' @param baseimg A /code{Raster*} to use as the base image. This layer will 
#' determine the output coordinate system.
#' @param matchimg A /code{Raster*} to match to the base image. If necessary 
#' the /code{matchimg} will be reprojected to match the coordinate system of 
#' the /code{baseimg}. The /code{matchimg} will then be cropped and extended to 
#' match the extent of the /code{baseimg}.
#' @param filename file on disk to save \code{Raster*} to (optional)
#' @param method the method to use if projection is needed to match image 
#' coordinate systems, or if resampling is needed to align image origins. Can 
#' be "ngb" for nearest-neighbor, or "binlinear" for bilinear interpolation
#' @param ... additional arguments to pass to \code{writeRaster} (such as 
#' datatype and filename)
#' @return The /code{matchimg} reprojected (if necessary), cropped, and 
#' extended to match the /code{baseimg}.
#' @details Note that \code{match_rasters} can run in parallel if 
#' \code{beginCluster()} is run prior to running \code{match_rasters}.
#' @examples
#' # Mosaic the two ASTER DEM tiles needed to a Landsat image
#' DEM_mosaic <- mosaic(ASTER_V002_EAST, ASTER_V002_WEST, fun='mean')
#' 
#' # Crop and extend the DEM mosaic to match the Landsat image
#' matched_DEM <- match_rasters(L5TSR_1986, DEM_mosaic)
match_rasters <- function(baseimg, matchimg, filename, method='bilinear',
                          ...) {
    if (projection(baseimg) != projection(matchimg)) {
        matchimg <- projectRaster(from=matchimg, to=baseimg, method=method)
    }

    # Crop overlapping area
    matchimg <- crop(matchimg, baseimg)
    if (any(res(matchimg) != res(baseimg))) {
        matchimg <- resample(matchimg, baseimg, method=method)
    }
    if (extent(matchimg) != extent(baseimg)) {
        matchimg <- extend(matchimg, baseimg)
    }

    if (!missing(filename)) {
            matchimg <- writeRaster(matchimg, filename=filename, ...)
    }
    return(matchimg)
}

minnaert <- function(x, slope, aspect, sunelev, sunazimuth, na.value=NA, GRASS.aspect=FALSE, IL.epsilon=0.000001, slopeclass = c(1, 5, 10, 15, 20, 25, 30, 45), coverclass)
{
# pixel-based Minnaert correction
# Lu et al. 2008. PE&RS 74:1343-1350.
# topographic correction for image x based on
# topography and sun location
# require(mgcv)

# IL.epsilon: if IL == 0, the corrected value is Inf (division by zero)
# adding a tiny increment eliminates the Inf

## aspect may be GRASS output: counterclockwise from east
## or nonGRASS output: clockwise from north
## require the latter for further calculations
## because sunazimuth is always measured clockwise from north
    if(GRASS.aspect) {
        aspect <- as.matrix(aspect)
        aspect <- -1 * aspect + 90
        aspect <- (aspect + 360) %% 360
    }

# all inputs are in degrees, but we need radians
    sloper <- (pi/180) * as.matrix(slope)
    sloped <- as.matrix(slope)
    aspect <- (pi/180) * as.matrix(aspect)
    sunzenith <- (pi/180) * (90 - sunelev)
    sunazimuth <- (pi/180) * sunazimuth

    x.orig <- x
    x <- as.matrix(x)
    x[x == na.value] <- NA

    IL <- cos(sloper) * cos(sunzenith) + sin(sloper) * sin(sunzenith) * cos(sunazimuth - aspect)
    IL[IL == 0] <- IL.epsilon

    if(missing(coverclass)) 
        coverclass <- rep(TRUE, length(as.vector(x)))

    ## Minnaert
    ## K is between 0 and 1
        # IL can be <=0 under certain conditions
        # but that makes it impossible to take log10 so remove those elements
    K <- data.frame(x = as.vector(x), IL = as.vector(IL), slope=as.vector(sloped))
    K <- K[coverclass, ]
    K <- K[!apply(K, 1, function(x)any(is.na(x))),]
    K <- K[K$x > 0, ]
    K <- K[K$IL > 0, ]

    ## calculate overall K value; only use points with greater than 5% slope
    targetslope <- (180/pi) * atan(.05)
    allcoef <- coefficients(lm(log10(K$x)[K$slope >= targetslope] ~ log10(K$IL/cos(sunzenith))[K$slope >= targetslope]))[[2]]

    results <- data.frame(matrix(0, nrow=length(slopeclass)-1, ncol=3))
    colnames(results) <- c("midpoint", "n", "k")
    results[,1] <- diff(slopeclass)/2 + slopeclass[1:length(slopeclass)-1]

    K.cut <- as.numeric(cut(K$slope, slopeclass)) # don't use slopes outside slopeclass range
    if(nrow(results) != length(table(K.cut))) stop("slopeclass is inappropriate for these data (empty classes)\n")
    results[,2] <- table(K.cut)

    #            sapply(unique(K.cut), function(i)coefficients(lm(log10(K$x)[K.cut == i] ~ log10(K$IL/cos(sunzenith))[K.cut == i]))[[2]])
   for(i in sort(unique(K.cut[!is.na(K.cut)]))) {
        results[i, 3] <- coefficients(lm(log10(K$x)[K.cut == i] ~ log10(K$IL/cos(sunzenith))[K.cut == i]))[[2]]
    }

    model <- with(results, gam(k ~ s(midpoint, k=length(midpoint)-1)))

    K.all <- data.frame(midpoint = as.vector(as.matrix(slope)))
    K.all[K.all > max(slopeclass)] <- max(slopeclass) # if slope is greater than modeled range, use maximum of modeled range
    K.all[K.all < min(slopeclass)] <- 0 # if slope is less than modeled range, treat it as flat
    K.all <- predict(model, newdata=K.all)
    K.all[K.all > 1] <- 1
    K.all[K.all < 0] <- 0

    xout <- as.vector(as.matrix(x)) * (cos(sunzenith)/as.vector(as.matrix(IL))) ^ K.all
    xout[K.all == 0 & !is.na(K.all)] <- as.vector(as.matrix(x))[K.all == 0 & !is.na(K.all)] # don't correct flat areas

    ## if x was a SpatialGridDataFrame, return an object of the same class
    if(class(x.orig) == "SpatialGridDataFrame") {
        x.orig@data[,1] <- as.vector(xout)
        xout <- x.orig
    }

    list(allcoef=allcoef, classcoef=results, model=model, minnaert=xout)

}

#' @import mgcv
.calc_k_table <- function(x, IL, slope, sampleindices, slopeclass,
                          coverclass, sunzenith) {
    if (!is.null(sampleindices)) {
        K <- data.frame(x=x[sampleindices],
                        IL=IL[sampleindices], 
                        slope=slope[sampleindices])
        # Remember that the sample indices are row-major (as they were drawn 
        # for a RasterLayer), so the coverclass matrix needs to be transposed 
        # as it is stored in column-major order
        if(!is.null(coverclass)) coverclass <- t(coverclass)[sampleindices]
    } else {
        K <- data.frame(x=getValues(x), IL=getValues(IL), 
                        slope=getValues(slope))
    }

    if(!is.null(coverclass)) {
        K <- K[coverclass, ]
    }

    ## K is between 0 and 1
    # IL can be <=0 under certain conditions
    # but that makes it impossible to take log10 so remove those elements
    K <- K[!apply(K, 1, function(rowvals) any(is.na(rowvals))), ]
    K <- K[K$x > 0, ]
    K <- K[K$IL > 0, ]

    k_table <- data.frame(matrix(0, nrow=length(slopeclass) - 1, ncol=3))
    names(k_table) <- c('midpoint', 'n', 'k')
    k_table$midpoint <- diff(slopeclass)/2 + slopeclass[1:length(slopeclass) - 1]

    # don't use slopes outside slopeclass range
    K.cut <- as.numeric(cut(K$slope, slopeclass))
    if(nrow(k_table) != length(table(K.cut))) {
        stop("slopeclass is inappropriate for these data (empty classes)\n")
    }
    k_table$n <- table(K.cut)

    for(i in sort(unique(K.cut[!is.na(K.cut)]))) {
        k_table$k[i] <- coefficients(lm(log10(K$x)[K.cut == i] ~ 
                                        log10(K$IL/cos(sunzenith))[K.cut == i]))[[2]]
    }

    return(k_table)
}

# Function to combine classes defined by the upper limits 'lims' with their 
# smallest neighbors until each class has at least n members. 'counts' stores 
# the number of members in each class.
clean_intervals <- function(counts, lims, n) {
    while(min(counts) < n) {
        # The "rev" below is so that the classes at the end are combined first 
        # (as classes with higher slopes are more more likely to be more rare 
        # and therefore have fewer members)
        min_index <- length(counts) - match(TRUE,
                                            rev(counts == min(counts))) + 1
        if (min_index == length(counts)) {
            counts[min_index - 1] <- counts[min_index - 1] + counts[min_index]
            lims <- lims[-(min_index - 1)]
        } else if (min_index == 1) {
            counts[min_index + 1] <- counts[min_index + 1] + counts[min_index]
            lims <- lims[-min_index]
        } else if (counts[min_index - 1] < counts[min_index + 1]) {
            counts[min_index - 1] <- counts[min_index - 1] + counts[min_index]
            lims <- lims[-(min_index - 1)]
        } else {
            # We know counts[min_index - 1] > counts[min_index + 1]
            counts[min_index + 1] <- counts[min_index + 1] + counts[min_index]
            lims <- lims[-min_index]
        }
        counts <- counts[-min_index]
    }
    return(lims)
}

#' Topographic correction for satellite imagery using Minnaert method
#'
#' Perform topographic correction using the Minnaert method. This code is 
#' modified from the code in the \code{landsat} package written by Sarah 
#' Goslee.  This version of the code has been altered from the \code{landsat} 
#' version to allow the option of using a sample of pixels for calculation of k 
#' in the Minnaert correction (useful when dealing with large images).
#' 
#' See the help page for \code{minnaert} in the \code{landsat} package for 
#' additional details on the parameters.
#'
#' @export
#' @param x image as a \code{RasterLayer}
#' @param slope the slope in radians as a \code{RasterLayer}
#' @param aspect the aspect in radians as a \code{RasterLayer}
#' @param sunelev sun elevation in degrees
#' @param sunazimuth sun azimuth in degrees
#' @param IL.epsilon a small amount to add to calculated illumination values 
#' that are equal to zero to avoid division by zero resulting in Inf values
#' @param slopeclass the slope classes to calculate k for (in radians), or 
#' NULL, in which case an algorithm will be used to choose reasonable defaults 
#' for the given image. If provided, \code{slopeclass} should be a list of 
#' slope class limits. For example: c(1, 5, 10, 15, 20, 25, 30, 45) * (pi/180)
#' @param coverclass used to calculate k for specific cover class (optional)
#' as \code{RasterLayer}
#' @param sampleindices (optional) row-major indices of sample pixels to use in 
#' the calculation of k values for the Minnaert correction. See
#' \code{\link{gridsample}}.
#' @param DN_min minimum allowable pixel value after correction (values less 
#' than \code{DN_min} are set to NA)
#' @param DN_max maximum allowable pixel value after correction (values less 
#' than \code{DN_max} are set to NA)
#' @return \code{RasterLayer} with topographically corrected data
#' @author Sarah Goslee and Alex Zvoleff
#' @references
#' Sarah Goslee. Analyzing Remote Sensing Data in {R}: The {landsat} Package.  
#' Journal of Statistical Software, 2011, 43:4, pg 1--25.  
#' http://www.jstatsoft.org/v43/i04/
minnaert_samp <- function(x, slope, aspect, sunelev, sunazimuth,
                          IL.epsilon=0.000001, slopeclass=NULL, 
                          coverclass=NULL, sampleindices=NULL, DN_min=NULL, 
                          DN_max=NULL) {

    if (is.null(slopeclass)) {
        slopeclass <- c(1, 2, 3, 4, 5, 6, 8, 10, 12,
                        15, 20, 25, 30, 45, 75) * (pi/180)
    }

    if (is.null(sampleindices)) {
        counts <- raster::freq(raster::cut(slope, slopeclass,
                                           include.lowest=TRUE), 
                               useNA='no')
        # Eliminate empty bins:
        slopeclass <- slopeclass[c(TRUE, 1:(length(slopeclass) - 1) %in% counts[, 1])]
        counts <- counts[, 2]
    } else {
        counts <- as.numeric(table(cut(slope[sampleindices], slopeclass, 
                                       include.lowest=TRUE), useNA='no'))
    }
    # The [-1] below is because clean_intervals only needs the upper limits
    slopeclass <- clean_intervals(counts, slopeclass[-1], 100)
    if (length(slopeclass) <= 5) {
        stop('insufficient sample size to develop k model - try changing slopeclass or sampleindices')
    }
    slopeclass <- c(1*pi/180, slopeclass)

    stopifnot(all((slopeclass >= 0) & slopeclass <= pi/2))

    # some inputs are in degrees, but we need radians
    stopifnot((sunelev >= 0) & (sunelev <= 90))
    stopifnot((sunazimuth >= 0) & (sunazimuth <= 360))
    sunzenith <- (pi/180) * (90 - sunelev)
    sunazimuth <- (pi/180) * sunazimuth

    IL <- .calc_IL(slope, aspect, sunzenith, sunazimuth, IL.epsilon)
    rm(aspect, sunazimuth)

    k_table <- .calc_k_table(x, IL, slope, sampleindices, slopeclass, 
                             coverclass, sunzenith)
    
    k_model <- with(k_table, bam(k ~ s(midpoint, k=length(midpoint) - 1), data=k_table))

    # If slope is greater than modeled range, use maximum of modeled range. If 
    # slope is less than modeled range, treat it as flat.
    slopeclass_max <- max(slopeclass)
    slopeclass_min <- min(slopeclass)
    slope <- calc(slope,
                  fun=function(vals) {
                      vals[vals > slopeclass_max] <- slopeclass_max
                      vals[vals < slopeclass_min] <- 0
                      return(vals)
                  })

    names(slope) <- 'midpoint'
    K.all <- predict(slope, k_model)
    K.all <- calc(K.all,
                  fun=function(vals) {
                      vals[vals > 1] <- 1
                      vals[vals < 0] <- 0
                      return(vals)
                  })

    # Perform correction
    xout <- overlay(x, IL, K.all,
                    fun=function(x_vals, IL_vals, K.all_vals) {
                        x_vals * (cos(sunzenith)/IL_vals) ^ K.all_vals
                    })
    # Don't correct flat areas
    xout[K.all == 0 & !is.na(K.all)] <- x[K.all == 0 & !is.na(K.all)]

    if ((!is.null(DN_min)) || (!is.null(DN_max))) {
        xout <- calc(xout, fun=function(vals) {
                        if (!is.null(DN_min)) vals[vals < DN_min] <- NA
                        if (!is.null(DN_max)) vals[vals > DN_max] <- NA
                        return(vals)
                     })
    }

    list(classcoef=k_table, model=k_model, minnaert=xout, sampleindices=sampleindices)
}
movingwindow <-
function(x, kernel)
{
    # apply kernel as a moving window to x
    results <- x
    x <- as.matrix(x)

    mwoffset <- (nrow(kernel)-1)/2

    newmat <- matrix(NA, nrow=nrow(x), ncol=ncol(x))

    for(i in (1+mwoffset):(nrow(x)-mwoffset)) {
        for(j in (1+mwoffset):(ncol(x)-mwoffset)) {
            newmat[i, j] <- sum(kernel * x[(i-mwoffset):(i+mwoffset), (j-mwoffset):(j+mwoffset)])
        }
    }

    # return the same structure as the input values
    if(class(results) == "SpatialGridDataFrame")
        results@data[,1] <- as.vector(newmat)
    else if(is.data.frame(results))
        results <- data.frame(matrix(newmat, nrow=nrow(results), ncol=ncol(results)))
    else if(is.matrix(results))
        results <- matrix(newmat, nrow=nrow(results), ncol=ncol(results))

    results
}

#' Normalizes two rasters
#'
#' Performs relative normalization on two rasters using model II regression. 
#' Based on the approach in the \code{relnorm} function in the \code{landsat} 
#' package.
#'
#' This function will run in parallel if a parallel backend is registered with 
#' \code{\link{foreach}}.
#'
#' @export
#' @importFrom iterators iter
#' @import foreach
#' @importFrom lmodel2 lmodel2
#' @param x a \code{Raster*} to use as the base image
#' @param y a \code{Raster*} to normalize to the base image
#' @param msk a \code{RasterLayer} with missing values in \code{x} or in {y} 
#' coded as 1, and all other values coded as 0 (optional)
#' @param method the regression method to use (must be a method recognized by 
#' \code{lmodel2}
#' @param size the number of pixels to use in developing the model
#' @return a \code{Raster*} of \code{y} normalized to \code{x}
#' @examples
#' L5TSR_2001_normed_1 <- normalize(L5TSR_1986, L5TSR_2001)
#' plotRGB(L5TSR_2001_normed_1, stretch='lin')
#'
#' # Use only half as many pixels to calculate the models
#' L5TSR_2001_normed_2 <- normalize(L5TSR_1986, L5TSR_2001, 
#'                                  size=ncell(L5TSR_1986)/2)
#' plotRGB(L5TSR_2001_normed_2, stretch='lin')
#' @references
#' Sarah Goslee. Analyzing Remote Sensing Data in {R}: The {landsat} Package.  
#' Journal of Statistical Software, 2011, 43:4, pg 1--25.  
#' http://www.jstatsoft.org/v43/i04/
normalize <- function(x, y, msk, method="MA", size=ncell(x)) {
    orig_datatype <- dataType(y)[1]
    compareRaster(x, y)
    stopifnot(nlayers(x) == nlayers(y))
    stopifnot(size <= ncell(x))
    if (!missing(msk)) {
        compareRaster(x, msk)
        stopifnot(nlayers(msk) == 1)
    }

    if (size < ncell(x)) {
        # Note that sampleRegular with cells=TRUE returns cell numbers in the 
        # first column
        x_vals <- sampleRegular(x, size=size, cells=TRUE)
        if (!missing(msk)) {
            x_vals <- x_vals[!(msk[x_vals[, 1]]), ]
        }
        y_vals <- y[x_vals[, 1]]
        x_vals <- x_vals[, -1]
    } else {
        x_vals <- getValues(x)
        y_vals <- getValues(y)
        if (!missing(msk)) {
            x_vals <- x_vals[!getValues(msk), ]
            y_vals <- y_vals[!getValues(msk), ]
        }
    }

    names(y_vals) <- names(x_vals)

    if (nlayers(y) > 1) {
        unnormed_layer <- x_sample <- y_sample <- NULL
        normed_y <- foreach(unnormed_layer=unstack(y),
                            x_sample=iter(x_vals, by='column'),
                            y_sample=iter(y_vals, by='column'),
                            .combine='addLayer', .multicombine=TRUE, 
                            .init=raster(),
                            .packages=c('raster', 'lmodel2', 'rgdal')) %dopar% {
            model <- suppressMessages(lmodel2(x_sample ~ y_sample, nperm=0))
            model <- model$regression.results[model$regression.results[, "Method"] == method, ]
            names(model) <- gsub("^ *", "", names(model))
            normed_layer <- model$Slope * unnormed_layer + model$Intercept
        }
    } else {
        model <- suppressMessages(lmodel2(x_vals ~ y_vals, nperm=0))
        model <- model$regression.results[model$regression.results[, "Method"] == method, ]
        names(model) <- gsub("^ *", "", names(model))
        normed_y <- model$Slope * y + model$Intercept
    }

    if (!missing(msk)) {
        # Copy masked values back into the output raster
        normed_y[msk] <- y[msk]
    }

    #dataType(normed_y) <- orig_datatype

    return(normed_y)
}
#' Make plot of image with overlaid polygon
#'
#' Useful for quick plots showing overlap between an area of interest (AOI) and 
#' a satellite image.
#'
#' @export
#' @importFrom grid unit
#' @import ggplot2
#' @param x image as a \code{Raster*} object
#' @param y polygon to overlay, as \code{SpatialPolygonDataFrame}
#' @param out filename for output image. The extension of this file will 
#' determine the output file format (png, pdf, etc.).
#' @param title title of plot
#' @param width width (in inches) of output image
#' @param height height (in inches) of output image
#' @param dpi DPI for output image
#' @param ... additional arguments to \code{ggsave}
#' @return used for writing plot to a file
overlay_poly <- function(x, y, out, title='', width=4, height=4, dpi=150,
                         ...) {
    stop('overlay_poly is not yet supported')
    if (!(nlayers(x) %in% c(1, 3))) {
        stop('x must be a one or three band image')
    }

    y <- spTransform(y, CRS(proj4string(x)))

    maxpixels <- width*height*dpi^2
    # Below is based on gplot from rasterVis package
    nl <- nlayers(x)
    if (ncell(x)/nl > maxpixels) {
        x <- sampleRegular(x, maxpixels*nl, ext=y, asRaster=TRUE)
    }

    x <- mask(x, y)

    coords <- xyFromCell(x, seq_len(ncell(x)))
    dat <- data.frame(linear_stretch(getValues(x)))
    dat <- dat[complete.cases(dat), ]
    coords <- coords[complete.cases(dat), ]
    if (nlayers(x) == 3) {
        dat <- rgb(dat)
    }
    dat <- data.frame(coords, color=dat)

    color=long=lat=NULL  # fix for R CMD CHECK
    theme_set(theme_bw(base_size=8))
    p <- ggplot(dat) +
        geom_raster(aes(x, y, fill=color)) + coord_fixed() +
        scale_fill_identity() +
        labs(title=title) +
        theme(axis.text.x=element_blank(), axis.text.y=element_blank(),
              axis.title.x=element_blank(), axis.title.y=element_blank(),
              panel.background=element_blank(), panel.border=element_blank(),
              panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
              plot.background=element_blank(), axis.ticks=element_blank(),
              plot.margin=unit(c(.1, .1, .1, .1), 'cm'))
    p
    ggsave(out, width=width, height=height, dpi=dpi, ...)
}
load_wrs_data <- function(wrs_type, wrs_mode) {
    if (wrs_type == 2) {
        wrs_polys <- wrs2_asc_desc
    } else if (wrs_type == 1) {
        wrs_polys <- wrs1_asc_desc
    } else {
        stop('wrs_type must be 1 or 2')
    }
    if (!(wrs_mode %in% c('D', 'A'))) {
        stop('wrs_mode must be "D", "A" or c("D", "A")')
    }
    return(wrs_polys[wrs_polys@data$MODE %in% wrs_mode, ])
}

#' @importFrom rgeos gIntersects gUnaryUnion
intersect_wrs_polys <- function(wrs_polys, x, as_polys) {
    intersecting <- as.logical(gIntersects(wrs_polys, gUnaryUnion(x), byid=TRUE))
    if (sum(intersecting) == 0) {
        stop('no intersecting pathrows found')
    } else {
        wrs_polys <- wrs_polys[intersecting, ]
        wrs_polys <- wrs_polys[order(wrs_polys$PATH, wrs_polys$ROW), ]
        if (!as_polys) {
            wrs_polys <- data.frame(PATH=wrs_polys@data$PATH, ROW=wrs_polys@data$ROW)
        }
        return(wrs_polys)
    }
}

#' Get WRS-2 path/row numbers for a given spatial object
#'
#' @export pathrow_num
#' @import methods
#' @import wrspathrowData
#' @param x a spatial object
#' @param wrs_type 1 (for WRS-1) or 2 (for WRS-2)
#' @param wrs_mode either 'D' for descending (daytime) or 'A' for ascending 
#' @param as_polys if FALSE (default) return a data.frame. If TRUE, return a 
#' \code{SpatialPolygonsDataFrame}.
#' @return data.frame with path and row as integers, or, if as_polys=TRUE, a 
#' \code{SpatialPolygonsDataFrame}
#' @examples
#' \dontrun{
#' library(sp)
#'
#' pathrow_num(test_poly)
#'
#' x <- pathrow_num(test_poly, as_polys=TRUE)
#' plot(x)
#' plot(test_poly, add=TRUE, lty=2, col="#00ff0050")
#' text(coordinates(x), labels=paste(x$PATH, x$ROW, sep=', '))
#' }
setGeneric("pathrow_num", function(x, wrs_type='2', wrs_mode='D', 
                                   as_polys=FALSE) {
    standardGeneric("pathrow_num")
})

#' @importFrom raster extent projectExtent crs
#' @importFrom rgeos gIntersects
#' @aliases pathrow_num,Raster-method
setMethod("pathrow_num", signature(x="Raster"),
    function(x, wrs_type, wrs_mode, as_polys) {
        wrs_polys <- load_wrs_data(wrs_type, wrs_mode)
        x_wgs84 <- projectExtent(x, crs=crs(wrs_polys))
        x_wgs84_sp <- as(extent(x_wgs84), 'SpatialPolygons')
        return(intersect_wrs_polys(wrs_polys, x_wgs84_sp, as_polys))
    }
)

#' @importFrom rgeos gIntersects
#' @importFrom sp CRS proj4string spTransform
#' @import rgdal
#' @aliases pathrow_num,Spatial-method
setMethod("pathrow_num", signature(x="Spatial"),
    function(x, wrs_type, wrs_mode, as_polys) {
        wrs_polys <- load_wrs_data(wrs_type, wrs_mode)
        x_wgs84 <- spTransform(x, CRS(proj4string(wrs_polys)))
        return(intersect_wrs_polys(wrs_polys, x_wgs84, as_polys))
    }
)
#' Get a polygon giving area of coverage of a given WRS-1 or WRS-2 path and row
#'
#' @export
#' @import wrspathrowData
#' @param wrs_path WRS-1 or WRS-2 path as an integer
#' @param wrs_row WRS-1 or WRS-2 row as an integer
#' @param wrs_type 1 (for WRS-1) or 2 (for WRS-2)
#' @param wrs_mode either 'D' for descending (daytime) or 'A' for ascending 
#' (nighttime)
#' @return \code{SpatialPolygonsDataFrame} with path and row polygon
#' @examples
#' library(sp)
#'
#' x <- pathrow_poly(225, 61)
#' plot(x)
pathrow_poly <- function(wrs_path, wrs_row, wrs_type='2', wrs_mode='D') {
    if (wrs_type == 2) {
        if (wrs_path < 1 || wrs_path > 233) {
            stop('WRS-2 paths range from 1 to 233')
        }
        if (wrs_row < 1 || wrs_row > 248) {
            stop('WRS-2 rows range from 1 to 248')
        }
    } else if (wrs_type == 1) {
        if (wrs_path < 1 || wrs_path > 251) {
            stop('WRS-1 paths range from 1 to 251')
        }
        if (wrs_row < 1 || wrs_row > 248) {
            stop('WRS-1 rows range from 1 to 248')
        }
    }
    wrs_polys <- load_wrs_data(wrs_type, wrs_mode)
    return(wrs_polys[wrs_polys@data$PATH == wrs_path &
                     wrs_polys@data$ROW == wrs_row, ])
}
PIF <-
function(band3, band4, band7, level=.99) {
# identify pseudo-invariant features after SSV1988
   
    if(is.character(band3)) {
        band3 <- read.asciigrid(band3)
        pifgrid <- band3
        band3 <- band3@data[,1]
    } else {
        pifgrid <- band3
        band3 <- as.vector(as.matrix(band3))
    } 
    
    if(is.character(band4)) {
        band4 <- read.asciigrid(band4)@data[,1]
    } else {
        band4 <- as.vector(as.matrix(band4))
    }

    if(is.character(band7)) {
        band7 <- read.asciigrid(band7)@data[,1]
    } else {
        band7 <- as.vector(as.matrix(band7))
    }

    band43 <- band4/band3
        
    band43.level <- quantile(band43, 1-level, na.rm=TRUE)
    band7.level <- quantile(band7, level, na.rm=TRUE)
    
    pifmask <- ifelse(band43 < band43.level & band7 > band7.level & band7 < 255, 1, 0)
    
    # return the same structure as the input values
    if(class(pifgrid) == "SpatialGridDataFrame")
        pifgrid@data[,1] <- pifmask
    else if(is.data.frame(pifgrid))
        pifgrid <- data.frame(matrix(pifmask, nrow=nrow(pifgrid), ncol=ncol(pifgrid)))
    else if(is.matrix(pifgrid))
        pifgrid <- matrix(pifmask, nrow=nrow(pifgrid), ncol=ncol(pifgrid))
    else # return a vector 
        pifgrid <- pifmask
    
    pifgrid
}

#' A class for representing training or testing data
#'
#' Used to represent training data for a machine learning classifier for image 
#' classificaion, or testing data used for testing a classification.
#'
#' @exportClass pixel_data
#' @rdname pixel_data-class
#' @aliases pixel_data
#' @slot x a \code{data.frame} of independent variables (usually pixel values)
#' @slot y a \code{data.frame} of the dependent variable (usually land cover 
#' classes)
#' @slot pixel_src a data.frame used to link pixels in \code{x} and \code{y} to 
#' an input polygon
#' @slot training_flag a binary vector of length equal to \code{nrow(x)} 
#' indicating each row in x should be used in training (TRUE) or in testing 
#' (FALSE)
#' @slot polys a \code{SpatialPolygonsDataFrame} of the polygons used to choose 
#' the pixels in \code{x} and \code{y}.
#' @import methods
#' @importFrom sp SpatialPolygonsDataFrame
setClass('pixel_data', slots=c(x='data.frame', y='factor', 
                               pixel_src='data.frame', training_flag='logical', 
                               polys='SpatialPolygonsDataFrame')
)

#' @export
#' @method summary pixel_data
summary.pixel_data <- function(object, ...) {
    obj = list()
    obj[['class']] <- class(object)
    obj[['n_classes']] <- nlevels(object)
    obj[['n_sources']] <- length(unique(object@polys$src))
    obj[['n_polys']] <- nrow(object@polys)
    obj[['n_pixels']] <- nrow(object@x)
    training_df <- data.frame(y=object@y,
                              pixel_src=src_name(object), 
                              training_flag=object@training_flag)
    y=pixel_src=training_flag=NULL # Keep R CMD CHECK happy
    class_stats <- summarize(group_by(training_df, y),
                             n_polys=length(unique(pixel_src)),
                             n_train_pixels=sum(training_flag),
                             n_test_pixels=sum(!training_flag),
                             train_frac=round(sum(training_flag) / 
                                              length(training_flag), 2))
    names(class_stats)[names(class_stats) == 'y'] <- 'class'
    obj[['class_stats']]  <- class_stats
    obj[['n_training']] <- sum(object@training_flag == TRUE)
    obj[['n_testing']] <- sum(object@training_flag == FALSE)
    obj[['training_frac']] <- sum(object@training_flag == TRUE) / length(object@training_flag)
    class(obj) <- 'summary.pixel_data'
    obj
}

#' @export
#' @method print summary.pixel_data
print.summary.pixel_data <- function(x, ...) {
    cat(paste('Object of class "', x[['class']], '"\n', sep = ''))
    cat('\n')
    cat(paste('Number of classes:\t', x[['n_classes']], '\n', sep=''))
    cat(paste('Number of polygons:\t', x[['n_polys']], '\n', sep=''))
    cat(paste('Number of pixels:\t', x[['n_pixels']], '\n', sep=''))
    cat(paste('Number of sources:\t', x[['n_sources']], '\n', sep=''))
    cat('\n')
    cat('Training data statistics:\n')
    print(x[['class_stats']])
    cat('\n')
    cat(paste('Number of training samples:\t', x[['n_training']], '\n', sep=''))
    cat(paste('Number of testing samples:\t', x[['n_testing']], '\n', sep=''))
    cat(paste('Training fraction:\t\t', round(x[['training_frac']], 2), '\n', sep=''))
    invisible(x)
}

#' @export
#' @method length pixel_data
length.pixel_data <- function(x) {
    return(length(x@y))
}

#' @export
#' @method levels pixel_data
levels.pixel_data <- function(x) {
    return(levels(x@y))
}

#' @export
#' @method print pixel_data
print.pixel_data <- function(x, ...) {
    print(summary(x, ...))
}

#' @export
#' @importFrom maptools spRbind
#' @method rbind pixel_data
rbind.pixel_data <- function(x, ...) {
    for (item in c(...)) {
        x@x <- rbind(x@x, item@x)
        x@y <- factor(c(as.character(x@y), as.character(item@y)))
        x@pixel_src <- rbind(x@pixel_src, item@pixel_src)
        x@training_flag <- c(x@training_flag, item@training_flag)
        if (any(row.names(x@polys) %in% row.names(item@polys)))
            stop('training polygon IDs are not unique - are src_names unique?')
        x@polys <- spRbind(x@polys, item@polys)
    }
    return(x)
}

#' Extract part of pixel_data class
#'
#' @method [ pixel_data
#' @rdname extract-methods
#' @param x a \code{pixel_data} object
#' @param i a class or list of classes to extract
#' @param j unused
#' @param ... additional arguments (none implemented)
setMethod("[", signature(x="pixel_data", i='character', j="ANY"),
function(x, i, j, ...) {
    if (!(i %in% levels(x@y))) {
        stop(paste0('"', i, '"', ' is not a class in this pixel_data object'))
    }
    sel_rows <- x@y %in% i
    used_polys <- which(paste(x@polys@data$src, x@polys@data$ID) %in% 
                        with(x@pixel_src[sel_rows, ], paste(src, ID)))
    initialize(x, x=x@x[sel_rows, ], y=x@y[sel_rows], 
               pixel_src=x@pixel_src[sel_rows, ], 
               training_flag=x@training_flag[sel_rows], 
               polys=x@polys[used_polys, ])
})

setMethod("show", signature(object="pixel_data"), function(object) 
          print(object))

#' Subsample a pixel_data object
#'
#' @export subsample
#' @param x a \code{pixel_data} object
#' @param size either 1) a number from 0 to 1, indicating \code{size} is the 
#' fraction of pixels to sample, or 2) a number greater than 1, in which case 
#' \code{size} is the number of pixels to sample. Size applies per strata, if 
#' stratification is chosen.
#' @param strata whether to draw samples from within individual classes, nested 
#' within source polygons (\code{strata='sources'}), or from within individual 
#' classes alone (\code{strata='classes'})
#' @param type whether to subsample training data (\code{type='training'}) or 
#' testing data (\code{type='testing'}). Whichever type is chosen, the other 
#' type will be left untouched (for example, if \code{type='testing'}, the 
#' training data will not be changed).
#' @param flag whether to swap training flag on sampled data (for example, flag 
#' sampled training data as testing data, if \code{flag=TRUE} and 
#' \code{type='training'}) or remove sampled data from dataset entirely 
#' (\code{flag=FALSE}).
#' @param classes specifies which classes to sample, defaults to all classes in 
#' \code{x}
#' @rdname subsample
#' @aliases subsample,pixel_data-method
setGeneric("subsample", function(x, size, strata="sources", type="training", 
                                 flag=TRUE, classes=levels(x@y))
    standardGeneric("subsample")
)

#' @rdname subsample
#' @aliases subsample,pixel_data,numeric-method
#' @importFrom dplyr group_by sample_frac
setMethod("subsample", signature(x="pixel_data", size="numeric"),
function(x, size, strata, type, flag, classes) {
    row_IDs <- data.frame(y=x@y,
                          pixel_src=paste(x@pixel_src$src, x@pixel_src$ID),
                          row_num=seq(1, length(x@y)))
    stopifnot(size > 0)
    stopifnot(strata %in% c("sources", "classes"))
    stopifnot(type %in% c("training", "testing"))
    if (type == "training") {
        row_IDs <- row_IDs[x@training_flag, ]
    } else {
        row_IDs <- row_IDs[!x@training_flag, ]
    }
    row_IDs <- row_IDs[row_IDs$y %in% classes, ]
    stopifnot(nrow(row_IDs) > 1)
    if (strata == "sources") {
        y=pixel_src=NULL
        row_IDs <- group_by(row_IDs, y, pixel_src)
    } else if (strata == "classes") {
        row_IDs <- group_by(row_IDs, y)
    }
    if (size < 1) {
        samp_rows <- dplyr:::sample_frac.grouped_df(row_IDs, size)$row_num
    } else {
        samp_rows <- dplyr:::sample_n.grouped_df(row_IDs, size)$row_num
    }
    if (flag) {
        if (type == 'testing') {
            x@training_flag[samp_rows] <- TRUE
        } else if (type == 'training') {
            x@training_flag[samp_rows] <- FALSE
        }
    } else {
        x@x <- x@x[samp_rows, ]
        x@y <- x@y[samp_rows]
        x@training_flag <- x@training_flag[samp_rows]
        x@pixel_src <- x@pixel_src[samp_rows, ]
    }
    return(x)
})

#' Get or set training_flag for a pixel_data object
#'
#' @export training_flag
#' @param x a \code{pixel_data} object
#' @param classes specifies a subset of classes in \code{x}
#' @aliases training_flag,pixel_data-method
setGeneric("training_flag", function(x, classes=levels(x@y)) {
    standardGeneric("training_flag")
})

#' @rdname training_flag
setMethod("training_flag", signature(x="pixel_data"),
function(x, classes) {
    if (identical(classes, levels(x@y))) {
        return(x@training_flag)
    } else {
        return(x@training_flag[x@y %in% classes])
    }
})

#' @rdname training_flag
#' @export
#' @param value training flag to assign for pixels in \code{x}
setGeneric("training_flag<-", function(x, classes=levels(x@y), value) {
    standardGeneric("training_flag<-")
})

#' @rdname training_flag
setMethod("training_flag<-", signature(x="pixel_data"),
function(x, classes=levels(x@y), value) {
    if (identical(classes, levels(x@y))) {
        # More efficiently handle special case of reassigning flags for all 
        # classes in x.
        if (length(value) == 1) value <- rep(value, length(x@training_flag))
        stopifnot(length(value) == length(x@training_flag))
        x@training_flag <- value
        return(x)
    } else {
        sel_rows <- which(x@y %in% classes)
        if (length(value) == 1) value <- rep(value, length(sel_rows))
        stopifnot(length(value) == length(sel_rows))
        x@training_flag[sel_rows] <- value
        return(x)
    }
})

#' Get number of testing pixels in a pixel_data object
#'
#' @export n_test
#' @param x a \code{pixel_data} object
#' @param classes specifies a subset of classes in \code{x}
#' @aliases n_test,pixel_data-method
setGeneric("n_test", function(x, classes=levels(x@y)) {
    standardGeneric("n_test")
})

#' @rdname n_test
setMethod("n_test", signature(x="pixel_data"),
function(x, classes) {
    if (identical(classes, levels(x@y))) {
        return(sum(!x@training_flag))
    } else {
        return(sum(!x@training_flag[x@y %in% classes]))
    }
})

#' Get number of training pixels in a pixel_data object
#'
#' @export n_train
#' @param x a \code{pixel_data} object
#' @param classes specifies a subset of classes in \code{x}
#' @aliases n_train,pixel_data-method
setGeneric("n_train", function(x, classes=levels(x@y)) {
    standardGeneric("n_train")
})

#' @rdname n_train
setMethod("n_train", signature(x="pixel_data"),
function(x, classes) {
    if (identical(classes, levels(x@y))) {
        return(sum(x@training_flag))
    } else {
        return(sum(x@training_flag[x@y %in% classes]))
    }
})

#' Get or set src_name for a pixel_data object
#'
#' @export src_name
#' @param x a \code{pixel_data} object
#' @param classes specifies a subset of classes in \code{x}
#' @aliases src_name,pixel_data-method
setGeneric("src_name", function(x, classes=levels(x@y)) {
    standardGeneric("src_name")
})

#' @method src_name pixel_data
setMethod("src_name", signature(x="pixel_data"),
function(x, classes) {
    if (identical(classes, levels(x@y))) {
        return(paste0(x@pixel_src$src, '_', x@pixel_src$ID))
    } else {
        return(with(x@pixel_src[x@y %in% classes, ], paste0(src, '_', ID)))
    }
})

#' @export
#' @rdname src_name
#' @param value a new \code{src_name} to assign for pixels in \code{x}
setGeneric("src_name<-", function(x, value) standardGeneric("src_name<-"))

#' @rdname src_name
setMethod("src_name<-", signature(x="pixel_data"),
function(x, value) {
    if (length(value) == 1) {
        value <- rep(value, nrow(x@polys))
    } else if (length(value) != nrow(x@polys)) {
        stop('src_name must be equal to 1 or number of polygons in x')
    }
    old_full_polyID <- paste(x@polys$src, x@polys$ID)
    x@polys$src <- value
    row.names(x@polys) <- paste0(x@polys$src, '_', x@polys$ID)

    new_full_polyID <- paste(x@polys$src, x@polys$ID)

    poly_pixel_match <- match(paste(x@pixel_src$src, x@pixel_src$ID), 
                              old_full_polyID)

    x@pixel_src$src <- x@polys$src[poly_pixel_match]
    x@pixel_src$ID <- x@polys$ID[poly_pixel_match]

    return(x)
})

#' Extract observed data for use in a classification (training or testing)
#'
#' @export
#' @param x a \code{Raster*} object from which observed data will be extracted.  
#' The data will be extracted from each layer in a \code{RasterBrick} or 
#' \code{RasterStack}.
#' @param polys a \code{SpatialPolygonsDataFrame} with polygons, each of which 
#' has been assigned to a particular class (using the \code{class_col}
#' @param class_col the name of the column containing the response variable 
#' (for example the land cover type of each pixel)
#' @param training indicator of which polygons to use in training. Can be: 1) a 
#' string giving the name of a column indicating whether each polygon is to be 
#' used in training (rows equal to 1) or in testing (rows equal to FALSE), or 
#' 2) a logical vector of length equal to length(polys), or 3) a number between 
#' 0 and 1 indicating the fraction of the polygons to be randomly selected for 
#'   use in training.
#' @param src name of this data source. Useful when gathering training 
#' data from multiple images.
#' @return a \code{link{pixel_data}} object
#' will contain the the @examples
#' set.seed(1)
#' train_data <- get_pixels(L5TSR_1986, L5TSR_1986_2001_training, "class_1986", 
#'                          training=.6)
get_pixels <- function(x, polys, class_col, training=1, src='none') {
    if (projection(x) != projection(polys)) {
        stop('Coordinate systems do not match')
    }
    stopifnot(length(src) == 1)
    if (tolower(class_col) == 'id') {
        stop('class_col cannot be named "ID" (case insensitive)')
    }
    # Convert class_col from the name of the column to an index
    class_colnum <- grep(paste0('^', class_col, '$'), names(polys))
    if (length(class_colnum) == 0) {
        stop(paste0('"', class_col, '" not found in polys'))
    }
    # This is displayed in the dataframe, and should never change
    polys$ID <- row.names(polys)
    polys$src <- src
    row.names(polys) <- paste0(polys$src, '_', polys$ID)
    if (is.character(training)) {
        # Handle case of having column name suppled as 'training'
        training_col_index <- grep(training, names(polys))
        if (length(training_col_index) == 0) {
            stop(paste0('"', training,  '" column not found in x'))
        } else if (!is.logical(polys[training_col_index])) {
            stop(paste0('"', training,  '" column must be a logical vector'))
        }
        training <- polys[, grep(training, names(polys))]
    } else if (is.numeric(training) && (length(training) == 1) &&
               (training >= 0) && (training <= 1)) {
        # Handle case of having fraction supplied as 'training'
        if ('training_flag' %in% names(polys)) {
            stop('"training_flag" column already present in polys')
        }
        if (training == 0) {
            # Handle training=0 separately to enable use of quantile function 
            # below.
            polys$training_flag <- FALSE
        } else {
            sample_strata <- function(x) {
                rand_vals <- runif(length(x))
                rand_vals <= quantile(rand_vals, training)
            }
            polys$training_flag <- unlist(tapply(polys@data$ID, 
                                                 polys@data[class_colnum], 
                                                 sample_strata))
        }
    } else if ((length(training) == length(polys)) && is.logical(training)) {
        # Handle case of having vector supplied as 'training'
        polys$training_flag <- training
    } else {
        stop('"training" must be a column name, vector of same length as polys, or length 1 numeric')
    }
    pixels <- extract(x, polys, small=TRUE, df=TRUE)

    poly_rows <- pixels$ID
    pixels <- pixels[!(names(pixels) == 'ID')]

    # Convert y classes to valid R variable names - if they are not valid R 
    # variable names, the classification algorithm may throw an error
    y <- factor(make.names(polys@data[poly_rows, class_colnum]))

    pixel_src <- data.frame(src=polys@data[poly_rows, ]$src,
                            ID=polys@data[poly_rows, ]$ID, 
                            stringsAsFactors=FALSE)

    return(new("pixel_data", x=pixels, y=y, pixel_src=pixel_src,
               training_flag=polys@data[poly_rows, ]$training_flag,
               polys=polys))
}
#' Performs a rough comparison of two proj4strings to see if they match
#'
#' Compares the proj, ellps, zone (if applicable), units, and datum tags in two 
#' proj4strings to determine if two projections match. Requires proj and ellps 
#' to be present. If present, zone, units, and datum must match in both 
#' strings.
#'
#' @export
#' @importFrom rgdal CRSargs
#' @importFrom stringr str_extract
#' @param x a proj4string to compare with \code{y}
#' @param y a proj4string to compare with \code{x}
#' @return TRUE or FALSE depending on if the projections match
proj4comp <- function(x, y) {
    if (grepl('+init=', x)) {
        x <- CRSargs(CRS(x))
    }
    if (grepl('+init=', y)) {
        y <- CRSargs(CRS(y))
    }

    x_proj <- str_extract(x, '+proj=[a-zA-Z0-9]*')
    y_proj <- str_extract(y, '+proj=[a-zA-Z0-9]*')
    if (sum(is.na(c(x_proj, y_proj))) > 0) {
        stop('proj string is missing')
    } else {
        if (x_proj != y_proj) {
            return(FALSE)
        }
    }

    x_ellps <- str_extract(x, '+ellps=[a-zA-Z0-9]*')
    y_ellps <- str_extract(y, '+ellps=[a-zA-Z0-9]*')
    if (sum(is.na(c(x_ellps, y_ellps))) > 0) {
        stop('ellps string is missing')
    } else if (x_ellps != y_ellps) {
        return(FALSE)
    }


    if (grepl('utm', tolower(x_proj))) {
        x_zone <- str_extract(x, '+zone=[a-zA-Z0-9]*')
        y_zone <- str_extract(y, '+zone=[a-zA-Z0-9]*')
        if (sum(is.na(c(x_zone, y_zone))) > 0) {
            stop('utm zone must be specified for utm projections')
        } else if (x_zone != y_zone) {
            return(FALSE)
        }
        x_south <- str_extract(tolower(x), '+south')
        y_south <- str_extract(tolower(y), '+south')
        if (!is.na(x_south) || !is.na(y_south)) {
            # Get here if one or more of x_south and y_south is not NA
            if (xor(is.na(x_south), is.na(y_south)) || (x_south != y_south)) {
                # Get here if ONLY one of x_south and y_south is NA, or, if 
                # x_south and y_south are both not NA and are both not equal
                return(FALSE)
            }
        }
    }

    x_datum <- str_extract(x, '+datum=[a-zA-Z0-9]*')
    y_datum <- str_extract(y, '+datum=[a-zA-Z0-9]*')
    if ((!is.na(x_datum) && !is.na(y_datum)) & (x_datum != y_datum)) {
        return(FALSE)
    }
    
    x_units <- str_extract(x, '+units=[a-zA-Z0-9]*')
    y_units <- str_extract(y, '+units=[a-zA-Z0-9]*')
    if ((!is.na(x_units) && !is.na(y_units)) & (x_units != y_units)) {
        return(FALSE)
    }

    return(TRUE)
}
#' Radiometric calibration and correction
#' 
#' Implements several different methods for absolute radiometric correction of Landsat data.
#' You can either specify a metadata file, or supply all neccesary values manually. With proper parametrization APREF and SDOS should work for other sensors as well.
#' 
#' @param x raster object
#' @param metaData either the result of \code{readMeta} or a path to the meta data (MCL) file. 
#' @param reflectance logical. If \code{TRUE} output will be reflectance, if \code{FALSE} it will be radiance
#' @param thermal logical. If \code{TRUE} thermal bands will be converted to brightness temperature (Kelvin).
#' @param bandSet numeric or character. original Landsat band numbers or names in the form of ("B1", "B2" etc). If set to 'full' all bands in the solar region will be processed.
#' @param gain Band-specific sensor gain. Require either gain and offset or Grescale and Brescale to convert DN to radiance.
#' @param offset Band-specific sensor offset. Require either gain and offset or Grescale and Brescale to convert DN to radiance.
#' @param Grescale Band-specific sensor Grescale (gain). Require either gain and offset or Grescale and Brescale to convert DN to radiance.
#' @param Brescale Band-specific sensor Brescale (bias). Require either gain and offset or Grescale and Brescale to convert DN to radiance.
#' @param sunElev Sun elevation in degrees
#' @param satZenith sensor zenith angle (0 for Landsat)
#' @param d Earth-Sun distance in AU.
#' @param esun Mean exo-atmospheric solar irradiance, as given by Chandler et al. 2009 or others.
#' @param SHV starting haze value, can be estimated using estimateSHV(). if not provided and method is "DOS" or "COSTZ" SHV will be estimated in an automated fashion. Not needed for apparent reflectance.
#' @param hazeBand band from which SHV was estimated.
#' @param method Radiometric correction method to be used. There are currently four methods available (see Details):
#'  "APREF", "DOS" (Chavez 1989), "COSTZ" (Chavez 1996), SDOS.
#' @note This was originally a fork of randcorr in the landsat package. It may be slower, however it works on Raster* objects and hence is memory-safe.
#' @details  \describe{
#' \item{APREF}{Apparent reflectance}
#' \item{DOS}{Dark object subtratction following Chavez (1989)}
#' \item{COSTZ}{Dark object subtraction following Chaves(1996)}
#' \item{SDOS}{Simple dark object subtraction. Classical DOS, Lhaze must be estimated for each band separately.}
#' }
#' @references S. Goslee (2011): Analyzing Remote Sensing Data in R: The landsat Package. Journal of Statistical Software 43(4).
#' @export
#' @seealso \link[landsat]{radiocorr} 
radCor <-	function(x, metaData, reflectance = TRUE, thermal = TRUE, satellite, bandSet = "full", gain, offset, G_rescale, B_rescale,
		sunElev, satZenith = 0, d, esun, date, SHV, hazeBand, atHaze,  method = "APREF"){
	# http://landsat.usgs.gov/Landsat8_Using_Product.php
	
	if(!method %in% c("APREF", "DOS", "COSTZ", "SDOS")) stop("method must be one of 'APREF', 'DOS', 'COSTZ' 'SDOS'", call.=FALSE)
	
	if(!reflectance & method != "APREF"){
		warning("For radiance calculations the 'method' argument is ignored")
		method <- "APREF"
	}
	
	if(!missing(metaData)) {
		
		## Read metadata from file
		if(is.character(metaData)) metaData <- readMeta(metaData)
		
		satellite 	<- metaData$UNIFIED_METADATA$SPACECRAFT_ID
		sensor 		<- metaData$UNIFIED_METADATA$SENSOR_ID
		B_rescale	<- metaData$UNIFIED_METADATA$RAD_OFFSET
		G_rescale	<- metaData$UNIFIED_METADATA$RAD_GAIN
		d			<- metaData$UNIFIED_METADATA$EARTH_SUN_DISTANCE
		sunElev		<- metaData$UNIFIED_METADATA$SUN_ELEVATION
		rad 		<- metaData$UNIFIED_METADATA$RADIOMETRIC_RES
		K1			<- metaData$UNIFIED_METADATA$K1
		K2			<- metaData$UNIFIED_METADATA$K2
		
	} else {
		###  FIXME: HARD CODED !!
		sensor = 1
		rad = 8
		###
		if(missing(G_rescale) | missing(B_rescale)){
			if(missing(offset) | missing(gain)) {
				stop("Please specify either a) metaData, b) gain and offset, c) B_rescale and G_rescale", call. = FALSE )
			} else {
				B_rescale <- 1/gain
				G_rescale <- -offset/gain
			}
		}
		
		
		if(missing(d)) {
			if(missing(date)) { 
				stop("Please specify either a) edist or b)date", call. = FALSE) 
			} else {
				d <- .ESdist(date) 
			}
		}
	}
	
	if(satellite == "LANDSAT8" & method != "APREF") {
		warning("DOS, COSTZ and SDOS are currently not implemented for Landsat 8. Using official reflectance calibration coefficients, i.e. output corresponds to method = 'APREF'", call. = FALSE) 
		method <- "APREF"
	}
	
	satZenith	<- satZenith * pi / 180
	satphi 		<- cos(satZenith)
	suntheta 	<- cos((90 - sunElev) * pi / 180)	
	
	## Query internal db	
	sDB <- LANDSAT.db[[satellite]][[sensor]]
	
	## We use .getNumeric to deal with band name appendices (e.g. LS7 can have to versions of band 6: B6_VCID_1 and B6_VCID_2
	## which would not match the database name B6
	sDB 	<- sDB[match(paste0("B", sapply(.getNumeric(names(x)),"[",1)), sDB$band),]	
	sDB		<- sDB[match(sDB$band, paste0("B",sapply(.getNumeric(names(x)),"[",1))),]
	
	if(any(bandSet == "full")) {
		bandSet <- names(x)
	} else {
		if(is.numeric(bandSet)) bandSet <- paste0("B", bandSet)
	}	
	
	if(missing(metaData))	names(B_rescale) <- names(G_rescale) <- bandSet
	
	origBands 	<- names(x)   
	corBands 	<- sDB[!sDB$bandtype %in% c("TIR", "PAN"), "band"]
	bandSet 	<- bandSet[bandSet %in% corBands]
	if(thermal){
		tirBands	<- if(satellite=="LANDSAT8") c("B10", "B11") else c("B6", "B6_VCID_1", "B6_VCID_2")	
		tirBands 	<- origBands[origBands %in% tirBands]
	} else {
		tirBands <- NULL
	}
	exclBands	<- origBands[!origBands %in% c(bandSet, tirBands)]
	
	if(length(exclBands) > 0) {
		xexc <- x[[exclBands]] 
	} else {
		xexc <- NULL
	}
	
	if(missing(esun)) {
		esun <- sDB[,"esun"] 
		names(esun) <- sDB$band
	}
	xref <- x[[bandSet]]
	
	if(reflectance) {
		message("Bands to convert to reflectance: ", paste(bandSet, collapse = ", "))
		if(length(tirBands) > 0 & thermal) message("Thermal bands to convert to brightness temperatures: ", paste(tirBands, collapse=", "))
		if(length(exclBands) > 0) message("Excluding bands: ", paste(exclBands, collapse = ", "))	
	} else {
		bandSet <- c(bandSet, tirBands)
		message("Bands to convert to toa radiance: ", paste(bandSet, collapse = ", "))
	}
	
	## Thermal processing
	if(thermal & reflectance & length(tirBands) > 0) {
		message("Processing thermal band(s)")
		## Convert to radiance
		L <- G_rescale[tirBands] * x[[tirBands]] + B_rescale[tirBands]
		## Convert to temperature
		xtir <- K2 / log(K1/L + 1) 
		names(xtir) <- tirBands
	} else {
		xtir <- NULL
	}
	
	message("Processing radiance / reflectance")
	
	## Radiance and reflectance processing
	if(method == "APREF") {
		TAUz <- 1
		TAUv <- 1
		Edown <- 0
		Lhaze <- 0
		
	} else {
		
		## Estimate SHV automatically
		if(missing(SHV)){
			if(missing(hazeBand))  hazeBand <- "B1"
			if(length(hazeBand) > 1) {
				warning("Automatic search for SHV values is intended for one band only. For more bands please estimate hzae DNs manually using estimateSHV() \nhazeBand was automatically reset to 1")
				hazeBand <- 1 }
			message("SHV was not provided -> Estimating SHV automatically")
			dP <- 0.02
			## We suppress warnings because we search for a possible value autimatically in case we missed the first time
			SHV <- suppressWarnings(estimateSHV(x, hazeBand = hazeBand, darkProp = dP , plot = FALSE, returnTables = TRUE))
			while(is.na(SHV[[1]])){
				dP	<- dP * 0.9
				SHV <- suppressWarnings(estimateSHV(SHV, hazeBand = hazeBand, darkProp = dP, plot = FALSE, returnTables = TRUE))
			}
			message(paste0("SHV estimated as: ", SHV[[1]]))
			SHV <- SHV[[1]]
		}
		
		
		# For SDOS gain, offset, Lhaze and Esun must be provided as coresponding vectors of equal length
		if(method == "SDOS") hazeBand <- bandSet 
		TAUz <- 1
		TAUv <- 1
		Edown <- 0				
		if (method == "COSTZ") {
			TAUz <- suntheta
			TAUv <- satphi
		}  
		
		## 1% correction and conversion to radiance
		Ldo <- 0.01 * ((esun[hazeBand] * suntheta * TAUz) + Edown) * TAUv / (pi * d ^ 2)
		Lhaze <- (SHV * G_rescale[hazeBand] + B_rescale[hazeBand]) - Ldo
		
		if(method %in% c("DOS", "COSTZ")) {		
			## Pick atmoshpere type
			if(missing(atHaze)) {
				atHaze.db <- data.frame(min = c(1,56,76,96,116), max = c(55,75,95,115,255)) / 255 * (2^rad-1)
				atHaze <- c("veryClear", "clear", "moderate", "hazy", "veryHazy")[Lhaze > atHaze.db[,1] & Lhaze <= atHaze.db[,2]]
				message("Selcting atmosphere: '", atHaze, "'")
			}		
			Lhaze	  <- Lhaze  * sDB[match(bandSet,sDB$band), paste0(hazeBand,"_", atHaze)]
			
			## Calculate corrected RAD_haze
			NORM  <- G_rescale[bandSet] / G_rescale[hazeBand]
			Lhaze <- Lhaze * NORM + B_rescale[bandSet]	
		}
		# In case Lhaze becomes negative we reset it to zero to prevent artefacts.
		Lhaze [Lhaze < 0] <- 0
	}
	
	B_rescale	<- B_rescale[bandSet]
	G_rescale 	<- G_rescale[bandSet]
	esun <- esun[bandSet]
	
	if(satellite != "LANDSAT8"){
		
		if(!reflectance) {
			## TOA Radiance
			xref <-  ( xref * G_rescale + B_rescale) / suntheta
		} else {
			## At-surface reflectance (precalculate coefficients to speed up raster processing)
			C <- (pi * d ^ 2)/(TAUv * (esun * suntheta * TAUz + Edown))	
			b <- C * (B_rescale - Lhaze)
			a <- C * G_rescale 
			xref <-  a * xref  + b
		}
		
	} else {
		
		if(reflectance) {
			B_rescale 		<- metaData$UNIFIED_METADATA$REF_OFFSET[bandSet]
			G_rescale 		<- metaData$UNIFIED_METADATA$REF_GAIN[bandSet]
		} 
		
		## At sensor radiance / reflectance
		xref <-  (G_rescale * xref + B_rescale) / suntheta
		
		## At-surface reflectance?
	}
	
	## Re-combine thermal, solar and excluded imagery
	x <- stack(xref,xtir, xexc)
	x <- x[[origBands]]
	
	return(x)
}


#' Landsat auxilliary data. Taken from Chander et al 2009
#' spatRes resampling: http://landsat.usgs.gov/band_designations_landsat_satellites.php
#' @keywords internal
LANDSAT.db <- list(
		LANDSAT5 = list (
				TM = data.frame(band = paste0("B", 1:7),
						bandtype = c(rep("REF", 5), "TIR", "REF"),
						centerWavl = c(0.485, 0.569, 0.66, 0.840, 1.676, 11.435, 2.223),
						spatRes1 = rep(30, 7),
						spatRes2 = c(rep(30,5), 60, 30), ## TM Band 6 was acquired at 120-meter resolution, but products processed before February 25, 2010 are resampled to 60-meter pixels. Products processed after February 25, 2010 are resampled to 30-meter pixels.
						esun = c(1983, 1796, 1536, 1031, 220, NA, 83.44))
		),
		LANDSAT7 = list(
				ETM = data.frame(band = paste0("B",1:8),
						bandtype = c(rep("REF", 5), "TIR", "REF", "PAN"),
						spatRes1 = c(rep(30, 7), 15),
						spatRes2 = c(rep(30,5), 60, 30, 15),  ## ETM+ Band 6 is acquired at 60-meter resolution. Products processed after February 25, 2010 are resampled to 30-meter pixels.
						centerWavl = c(0.485, 0.560, 0.660, 0.835, 1.650,11.335,2.220,0.706),
						esun = c(1997,1812,1533,1039,230.8,NA,84.9,1362)
				)
		),
		LANDSAT8 = list(
				OLI_TIRS = data.frame(band = c(paste0("B",1:11), "BQA"),
						bandtype = c(rep("REF", 7), "PAN", "REF", "TIR", "TIR", "QA"),
						spatRes1 = c(rep(30, 7), 15, rep(30,4)),
						spatRes2 = c(rep(30, 7), 15, rep(30,4)),  ## ETM+ Band 6 is acquired at 60-meter resolution. Products processed after February 25, 2010 are resampled to 30-meter pixels.
						centerWavl = c(0.44,0.48,0.56,0.655,0.865,1.61,2.2,0.59,1.37,10.6,11.5, NA), 
						esun = c(NA, 2067, 1893, 1603, 972.6, 245, 79.72, NA, 399.7, NA, NA, NA ) ## http://www.gisagmaps.com/landsat-8-atco/ ##http://landsat.usgs.gov/Landsat8_Using_Product.php
				)
		)

) 

exponents <- c(-4, -2, -1, -.7, -.5)
for(s in names(LANDSAT.db)){
	bandType		<- LANDSAT.db[[s]][[1]][,"bandtype"] == "REF"
	centerWavl		<- LANDSAT.db[[s]][[1]][bandType, "centerWavl"] 
	bands 			<- LANDSAT.db[[s]][[1]][bandType, "band"]
	
	## Calc Chavez Tab 1
	TAB1			<- sapply(exponents, function(x) centerWavl ^ x)
	rownames(TAB1)  <- bands
	colnames(TAB1)	<- c("veryClear", "clear", "moderate", "hazy", "veryHazy")
	
	## Calc Chavez Tab 2, but only until SHVB = B4, larger wavelengths don't make sense to estimate haze
	TAB2 <- lapply(paste0("B", 1:4), function(SHVB){ sweep(TAB1, 2, TAB1[SHVB,], "/")})
	TAB2 <- do.call("cbind", TAB2)
	colnames(TAB2) <- paste0(rep(paste0("B", 1:4), each = 5),"_", colnames(TAB2))
	
	LANDSAT.db[[s]][[1]] <-  merge(LANDSAT.db[[s]][[1]] , TAB2, by.x = "band", by.y = "row.names", all.x = TRUE, sort = FALSE)
}







radiocorr <-
function(x, gain, offset, Grescale, Brescale, sunelev, satzenith=0, edist, Esun, Lhaze, method="apparentreflectance")
{

### Radiometric correction following one of four models:
###   - 1: Absolute Radiance
###   - 2: Dark Object Subtraction (DOS) of Chavez 1989
###   - 3: COSTZ of Chavez 1996
###   - 4: DOS4 of SWS+2001
### x: image data
### gain and offset or gain and bias or Lmin and Lmax
### sunelev: sun elevation in degrees
### satzenith: satellite zenith angle in degrees (= 0 for Landsat)
### edist: Earth-Sun distance (calculated from DOY)
### Esun: extrasolar radiation
### Lhaze: result of DOS() for this band, if needed
### crosscalib: cross-calibration coefficient of Teillet et al. 2006 or other
###    multiplicative adjustment to be applied to at-sensor reflectance - NOT DONE!

    results <- x
    x <- as.vector(as.matrix(x))

    METHODS <- c("apparentreflectance", "DOS", "COSTZ", "DOS4")
    method <- pmatch(method, METHODS)
    if (is.na(method)) 
        stop("invalid method")
    if (method == -1) 
        stop("ambiguous method")

    suntheta <- (90-sunelev) * pi / 180
    suntheta <- cos(suntheta)

    satzenith <- satzenith * pi / 180
    satphi <- cos(satzenith)

    # most new references provide gain and bias
    # want gain and offset
    if(missing(offset)) {
        offset <- -1 * Brescale / Grescale
        gain <- 1/Grescale
    }

    ### Done with basic setup.

    if(method == 1) {
    ## 1. Apparent Reflectance
        TAUz <- 1.0
        TAUv <- 1.0
        Edown <- 0.0 
        Lhaze <- 0.0
    }
    else if(method == 2) {
    ## 2. DOS
        TAUz <- 1.0
        TAUv <- 1.0
        Edown <- 0.0 
        if(missing(Lhaze)) stop("This model requires Lhaze to be specified.\n")	
    }
    else if(method == 3) {
    ## 3. COSTZ
        TAUz <- suntheta
        TAUv <- satphi
        Edown <- 0.0
        if(missing(Lhaze)) stop("This model requires Lhaze to be specified.\n")
    } 
    else if(method == 4) {
    ## 4. DOS4 of SWS+2001
        TAUv <- TAUz <- 1
        taudiff <- 1
            tau <- 9999
            Edown <- 0
            
        Lhaze.orig <- Lhaze
        
        while(abs(taudiff) > 0.0000001) {
            taudiff <- tau
            
            ## if Lhaze is too large, the formula tries to take log of a negative number
            ## iteratively adjust Lhaze downward until it works
            ## This is a lazy kludge!!!

            Eo <- Esun/edist^2

            Lp <- (Lhaze - offset) / gain - 0.01 * (Eo * suntheta * TAUz + Edown) * TAUv / pi

            taustep <- 1 - (4 * pi * Lp) / (Eo * suntheta)

            while(taustep < 0) {
                Lhaze <- Lhaze - 1
                Lp <- (Lhaze - offset) / gain - 0.01 * (Eo * suntheta * TAUz + Edown) * TAUv / pi
                taustep <- 1 - (4 * pi * Lp) / (Eo * suntheta)
            }
            
            tau <- -1 * suntheta * log(1 - (4 * pi * Lp) / (Eo * suntheta))
            TAUv <- exp(-1 * tau / satphi)
            TAUz <- exp(-1 * tau / suntheta)		
            Edown <- pi * Lp
            
                    taudiff <- taudiff - tau

        }
            
        if(!identical(Lhaze.orig, Lhaze)) warning(paste("Lhaze adjusted from ", Lhaze.orig, " to ", Lhaze, sep=""))
        
        if(missing(Lhaze)) stop("This model requires Lhaze to be specified.\n")
    #-#

    }


    ## Applying the models
    ## REF <-  (pi * edist^2 * (Lsat - Lhaze)) / (TAUv * (Esun * cos(sunzenith) * TAUz + Edown))

    ## First convert DN to at-sensor radiance
    ## Lhaze output from DOS() is in DN, so this is done as a separate step

    # subtract Lhaze and convert DN to radiance
    x <- x - Lhaze
    x <- (x - offset) / gain

    ## proceed with radiometric correction
    ## calculate at-surface reflectance

    x <-  (pi * edist^2 * x) / (TAUv * (Esun * suntheta * TAUz + Edown))

    # return the same structure as the input values
    if(class(results) == "SpatialGridDataFrame")
        results@data[,1] <- x
    else if(is.data.frame(x))
        results <- data.frame(matrix(x, nrow=nrow(results), ncol=ncol(results)))
    else # return a matrix 
        results <- x
    
    results
}

# This file was generated by Rcpp::compileAttributes
# Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#' Threshold an image using Huang's fuzzy thresholding method.
#'
#' Implements Huang's fuzzy thresholding method. This function is called by 
#' the \code{\link{threshold}} function. It is not intended to be used 
#' directly.
#'
#' Ported to C++ by from the code in the Auto_threshold imageJ plugin by 
#' Gabriel Landini.
#'
#' See original code at:
#' http://www.mecourse.com/landinig/software/autothreshold/autothreshold.html
#'
#' @param data the input image
#' @return integer threshold value
#' @references Huang, L.-K., and M.-J. J. Wang. 1995. Image thresholding by 
#' minimizing the measures of fuzziness. Pattern recognition 28 (1):41--51.
threshold_Huang <- function(data) {
    .Call('teamlucc_threshold_Huang', PACKAGE = 'teamlucc', data)
}

#' Calculate change direction
#'
#' This code calculate the change direction from two probability images. Not
#' intended to be called directly - see \code{chg_dir}.
#'
#' @export
#' @param t1p time 1 posterior probability matrix (with pixels in rows, bands 
#' in columns)
#' @param t2p time 2 posterior probability matrix (with pixels in rows, bands 
#' in columns)
#' @return vector of change directions
#' @references Chen, J., P. Gong, C. He, R. Pu, and P. Shi. 2003.
#' Land-use/land-cover change detection using improved change-vector analysis.
#' Photogrammetric Engineering and Remote Sensing 69:369-380.
#' 
#' Chen, J., X. Chen, X. Cui, and J. Chen. 2011. Change vector analysis in 
#' posterior probability space: a new method for land cover change detection.  
#' IEEE Geoscience and Remote Sensing Letters 8:317-321.
calc_chg_dir <- function(t1p, t2p) {
    .Call('teamlucc_calc_chg_dir', PACKAGE = 'teamlucc', t1p, t2p)
}

#' Cloud fill using the algorithm developed by Xiaolin Zhu
#'
#' This function is called by the \code{\link{cloud_remove}} function. It is
#' not intended to be used directly.
#'
#' @param cloudy the cloudy image as a matrix, with pixels in columns (in 
#' column-major order) and with number of columns equal to number of bands
#' @param clear the clear image as a matrix, with pixels in columns (in 
#' column-major order) and with number of columns equal to number of bands
#' @param cloud_mask the cloud mask image as a vector (in column-major order), 
#' with clouds coded with unique integer codes starting at 1, and with areas 
#' that are clear in both images  coded as 0. Areas that are missing in the 
#' clear image, should be coded as -1.
#' @param dims the dimensions of the cloudy image as a length 3 vector: (rows, 
#' columns, bands)
#' @param num_class set the estimated number of classes in image
#' @param min_pixel the sample size of similar pixels
#' @param max_pixel the maximum sample size to search for similar pixels
#' @param cloud_nbh the range of cloud neighborhood (in pixels)
#' @param DN_min the minimum valid DN value
#' @param DN_max the maximum valid DN value
#' @param verbose whether to print detailed status messages
#' @return array with cloud filled image with dims: cols, rows, bands
#' parameter, containing the selected textures measures
#' @references Zhu, X., Gao, F., Liu, D., Chen, J., 2012. A modified
#' neighborhood similar pixel interpolator approach for removing thick clouds 
#' in Landsat images. Geoscience and Remote Sensing Letters, IEEE 9, 521--525.
cloud_fill <- function(cloudy, clear, cloud_mask, dims, num_class, min_pixel, max_pixel, cloud_nbh, DN_min, DN_max, verbose = FALSE) {
    .Call('teamlucc_cloud_fill', PACKAGE = 'teamlucc', cloudy, clear, cloud_mask, dims, num_class, min_pixel, max_pixel, cloud_nbh, DN_min, DN_max, verbose)
}

#' Cloud fill using a simple linear model approach
#'
#' This algorithm fills clouds using a simple approach in which the value of 
#' each clouded pixel is calculated using a linear model. The script
#' develops a separate linear model (with slope and intercept) for each band 
#' and each cloud. For each cloud, and each image band, the script finds all 
#' pixels clear in both the cloudy and fill images, and calculates a 
#' regression model in which pixel values in the fill image are the 
#' independent variable, and pixel values in the clouded image are the 
#' dependent variable. The script then uses this model to predict pixel values 
#' for each band in each cloud in the clouded image.
#'
#' This function is called by the \code{\link{cloud_remove}} function. It is
#' not intended to be used directly.
#'
#' @param cloudy the cloudy image as a matrix, with pixels in columns (in 
#' column-major order) and with number of columns equal to number of bands
#' @param clear the clear image as a matrix, with pixels in columns (in 
#' column-major order) and with number of columns equal to number of bands
#' @param cloud_mask the cloud mask image as a vector (in column-major order), 
#' with clouds coded with unique integer codes starting at 1, and with areas 
#' that are clear in both images  coded as 0. Areas that are missing in the 
#' clear image, should be coded as -1.
#' @param dims the dimensions of the cloudy image as a length 3 vector: (rows, 
#' columns, bands)
#' @param num_class set the estimated number of classes in image
#' @param cloud_nbh the range of cloud neighborhood (in pixels)
#' @param DN_min the minimum valid DN value
#' @param DN_max the maximum valid DN value
#' @param verbose whether to print detailed status messages
#' @return array with cloud filled image with dims: cols, rows, bands
#' parameter, containing the selected textures measures
cloud_fill_simple <- function(cloudy, clear, cloud_mask, dims, num_class, cloud_nbh, DN_min, DN_max, verbose = FALSE) {
    .Call('teamlucc_cloud_fill_simple', PACKAGE = 'teamlucc', cloudy, clear, cloud_mask, dims, num_class, cloud_nbh, DN_min, DN_max, verbose)
}

RCS <-
function(data.tc, level=.01) {
# radiometric control sets, HSNG199
# takes output of tasscap
#    list(Brightness=Brightness, Greenness=Greenness, Wetness=Wetness)
# or named data frame, which is also a list

    rcsgrid <- data.tc$Brightness
    
    brightness <- as.vector(as.matrix(data.tc$Brightness))
    greenness <- as.vector(as.matrix(data.tc$Greenness))
        
    bright.llevel <- quantile(brightness, level, na.rm=TRUE)
    bright.ulevel <- quantile(brightness, 1-level, na.rm=TRUE)
    green.level <- quantile(greenness, level, na.rm=TRUE)
    
    rcsmask <- ifelse(brightness < bright.llevel & greenness < green.level, 1, 0)
    rcsmask <- ifelse(brightness > bright.ulevel & greenness < green.level, 1, rcsmask)

    # return the same structure as the input values
    if(class(rcsgrid) == "SpatialGridDataFrame") 
        rcsgrid@data[,1] <- rcsmask
    else if(is.data.frame(rcsgrid)) 
        rcsgrid <- data.frame(matrix(rcsmask, nrow=nrow(rcsgrid), ncol=ncol(rcsgrid)))
    else if(is.matrix(rcsgrid)) 
        rcsgrid <- matrix(rcsmask, nrow=nrow(rcsgrid), ncol=ncol(rcsgrid))
    else # return a vector 
        rcsgrid <- rcsmask

    rcsgrid
}

#' Reads a Landsat 8 product
#' @description Reads a Landsat 8 product
#'
#' @param product name of the product, e.g. LC80522102014165LGN00. It must be in the working directory
#' @return list with metadata and raster bands
#' @examples \dontrun{
#' ReadLandsat8("LC80522102014165LGN00")
#' }
#'
#' @note  Function in Package rLandsat8
#'
#' @export
#' @import raster

ReadLandsat8 <- function(product) {

  raster.files <- list("aerosol"="file_name_band_1",
    "blue"="file_name_band_2",
    "green"="file_name_band_3",
    "red"="file_name_band_4",
    "nir"="file_name_band_5",
    "swir1"="file_name_band_6",
    "swir2"="file_name_band_7",
    "panchromatic"="file_name_band_8",
    "cirrus"="file_name_band_9",
    "tirs1"="file_name_band_10",
    "tirs2"="file_name_band_11"
    )

  meta.file <- paste0(product, "/", product, "_MTL.txt")

  if (!file.exists(meta.file))
       stop(paste(meta.file, "file not found."))

  textLines <- readLines(meta.file)

  counts <- count.fields(textConnection(textLines), sep="=")

  met <- read.table(text=textLines[counts == 2], as.is=TRUE, header=FALSE, sep="=", strip.white=TRUE, stringsAsFactors=FALSE)

  met <- read.table(text=textLines[counts == 2], as.is=TRUE, header=FALSE, sep="=", strip.white=TRUE, stringsAsFactors=FALSE, row.names = NULL, col.names=c("name", "value"))

  met <- met[!met$name == "GROUP", ]
  met <- met[!met$name == "END_GROUP", ]
  rownames(met) <- tolower(met[, "name"])
  met[, "name"] <- NULL

  met <- as.list(as.data.frame(t(met), stringsAsFactors=FALSE))

  bands=lapply(raster.files, function(x) {
    r <- raster(paste0(product, "/", met[[x]]))
    r@title <- names(raster.files)[seq_along(raster.files)[sapply(raster.files, function(a) x %in% a)]]
    NAvalue(r) <- 0
    return(r)
  })

  return(list(metadata=met,
    band=bands)
  )
}
#' Read landsat MTL metadata files
#' 
#' Besides reading metadata, readMeta deals with legacy versions of Landsat metadata files and where possible adds missing information (radiometric gain and offset, earth-sun distance).
#' 
#' @param file path to Landsat MTL file (...MTL.txt)
#' @param unifiedMetadata logical. If \code{TRUE} some relevant etadata of Landsat 5:8 are homogenized into a standard format and appended to the original metadata.
#' @return Returns a list containing the Metadata of the MTL file, structured by the original grouping.
#' 
#' @import landsat
#' @export 
#' 
#' 
#' 
readMeta <- function(file, unifiedMetadata = TRUE){
	if(!grepl("MTL", file) & !grepl("xml", file)) warning("The Landsat metadata file you have specified looks unusual. Typically the filename contains the string 'MTL' or 'xml'. Are you sure you specified the right file? \n I'll try to read it but check the results!")
	
	## Read mtl file
	metaDataFormat <- if(grepl('xml', file)) "XML" else "MTL"
	
	if(metaDataFormat == "MTL") {
		## PROCESS LPS MTL FILES
		
		meta <- read.delim(file, sep = "=", head = FALSE, stringsAsFactors = FALSE, strip.white = TRUE, skip = 1, skipNul = TRUE)
		meta <- meta[-(nrow(meta)-c(1,0)),]
		
		## Retrieve groups
		l <- meta[grep("GROUP",meta[,1]),]
		
		## Assemble metadata list
		meta <- lapply(unique(l[,2]), FUN = function(x){
					w <- which(meta[,2] == x)
					m <- meta[(w[1]+1):(w[2]-1),]
					rownames(m) <- m[,1]
					m <- m[ , 2, drop = FALSE]
					colnames(m) <- "VALUE"
					return(m)
				})
		
		names(meta) <- unique(l[,2])
		
		## Legacy MTL? 
		legacy <- "PROCESSING_SOFTWARE" %in% rownames(meta$PRODUCT_METADATA)
		if(legacy) message("This scene was processed before August 29, 2012. Using MTL legacy format. Some minor infos such as SCENE_ID will be missing")
		
		if(unifiedMetadata){
			
			meta[["UNIFIED_METADATA"]] <- list(
					SPACECRAFT_ID 		= {SAT <- paste0("LANDSAT", .getNumeric(meta$PRODUCT_METADATA["SPACECRAFT_ID",]))},
					SENSOR_ID 			= meta$PRODUCT_METADATA["SENSOR_ID",]	,			
					SCENE_ID 			= meta$METADATA_FILE_INFO["LANDSAT_SCENE_ID",],  ## could assemble name for legacy files: http://landsat.usgs.gov/naming_conventions_scene_identifiers.php
					DATA_TYPE			= if(!legacy) meta$PRODUCT_METADATA["DATA_TYPE",] else meta$PRODUCT_METADATA["PRODUCT_TYPE",],
					ACQUISITION_DATE	= {date <- if(!legacy) meta$PRODUCT_METADATA["DATE_ACQUIRED",] else meta$PRODUCT_METADATA["ACQUISITION_DATE",]},
					PROCESSING_DATE		= if(!legacy) meta$METADATA_FILE_INFO["FILE_DATE",] else meta$METADATA_FILE_INFO["PRODUCT_CREATION_TIME",], 
					PATH				= as.numeric(meta$PRODUCT_METADATA["WRS_PATH",]),
					ROW					= if(!legacy) as.numeric(meta$PRODUCT_METADATA["WRS_ROW",]) else as.numeric(meta$PRODUCT_METADATA["STARTING_ROW",]),
					RADIOMETRIC_RES		= if(SAT == "LANDSAT8") 16 else 8,				
					FILES				= {files <- row.names(meta[["PRODUCT_METADATA"]])[grep("^.*FILE_NAME", row.names(meta$PRODUCT_METADATA))]
						files <- files[grep("^.*BAND",files)]
						files <- meta[["PRODUCT_METADATA"]][files,]	},
					
					BANDS 				= {junk <- unique(sapply(str_split(files, "_B"), "[" ,1 ))
						bds <- str_replace(str_replace(files, paste0(junk,"_"), ""), {if(SAT=="LANDSAT5") "0.TIF" else ".TIF"}, "")
					},
					BAND_TYPE 			= {
						ty <- rep("image", length(bds))
						ty[grepl("QA", bds)] <- "qa"
						ty
					},
					## INSOLATION
					NA_VALUE 			= rep(0, length(ty)),
					SUN_AZIMUTH			= if(!legacy) as.numeric(meta$IMAGE_ATTRIBUTES["SUN_AZIMUTH",]) else as.numeric(meta$PRODUCT_PARAMETERS["SUN_AZIMUTH",]),
					SUN_ELEVATION		= if(!legacy) as.numeric(meta$IMAGE_ATTRIBUTES["SUN_ELEVATION",]) else as.numeric(meta$PRODUCT_PARAMETERS["SUN_ELEVATION",]),
					EARTH_SUN_DISTANCE  = {es <- meta$IMAGE_ATTRIBUTES["EARTH_SUN_DISTANCE",]
						if(is.null(es) || is.na(es)) es <- .ESdist(date)
						as.numeric(es)}
			)
			
			## RADIOMETRIC CORRECTION/RESCALING PARAMETERS
			RADCOR <-  if(!legacy) { list(		
								RAD_OFFSET				= {
									r <- meta$RADIOMETRIC_RESCALING
									r[,1]		<- as.numeric(r[,1])
									bandnames	<- str_c("B", str_replace(rownames(r), "^.*_BAND_", ""))
									go			<- grep("RADIANCE_ADD*", rownames(r))
									ro 			<- r[go,]
									names(ro)	<- bandnames[go]
									ro},
								RAD_GAIN				= {go			<- grep("RADIANCE_MULT*", rownames(r))
									ro 			<- r[go,]
									names(ro)	<- bandnames[go]
									ro},
								REF_OFFSET				= {	go			<- grep("REFLECTANCE_ADD*", rownames(r))
									ro 			<- r[go,]
									names(ro)	<- bandnames[go]
									ro},
								REF_GAIN				= {go			<- grep("REFLECTANCE_MULT*", rownames(r))
									ro 			<- r[go,]
									names(ro)	<- bandnames[go]
									ro})
										
					} else {
						
						bandnames <- paste0("B", .getNumeric(rownames(meta$MIN_MAX_RADIANCE)))
						bandnames <- bandnames[seq(1, length(bandnames), 2)]
						
						L <- diff(as.numeric(meta$MIN_MAX_RADIANCE[,1]))
						L <- L[seq(1, length(L), 2)] 
						
						Q <- diff(as.numeric(meta$MIN_MAX_PIXEL_VALUE[,1]))  
						Q <- Q[seq(1, length(Q), 2)]
						
						RAD_GAIN	<- L/Q
						RAD_OFFSET 	<- as.numeric(meta$MIN_MAX_RADIANCE[,1])[seq(2,nrow(meta$MIN_MAX_RADIANCE),2)] - (RAD_GAIN) * 1
						
						names(RAD_OFFSET) <- names(RAD_GAIN) <- bandnames
												
						list(RAD_OFFSET = RAD_OFFSET, RAD_GAIN = RAD_GAIN)
						
					}
			
	 if(SAT == "LANDSAT8"){
				RADCOR$K1 ={ r <- meta$TIRS_THERMAL_CONSTANTS
					r[,1]		<- as.numeric(r[,1])
					bandnames	<- str_c("B", str_replace(rownames(r), "^.*_BAND_", ""))
					go			<- grep("K1", rownames(r))
					ro 			<- r[go,]
					names(ro)	<- bandnames[go]
					ro}
				RADCOR$K2 = {go			<- grep("K2", rownames(r))
					ro 			<- r[go,]
					names(ro)	<- bandnames[go]
					ro}				
			} else {
				TAB7 <- list(LANDSAT4 = c(B6=671.62,B6=1284.3), # TAB7 from Chander 2009
						LANDSAT5 = c(B6=607.76,B6=1260.56),
						LANDSAT7 = c(B6=666.09,B6=1282.71))
					
				RADCOR$K1 <- TAB7[[SAT]][1]
				RADCOR$K2 <- TAB7[[SAT]][2]
			}
			
			meta[["UNIFIED_METADATA"]] <- c(meta[["UNIFIED_METADATA"]], RADCOR)
		}
	} else {
		## PROCESS ESPA LEDAPS XML FILES
		meta <- xmlParse(file)
		meta <- xmlToList(meta)
		names(meta$bands) <- str_replace_all(unlist(sapply(meta$bands, "[", "long_name")), " ", "_")
		
		if(unifiedMetadata){
			
			atts <- sapply(meta$bands, "[", ".attrs")
			
			meta[["UNIFIED_METADATA"]] <- list(
					SPACECRAFT_ID 		= {SAT <- paste0("LANDSAT", .getNumeric(meta$global_metadata$satellite))},
					SENSOR_ID 			= meta$global_metadata$instrument,			
					SCENE_ID 			= SID <- str_replace(meta$global_metadata$lpgs_metadata_file, "_MTL.txt", ""),  ## could assemble name for legacy files: http://landsat.usgs.gov/naming_conventions_scene_identifiers.php
					DATA_TYPE			= if(meta$bands[[1]]$.attrs["product"] == "sr_refl") "SR", 
					ACQUISITION_DATE	= {date <- meta$global_metadata$acquisition_date},
					PROCESSING_DATE		= meta$bands[[1]]$production_date, 
					PATH				= as.numeric(meta$global_metadata$wrs["path"]),
					ROW					= as.numeric(meta$global_metadata$wrs["row"]),
					
					FILES				= {files <- sapply(meta$bands, "[[", "file_name")
						names(files) <- NULL
						files},					
					BANDS 				= {	
						bds <- grepl("_band", files)
						toa <- grepl("_toa_", files)
						qas <- grepl("qa", files)	
						bnames				<- toupper(str_replace(files, paste0(SID, "_"), ""))					
						bnames[bds]			<- paste0("B", .getNumeric(bnames[bds]))
						bnames[bds & qas] 	<- paste0(bnames[bds & qas], "_QA")
						bnames				<- str_replace(str_replace(str_replace(bnames, "\\.TIF", ""), "SR_", ""), "TOA_", "")
						bnames[toa] 		<- paste0(bnames[toa], "_TOA")
						bnames
					},
					BAND_TYPE			= {ty <- sapply(atts, "[" , "category")
						names(ty) <- NULL
						ty
					},
					NA_VALUE 			= as.numeric(sapply(atts, "[" , "fill_value")),
					SATURATE_VALUE 		= as.numeric(sapply(atts, "[" , "saturate_value")),
					SCALE_FACTOR 		= as.numeric(sapply(atts, "[" , "scale_factor")),
					
					SUN_AZIMUTH			= as.numeric(meta$global_metadata$solar_angles["azimuth"]),
					SUN_ELEVATION		= 90 - as.numeric(meta$global_metadata$solar_angles["zenith"]),
					EARTH_SUN_DISTANCE  = {.ESdist(date)}
			)
			
		}
		
	}
	return(meta)
}





#' Import separate Landsat files into single stack
#' 
#' Reads Landsat MTL or XML metadata files and loads single Landsat Tiffs into a rasterStack.
#' Be aware that by default stackLS() does NOT import panchromatic bands nor thermal bands with resolutions != 30m.
#' 
#' @param file character. Path to Landsat MTL metadata file.
#' @param allResolutions logical. if \code{TRUE} a list will be returned with length = unique spatial resolutions.
#' @param resampleTIR logical. As of  the USGS resamples TIR bands to 30m. Use this option if you use data processed prior to February 25, 2010 which has not been resampled.
#' @param resamplingMethod character. Method to use for TUR resampling ('ngb' or 'bilinear'). Defaults to 'ngb' (nearest neighbor).
#' @param products character vector. Which products should be returned in the stack? (only relevant for LS8 and LEDAPS processed products). 'image': image data, 'index': multiband indices, 'qa' quality flag bands. 
#' @return Either a list of rasterStacks comprising all resolutions or only one rasterStack comprising only 30m resolution imagery
#' @note 
#' Be aware that by default stackLS() does NOT import panchromatic bands nor thermal bands with resolutions != 30m. Use the allResolutions argument to import all layers.
#' 
#' The USGS uses cubic convolution to resample TIR bands to 30m resolution. In the opinion of the author this may not be the best choice for supersampling. 
#' Therefore the default method in this implementation is nearest neighbor. Keep this in mind if you plan to compare TIR bands created by differing resampling routines.
#' Typically, however, you will already have the USGS 30m TIR products, so no need to worry...
#' @export
stackMeta <- function(file, allResolutions = FALSE,  resampleTIR = FALSE, resamplingMethod = "ngb", products = c("image", "index", "qa")){
	
	## Read metadata and extract layer file names
	meta  <- readMeta(file)
	files <- meta$UNIFIED_METADATA$FILES
	
	## Load layers
	path  <- if(basename(file) != file)  str_replace(file, basename(file), "") else NULL
	
	## Import rasters
	rl <- lapply(paste0(path, files), raster)
	resL <- lapply(lapply(rl, res),"[", 1)
	
	if(any(resL > 30)) {
		message("Your Landsat data includes TIR band(s) which were not resampled to 30m.
						\nYou can set resampleTIR = TRUE to resample TIR bands to 30m if you want a single stack")
		
		## Resample TIR to 30m
		if(resampleTIR){
			for(i in which(resL > 30))
				rl[[i]] <- resample(rl[[i]], rl[[which(resL == 30)[1]]], method = resamplingMethod)		
		}
	}
	
	## Stack
	returnRes <- if(allResolutions) unlist(unique(resL)) else 30
	
	LS 	<- lapply(returnRes, function(x){
				s			<- stack(rl[resL == x])
				names(s) 	<- meta$UNIFIED_METADATA$BANDS[resL == x]
				NAvalue(s)	<- meta$UNIFIED_METADATA$NA_VALUE[resL == x]	
				s <- s[[ which(names(s) %in% meta$UNIFIED_METADATA$BANDS[meta$UNIFIED_METADATA$BAND_TYPE %in% products])	]]
				s
			})
	
	if(!allResolutions) LS <- LS[[1]]
	
	return(LS)
}relnorm <-
function(master, tofix, mask, method="MA", nperm=1000)
{
    # relative normalization of tofix to match master
    # based on values in mask and using regression
    # method OLS, MA, or SMA
    # mask should contain NA for values to include; all other values will be omitted

    results <- tofix

    master <- as.vector(as.matrix(master))
    tofix <- as.vector(as.matrix(tofix))

    if(missing(mask)) { 
        mask <- rep(NA, length(master))
    } else {
        mask <- as.vector(as.matrix(mask))
    }

    master.mask <- master[is.na(mask)]
    x.mask <- tofix[is.na(mask)]

    master.lm <- lmodel2(master.mask ~ x.mask, nperm=nperm)
    
    master.lm <- master.lm$regression.results[master.lm$regression.results[, "Method"] == method, ]
    names(master.lm) <- gsub("^ *", "", names(master.lm))
    
    x.transform <- master.lm$Slope * tofix + master.lm$Intercept
    
    x.transform[!is.na(mask)] <- NA


    # return the same structure as the input values
    if(class(results) == "SpatialGridDataFrame")
        results@data[,1] <- x.transform
    else if(is.data.frame(results))
        results <- data.frame(matrix(x.transform, nrow=nrow(results), ncol=ncol(results)))
    else if(is.matrix(results))
        results <- matrix(x.transform, nrow=nrow(results), ncol=ncol(results))
    else # return a vector 
        results <- x.transform

    list(regression.results = master.lm, newimage = results)
}

#' RStoolbox: A Collection of Remote Sensing Tools
#'
#' @name RStoolbox
#' @import raster rgeos geosphere plyr randomForest stringr
#' @docType package
NULL
#!/bin/bash

source ${ciop_job_include}

tsrs="`ciop-getparam tsrs`"

INPUTDIR=$TMPDIR/input
OUTPUTDIR=$TMPDIR/output

mkdir -p $INPUTDIR $OUTPUTDIR 

while read product
do

  ciop-log "INFO" "Getting product from $product"

  retrieved=`ciop-copy -U -o $INPUTDIR $product`

  ciop-log "DEBUG" "The local product is $retrieved"

  for band in 4 5 6
  do
  	tar tfz $retrieved | grep B$band.TIF | xargs tar -C $INPUTDIR -zxvf $retrieved 1>&2
  done  

  ciop-log "DEBUG" "`tree $TMPDIR`"

  for tif in `ls $INPUTDIR/*.TIF`
  do
    gdalwarp  -t_srs $tsrs $tif $OUTPUTDIR/proj_`basename $tif` 1>&2
  done

ciop-log "DEBUG" "`tree $TMPDIR`"

  b4="`find $OUTPUTDIR -name "proj_*B4*.TIF"`"
  b5="`find $OUTPUTDIR -name "proj_*B5*.TIF"`"
  b6="`find $OUTPUTDIR -name "proj_*B6*.TIF"`"

  formula="(A-B)/(A+B)"

  ciop-log "INFO" "new branch is cool: $b4 $b5 $b6"
  gdal_calc.py -A $b4 -B $b5 --outfile=$NDVIDIR/ndvi.tif --calc=$formula
  gdal_calc.py -A $b5 -B $b6 --outfile=$NDVIDIR/ndbi.tif --calc=$formula


  ciop-publish -m $NDVIDIR/ndvi.tif
  ciop-publish -m $NDVIDIR/ndbi.tif

  rm -fr $OUTPUTDIR/* $INPUTDIR/* 
done

#' Generate random sample polygons from a raster layer, optionally with 
#' stratification
#'
#' Useful for gathering training data for an image classification. With the 
#' default settings, the output polygons will be perfectly aligned with the 
#' pixels in the input raster.
#'
#' @export
#' @importFrom rgdal writeOGR
#' @param x a \code{Raster*}
#' @param size the sample size (number of sample polygons to return)
#' @param side desired length for each side of the sample polygon (units of the 
#' input \code{Raster*}, usually meters)
#' @param strata (optional) a \code{RasterLayer} of integers giving the strata 
#' of each pixel (for example, a classified image)
#' @param fields a list of fields to include in the output 
#' \code{SpatialPolygonsDataFrame} (such as a "class" field if you will be 
#' digitizing classes).
#' @param na.rm whether to remove pixels with NA values from the sample
#' @param exp multiplier used to draw larger initial sample to account for the 
#' loss of sample polygons lost because they contain NAs, and, for stratified 
#' sampling, to account for classes that occur very infrequently in the data.  
#' Increase this value if the final sample has fewer sample polygons than 
#' desired.
#' @return a \code{SpatialPolygonsDataFrame}
#' @examples
#' \dontrun{
#' set.seed(0)
#' L5TSR_1986_b1 <- raster(L5TSR_1986, layer=1)
#' training_polys <- sample_raster(L5TSR_1986_b1, 30,
#'                                   side=6*xres(L5TSR_1986_b1))
#' plot(L5TSR_1986_b1)
#' plot(training_polys, add=TRUE)
#' }
sample_raster <- function(x, size, strata=NULL, side=xres(x), fields=c(), 
                          na.rm=TRUE, exp=5) {
    if (!is.null(strata)) {
        stratified <- TRUE
        if (proj4string(strata) != proj4string(x)) {
            stop('x and strata must have the same coordinate system')
        }
        if (!identical(extent(strata), extent(x))) {
            stop('x and strata must have the same extent')
        }
        if (!identical(res(strata), res(x))) {
            stop('x and strata must have the same resolution')
        }
        strat_sample <- sampleStratified(strata, size, exp=exp, na.rm=na.rm)
        cell_nums <- strat_sample[, 1]
        strataids <- strat_sample[, 2]
    } else {
        stratified <- FALSE
        cell_nums <- sampleRandom(x, size, exp=exp, na.rm=na.rm)
    }

    xy <- xyFromCell(x, cell_nums)
    # Convert from cell-center coordinates to ul corner of cell coordinates
    xy[, 1] <- xy[, 1] - xres(x)/2
    xy[, 2] <- xy[, 2] + yres(x)/2
    # Coordinate order is: ll, lr, ur, ul, ll. Need to end with ll to close the 
    # polygon
    xcoords <- cbind(xy[, 1],
                     xy[, 1] + side,
                     xy[, 1] + side,
                     xy[, 1],
                     xy[, 1])
    ycoords <- cbind(xy[, 2] - side,
                     xy[, 2] - side,
                     xy[, 2],
                     xy[, 2],
                     xy[, 2] - side)
    xycoords <- array(cbind(xcoords, ycoords),
                      dim=c(nrow(xcoords), ncol(xcoords), 2))

    # TODO: Add check to ensure no polygons overlap, and warn if they fall 
    # outside raster extent

    # Function to make individual polygons for each sample area
    make_Polygon <- function(slice) {
        list(Polygon(cbind(x=slice[, 1], y=slice[, 2])))
    }
    polys <- apply(xycoords, c(1), make_Polygon)
    # Now convert the list of Polygon objects to a list of Polygons objects 
    # (notice the trailing "s" in "Polygons")
    polys <- mapply(function(poly, ID) Polygons(poly, ID=ID),
                    polys, seq(1, length(polys)))
    Sr <- SpatialPolygons(polys, proj4string=CRS(proj4string(x)))
    if (stratified) {
        out_data <- data.frame(ID=names(Sr), strata=strataids)
    } else {
        out_data <- data.frame(ID=names(Sr))
    }
    for (field in fields) {
        out_data <- cbind(out_data, rep('', nrow(out_data)))
        names(out_data)[ncol(out_data)] <- field
    }
    # Finally, convert to a SpatialPolygonsDataFrame
    polys <- SpatialPolygonsDataFrame(Sr, data=out_data)

    if (nrow(polys) < size) {
        warning('length of polys < size. Try increasing exp.')
    }

    return(polys)
}
#' Scales a \code{Raster*} by a power of a given integer and rounds to nearest 
#' integer
#'
#' Useful for scaling and (optionally) rounding a \code{RasterLayer} to integer 
#' so that a layer can be saved as an integer datatype such as "INT1U", 
#' "INT1S", "INT2" or "INT2S".
#'
#' This function will run in parallel if a parallel backend is registered with 
#' \code{\link{foreach}}.
#'
#' @export scale_raster
#' @import methods
#' @seealso \code{\link{dataType}}
#' @param x a \code{Raster*} object
#' @param power_of raster will be scaled using the highest possible power of 
#' this number
#' @param max_out the scaling factors will be chosen for each layer to ensure 
#' that the maximum and minimum (if minimum is negative) values of each layer 
#' do not exceed \code{max_out}
#' @param do_scaling perform the scaling and return a \code{Raster*} (if 
#' \code{do_scaling} is TRUE) or return a list of scale factors (if 
#' \code{do_scaling} is FALSE)
#' @param round_output whether to round the output to the nearest integer
#' @return a \code{Raster*} if \code{do_scaling} is TRUE, or a list of scaling 
#' factors if \code{do_scaling} is false.
setGeneric("scale_raster", function(x, power_of=10, max_out=32767, 
                                    round_output=TRUE, do_scaling=TRUE) {
    standardGeneric("scale_raster")
})

scale_layer <- function(x, power_of, max_out, round_output, do_scaling) {
    if (!x@data@haveminmax) {
        warning('no stored minimum and maximum values - running setMinMax')
        x <- setMinMax(x)
    }
    layer_max <- max(abs(c(minValue(x), maxValue(x))))
    scale_factor <- power_of ^ floor(log(max_out / layer_max, base=power_of))
    if (do_scaling) {
        x <- calc(x, function(vals, ...) {
                  vals <- vals * scale_factor
                  if (round_output) vals <- round(vals)
                  vals
                  })
        return(x)
    } else {
        return(scale_factor)
    }
}

#' @rdname scale_raster
#' @aliases scale_raster,RasterLayer,ANY-method
setMethod("scale_raster", signature(x="RasterLayer"),
    function(x, power_of, max_out, round_output, do_scaling) {
        ret <- scale_layer(x, power_of, max_out, round_output, do_scaling)
        names(ret) <- names(x)
        return(ret)
    }
)

#' @import foreach
scale_stack_or_brick <- function(x, power_of, max_out, round_output, do_scaling) {
    unscaled_layer=NULL
    if (do_scaling) {
        scale_outputs <- foreach(unscaled_layer=unstack(x), 
                                 .combine='addLayer', .multicombine=TRUE, 
                                 .init=raster(), .packages=c('teamlucc'),
                                 .export=c('scale_layer')) %dopar% {
            scale_output <- scale_layer(unscaled_layer, power_of, max_out, 
                                        round_output, do_scaling)
        }
    } else {
        scale_outputs <- foreach(unscaled_layer=unstack(x), 
                                 .packages=c('raster', 'teamlucc'),
                                 .export=c('scale_layer')) %dopar% {
            scale_output <- scale_layer(unscaled_layer, power_of, max_out, 
                                        round_output, do_scaling)
        }
    }
    names(scale_outputs) <- names(x)
    return(scale_outputs)
}

#' @rdname scale_raster
#' @aliases scale_raster,RasterStack,ANY-method
setMethod("scale_raster", signature(x="RasterStack"),
    function(x, power_of, max_out, round_output, do_scaling) {
        ret <- scale_stack_or_brick(x, power_of, max_out, round_output, 
                                    do_scaling)
        return(ret)
    }
)

#' @rdname scale_raster
#' @aliases scale_raster,RasterBrick,ANY-method
setMethod("scale_raster", signature(x="RasterBrick"),
    function(x, power_of, max_out, round_output, do_scaling) {
        ret <- scale_stack_or_brick(x, power_of, max_out, round_output, 
                                    do_scaling)
        return(ret)
    }
)
"sitecode","wrspath","wrsrow","do_tc"
"BBS","123","064",FALSE
"BBS","124","064",TRUE
"BBS","124","063",TRUE
"BBS","125","063",TRUE
"BCI","012","053",TRUE
"BCI","012","054",TRUE
"BIF","172","061",TRUE
"BIF","173","061",TRUE
"BIF","173","060",TRUE
"CAX","225","061",FALSE
"CAX","225","062",FALSE
"CAX","226","061",FALSE
"COU","003","068",FALSE
"COU","003","069",TRUE
"COU","004","068",TRUE
"COU","004","069",TRUE
"COU","005","068",TRUE
"CSN","229","056",FALSE
"CSN","229","057",TRUE
"CSN","229","058",TRUE
"CSN","230","057",TRUE
"CSN","230","058",FALSE
"KRP","187","056",TRUE
"KRP","187","057",TRUE
"MAS","230","061",FALSE
"MAS","230","062",FALSE
"MAS","231","061",FALSE
"MAS","231","062",FALSE
"MAS","232","061",FALSE
"NAK","127","047",TRUE
"NAK","127","048",TRUE
"NAK","128","047",TRUE
"NNN","181","058",FALSE
"NNN","181","059",FALSE
"NNN","182","058",FALSE
"NNN","182","059",FALSE
"PSH","127","057",TRUE
"PSH","127","058",TRUE
"PSH","126","058",TRUE
"RNF","158","075",TRUE
"RNF","159","075",TRUE
"RNF","159","074",TRUE
"UDZ","167","065",TRUE
"UDZ","167","066",TRUE
"UDZ","168","065",TRUE
"UDZ","168","066",TRUE
"VB","014","053",TRUE
"VB","015","053",TRUE
"VB","015","052",TRUE
"YAN","006","068",TRUE
"YAN","006","067",TRUE
"YAN","007","067",TRUE
"YAS","008","061",FALSE
"YAS","008","060",FALSE
"YAS","009","061",FALSE
"YAS","009","060",FALSE
#' Count number of vertices in an sp polygon object
#'
#' @param poly_obj an sp polygon object
#' @return the number of vertices in the polygon
#' @examples
#' # TODO: Write an example.
nverts <- function(poly_obj) {
    n_verts <- sapply(poly_obj@polygons, function(y) 
                      nrow(y@Polygons[[1]]@coords))[[1]]
    if (is.null(n_verts)) {
        n_verts <- 0
    } else {
        # Need to subtract one as the starting coordinate and ending coordinate 
        # are identical, but appear twice - at beginning and at end of list.
        n_verts <- n_verts - 1
    }
    return(n_verts)
}

#' Simplify a polygon to contain less than a certain number of vertices
#'
#' Useful for simplifying area of interest (AOI) polygons for use with online 
#' data portals (like USGS EarthExplorer) that limit the number of vertices 
#' allowed in uploaded AOI shapefiles.
#'
#' @export
#' @importFrom rgeos gSimplify
#' @param poly_obj a polygon as an sp object
#' @param max_vertices the maximum number of vertices to allow in the 
#' simplified polygon
#' @param maxit the maximum number of iterations to use to simplify the polygon
#' @param multiplier a number used to increase the tolerance for each 
#' subsequent iteration, if the number of vertices in the simplified polygon is 
#' not less than /code{max_vertices}
#' @param initial_tolerance initial value for tolerance used to remove vertices 
#' from polygon.  If set to the default option, "dynamic", the code will 
#' automatically set the \code{initial_tolerance} to .01 * the length of the 
#' diagonal of the bounding box of the polygon. \code{initial_tolerance} can 
#' also be set to an arbitrary value, in the same units as the polygon object.
#' @return polygon with less than \code{max_vertices} vertices
#' @examples
#' # TODO: add an example
simplify_polygon <- function(poly_obj, max_vertices, maxit=100, 
                             multiplier=1.05, initial_tolerance='dynamic') {
    if (class(poly_obj) == 'SpatialPolygonsDataFrame') {
        poly_data <- poly_obj@data
    } else {
        poly_data <- NULL
    }
    n_parts <- sapply(poly_obj@polygons, function(x) length(x))
    if (length(n_parts) > 1)
        stop('poly_obj contains more than one polygon')
    else if (n_parts > 1)
        stop('poly_obj polygon is a multipart polygon')

    if (initial_tolerance == 'dynamic') {
        # Set the initial tolerance as the extent / 100
        ext <- extent(poly_obj)
        diag_seg_length <- sqrt((ext@xmax - ext@xmin)**2 +
                                (ext@ymax - ext@ymin)**2)
        tolerance <- diag_seg_length / 100
    } else {
        tolerance <- initial_tolerance
    }

    # Iterate, increasing tolerance, until polygon has less than maxpts 
    # vertices.
    n_verts <- nverts(poly_obj)
    n <- 0
    while ((n_verts > 0) && (n < maxit) && (n_verts > max_vertices)) {
        poly_obj <- gSimplify(poly_obj, tol=tolerance)
        n_verts <- nverts(poly_obj)
        tolerance <- tolerance * multiplier 
        n <- n + 1
    }
    if (n == maxit)
        warning(paste('Reached maximum iterations (', maxit, ')', sep=''))
    if (n_verts == 0)
        warning(paste('Simplified polygon has no vertices.'))
    else if (n_verts <= 2)
        warning(paste('Simplified polygon has only', n_verts, 'vertices.'))

    if (!is.null(poly_data)) {
        poly_obj <- SpatialPolygonsDataFrame(poly_obj, data=poly_data)
    }

    return(poly_obj)
}
Site.Name.Code,Site.Name,Site.Name.Short,Site.Type,Site.Country,Site.Continent
BBS,Bukit Barisan,Bukit Barisan,II,PAN,LA
BCI,Barro Colorado Nature Monument - Soberania National Park,Barro Colorado - Soberania,II/III,IDN,AS
BIF,Bwindi Impenetrable Forest,Bwindi,III,UGA,AF
CAX,Caxiuanã,Caxiuanã,I,BRA,LA
COU,Cocha Cashu - Manu National Park,Cocha Cashu - Manu,I,PER,LA
CSN,Central Suriname Nature Reserve,Central Suriname,I,SUR,LA
KRP,Korup National Park,Korup,II,CMR,AF
MAS,Manaus,Manaus,II,BRA,LA
NAK,Nam Kading,Nam Kading,III,LAO,AS
NNN,Nouabalé Ndoki,Nouabalé Ndoki,I,COG,AF
PSH,Pasoh Forest Reserve,Pasoh,III,MYS,AS
RNF,Ranomafana,Ranomafana,III,MDG,AF
UDZ,Udzungwa,Udzungwa,II,TZA,AF
VB,Volcán Barva,Volcán Barva,III,CRI,LA
YAN,Yanachaga Chimillén National Park,Yanachaga Chimillén,I,PER,LA
YAS,Yasuni,Yasuni,I,ECU,LA
slopeasp <- function (x, EWres, NSres, EWkernel, NSkernel, smoothing = 1) 
{
    if(class(x) == "SpatialGridDataFrame") {
    	xmat <- t(as.matrix(x))
    }
    else {
       xmat <- as.matrix(x)
    }
    if (missing(EWres)) {
        if (class(x) == "SpatialGridDataFrame") {
            EWres <- x@grid@cellsize[1]
        }
        else {
            stop("EWres must be specified if x is not a SpatialGridDataFrame.\n")
        }
    }
    if (missing(NSres)) {
        if (class(x) == "SpatialGridDataFrame") {
            NSres <- x@grid@cellsize[2]
        }
        else {
            stop("NSres must be specified if x is not a SpatialGridDataFrame.\n")
        }
    }
    if (missing(EWkernel)) {
        EWkernel <- matrix(c(-1/8, 0, 1/8, -2/8, 0, 2/8, -1/8, 
            0, 1/8), ncol = 3, nrow = 3, byrow = TRUE)
    }
    EW.mat <- movingwindow(xmat, EWkernel)/EWres
    if (missing(NSkernel)) {
        NSkernel <- matrix(c(1/8, 2/8, 1/8, 0, 0, 0, -1/8, -2/8, 
            -1/8), ncol = 3, nrow = 3, byrow = TRUE)
    }
    NS.mat <- movingwindow(xmat, NSkernel)/NSres
    slope <- atan(sqrt(EW.mat^2 + NS.mat^2)/smoothing)
    slope <- (180/pi) * slope
    aspect <- 180 - (180/pi) * atan(NS.mat/EW.mat) + 90 * (EW.mat/abs(EW.mat))
    aspect[slope == 0] <- 0
    if (class(x) == "SpatialGridDataFrame") {
        temp <- x
        temp@data[, 1] <- as.vector(t(aspect))
        aspect <- temp
        temp@data[, 1] <- as.vector(t(slope))
        slope <- temp
    }
    list(slope = slope, aspect = aspect)
}

#' Spectral indices
#' 
#' @param inputRaster Raster* object. Typically remote sensing imagery, which is to be classified.
#' @param indices character. one or more spectral indices 
#' @param sensor if a sensor is specified \code{bands} is populated automatically. Specifying a sensor requires the layernames in inputRaster to match the official band designations formatted as "B1", "B2" etc.
#' @param bands list of band designations. See notes for details
#' @param maskRaster Raster layer containing a binary mask to exclude areas from prediction.
#' @param verbose logical. prints progress, statistics and graphics during execution
#' @param ... further arguments such as filename etc. passed to \link[raster]{writeRaster}
#' @return rasterBrick or rasterStack
#' @seealso \code{\link[raster]{overlay}} 
#' @export
#' @examples
#' r <- raster(ncol=10,nrow=10)
#' r[] <- sample(1:155, 100, TRUE)
#' r <- stack(r, r + 90 + rnorm(100, 10)) 
#' names(r) <- c("red", "nir")
#' SI <- spectralIndices(r, indices = c("SR", "NDVI"), bands = list(NIR = "nir", RED = "red"))
#' plot(SI)
spectralIndices <- function(inputRaster, indices = "NDVI", sensor, bands , maskRaster = NULL, verbose = FALSE, ... ) {
	# TODO: add indices
	# TODO: add examples
	# TODO: add formulas to help file
	# TODO: internal sensor db
	# TODO: value checks?
	# TODO: check sensor list for correctness and extend it 
	
	## Sensor db
	SENSORS <- list(
			LANDSAT5 = list(BLUE = "B1", GREEN = "B2", RED = "B3", NIR = "B4", MIR = "B7"),
			LANDSAT7 = list(BLUE = "B1", GREEN = "B2", RED = "B3", NIR = "B4"),
			LANDSAT8 = list(BLUE = "B2", GREEN = "B3", RED = "B4", NIR = "B5")
	)
	
	if(!missing(sensor)){
		if(!sensor %in% names(SENSORS)) stop(paste0("Unknown sensor. Please provide the 'bands' argument or 'sensor' as one of ", names(SENSORS)))
		bands <- SENSORS[[sensor]]
		if(any(!bands %in% names(inputRaster))) stop("Bandnames of inputRaster do not match the required format or are missing. Please provide 'bands' argument manually or make sure the names(inputRaster) follow the 'B1' 'B2'  ... format if you want to make use of the 'sensor' argument.")
	}
	bands <- lapply(bands, function(x) if(is.character(x)) which(names(inputRaster) == x) else x )
	
	## Internal db
	INDICES <-  list(
			SR 		= function(NIR, RED) {NIR / RED},
			DVI		= function(NIR, RED) {NIR-RED},
			NDVI	= function(NIR, RED) {(NIR-RED)/(NIR+RED)}, 
			TVI 	= function(NIR, RED) {(((NIR-RED)/(NIR+RED))+0.5)^0.5}, 
			MSAVI	= function(NIR, RED) {NIR + 0.5 - (0.5 * sqrt((2 * NIR + 1)^2 - 8 * (NIR - (2 * RED))))},
			MSAVI2	= function(NIR, RED) {(2 * (NIR + 1) - sqrt((2 * NIR + 1)^2 - 8 * (NIR - RED))) / 2},
			GEMI	= function(NIR, RED) {(((NIR^2 - RED^2) * 2 + (NIR * 1.5) + (RED * 0.5) ) / (NIR + RED + 0.5)) * (1 - ((((NIR^2 - RED^2) * 2 + (NIR * 1.5) + (RED * 0.5) ) / (NIR + RED + 0.5)) * 0.25)) - ((RED - 0.125) / (1 - RED))},                   
			SLAVI	= function(RED, MIR) {NIR / (RED + MIR)},
			EVI		= function(NIR, RED, BLUE) {G * ((NIR - RED) / (NIR + C1 * RED - C2 * BLUE + L))}# include a G or L specification in command
	)
	
	## Get arguments and check for mising arguments
	args <- lapply(indices, function(index) {
				need <- names(formals(INDICES[[index]]))	
				if(any(!need %in% names(bands))) stop("Band specification(s) of >> ", paste(names(bands)[!names(bands) %in% need], collapse = ","), 
							" << are missing or do not match layer names in the brick/stack. \nPlease specify the correct layer number or name in a list, e.g. bands = list(RED = 'B4', NIR = 'B5')", call. = FALSE)
				need <- unlist(bands[need])
			})
	names(args) <- indices 
	
	## We do this in a separate step, so we can throw an error before we start the calculations
	inList <- lapply(indices, function(index) {
				if(verbose) print(paste0("Calculating ", index))
			m<-	overlay(inputRaster[[args[[index]]]], fun = INDICES[[index]])
			})
	
	## Combine and return
	outStack <- stack(inList)
		
	## Write file if filename is provided. Doing it this way we write the file twice. We could provide filenames to overlay instead and return a stack so we only write once. 
	## But then we have an output of n single files instead of one multi-layer file containing all indices.
	## Maybe we should make this optional
	if(any(grepl("file", names(list(...))))) outStack <-  writeRaster(outStack, ...)
	
	names(outStack) <- indices	 

	return(outStack)
}#' Split classes in a training dataset using normal mixture modeling
#'
#' This function can be used to aid in classifying spectrally diverse classes 
#' by splitting the input classes into subclasses using a clustering algorithm.  
#' After classification, these subclasses are merged back into their original 
#' parent classes. For example, the training data for an agriculture class 
#' might have both fallow and planted fields in the training data, or fields 
#' planted with different crops that are spectrally dissimilar.  This function 
#' can be used to automatically split the agriculture class into a number of 
#' subclasses.  The classifier is then run on this larger set of classes, and 
#' following classification, these subclasses can all be merged together into a 
#' single overall agriculture class.
#'
#' @export
#' @importFrom mclust Mclust
#' @param train_data a \code{link{pixel_data}} object
#' @param split_levels (optional) a list giving the names of the levels to 
#' split. If missing, all levels will be split.
#' @param verbose whether to report status while running
split_classes <- function(train_data, split_levels, verbose=FALSE) {
    y_reclass <- vector('numeric', nrow(train_data@x))
    if (missing(split_levels)) {
        split_levels <- levels(train_data)
    }
    for (level in split_levels) {
        level_ind <- train_data@y == level
        model <- Mclust(train_data@x[level_ind, ])
        y_reclass[level_ind]  <- paste(train_data@y[level_ind], 
                                       model$classification, sep='_clust')
        if (verbose) print(paste(level, 'split into', model$g, 'classes'))
    }
    y <- factor(y_reclass)
    reclass_mat <- data.frame(split_name=levels(y))
    reclass_mat$split_id <- as.numeric(reclass_mat$split_name)
    reclass_mat$name <- gsub('_clust[0-9]*$', '', reclass_mat$split_name)
    reclass_mat$id <- match(reclass_mat$name, unique(reclass_mat$name))
    return(list(reclass_mat=reclass_mat, y=factor(y_reclass)))
}
#' Supervised Classification
#' 
#' @param inputRaster Raster* object. Typically remote sensing imagery, which is to be classified.
#' @param trainingData SpatialPolygonsDataFrame containing the training data used to train the classifier.  
#' @param classAttributes character giving the column in \code{trainingData}, which contains the class attribute. Can be omitted, when \code{trainingData} has only one column.
#' @param nSamples number of samples per land cover class
#' @param filename path to output file (optional). If \code{NULL}, standard raster handling will apply, i.e. storage either in memory or in the raster temp directory.
#' @param maskRaster Raster layer containing a binary mask to exclude areas from prediction.
#' @param verbose logical. prints progress, statistics and graphics during execution
#' @param predict logical. \code{TRUE} (default) will return a classified map, \code{FALSE} will only train the classifier
#' @param ... further arguments to be passed to randomForest
#' @return A list containing [[1]] the model, [[2]] the predicted raster and [[3]] the class mapping  
#' @seealso \code{\link{randomForest}} 
#' @export
superClass <- function(inputRaster, trainingData, classAttributes = NULL, nSamples = 100, filename = NULL, maskRaster = NULL, verbose = FALSE, predict = TRUE, overwrite = TRUE, ...) {
	# TODO: point vector data
	# TODO: enable regression mode
	# TODO: cross-validation
	# TODO: make classifier modular
	# TODO: add examples
	# TODO: check applicability of raster:::.intersectExtent 
	# DISCUSS: demo data
	
	## Filetypes
	if(!inherits(inputRaster, 'Raster')) stop("inputRaster must be a raster object (RasterLayer,RasterBrick or RasterStack)", call.=FALSE)
	if(!inherits(trainingData, 'SpatialPolygonsDataFrame')) stop("traingData must be a SpatialPolygonsDataFrame", call.=FALSE)
	
	## Attribute column
	if(is.null(classAttributes)){
		if(ncol(trainingData) == 1) {
			classAttributes <- 1
			message("You did not specify the classAttributes column. \nSince your trainingData only contains one column we assume this is it")
		} else {
			stop(paste("Dont't know which column in trainingData contains the class attribute. \nPlease specify classAttributes as one of: ", paste(colnames(trainingData@data),collapse=", ")), call. = FALSE)
		}
	} 
	if(!classAttributes %in% colnames(trainingData@data)) 
		stop(paste0("The column ", classAttributes, " does not exist in trainingData. \nAvailable columns are: ", colnames(trainingData@data,collapse=", ")), call. = FALSE) 
		
	## Check projections
	if(!compareCRS(inputRaster, trainingData)) 
		stop("Projection of trainingData does not match inputRaster")
		## DISCUSS: Should we do a spTransform of vector data here, or require proper projection from the user?
	
	## Check overlap of vector and raster data	
	if(!gIntersects(as(extent(inputRaster),"SpatialPolygons"), as(extent(trainingData),"SpatialPolygons"))) 
		stop("inputRaster and trainingData do not overlap")
	
	## Calculate area weighted number of samples per polygon
	## this way we'll end up with n > nSamples, but make sure to sample each polygon at least once
	if(is.projected(trainingData)){
		trainingData[["area"]] <- gArea(trainingData, byid = TRUE)
	} else {
		trainingData[["area"]] <- areaPolygon(trainingData)		
	}
	
	## Calculate optimal nSamples per class
	trainingData@data[["order"]] <- 1:nrow(trainingData) 		
	weights <- ddply(trainingData@data, .variables = classAttributes, .fun = here(mutate), nSamplesClass = ceiling(nSamples * area / sum(area)))
	trainingData@data <- weights[order(weights$order),]
		
	## Get random coordinates within polygons
	xy  <- lapply(seq_along(trainingData), function(i_poly){	
				pts <- spsample(trainingData[i_poly, ], type = "random", n = trainingData@data[i_poly,"nSamplesClass"], iter = 20) 
			})
	xy <- do.call("rbind", xy)
	
	### Display, verbose only
	if(verbose) {
		plot(inputRaster,1)
		plot(trainingData, add = T)
		points(xy, pch = 3, cex = 0.5)
	}	
	
	## Extract response and predictors and combine in final training set
	if(verbose) print("Begin extract")
	dataSet <- data.frame(
			response = as.factor(over(x = xy, y = trainingData)[[classAttributes]]),
			extract(inputRaster, xy, cellnumbers = TRUE))
	
	## Discard duplicate cells
	dataSet <- dataSet[!duplicated(dataSet[,"cells"]),]
	dataSet <- dataSet[,colnames(dataSet) != "cells"]
	
	## Unique classes
	classes <- unique(trainingData[[classAttributes]])
	classMapping <- data.frame(classID = as.numeric(classes), class = levels(classes))
	
	## TRAIN ######################### 
	if(verbose) print("Starting to calculate random forest model") 
	model <- randomForest(response ~ . , data = dataSet, na.action = na.omit, confusion = TRUE, ...)		
	
	## PREDICT ######################### 
	progress <- "none"
	if(verbose) { print("Starting spatial predict")
		progress <- "text"
	}
	 
	## Don't know whether we need this, who would be crazy enough to do more than 255 classes...
	ifelse(length(classes) < 255, dataType <- "INT1U",  dataType <- "INT2U")
	
	if(is.null(filename)){
		spatPred <- predict(inputRaster, model, progress = progress, dataType = dataType, overwrite = overwrite)
	} else {
		spatPred <- predict(inputRaster, model, filename = filename, progress = progress, dataType = dataType, overwrite = overwrite)
	}
	 
	## Print summary stats
	if(verbose)
		print(paste0(paste0(rep("*",20), collapse = "")," Classification summary " ,paste0(rep("*",20), collapse = "")))
		## Samples total
		## Samples per class
		print(model)
	## TODO: calculate users,producer's accuracies and kappas
	
	## DISCUSS: should we return sample points as well?
	return(list(model = model, map = spatPred, classMapping = classMapping)) 
	
}

#' Calculates the Normalized Difference Vegetation Index (NDVI)
#'
#' Calculates the NDVI, defined as: (nir - red) / (nir + red).
#'
#' @export NDVI
#' @param red red
#' @param nir near-infrared
#' @param ... additional arguments as for \code{\link{writeRaster}}
#' @examples
#' NDVI_img <- NDVI(red=raster(L5TSR_1986, layer=3), nir=raster(L5TSR_1986, 
#'                 layer=4))
#' plot(NDVI_img)
setGeneric("NDVI", function(red, nir, ...) {
    standardGeneric("NDVI")
})

NDVI_calc <- function(red, nir) {
    v <- (nir - red) / (nir + red)
    return(v)
}

#' @rdname NDVI
#' @aliases NDVI,numeric,numeric,numeric-method
setMethod("NDVI", signature(red="numeric", nir="numeric"),
    function(red, nir) {
        NDVI_calc(red, nir)
    }
)

#' @rdname NDVI
#' @aliases NDVI,matrix,matrix,matrix-method
setMethod("NDVI", signature(red="matrix", nir="matrix"),
    function(red, nir) {
        ret <- NDVI_calc(red, nir)
        return(ret)
    }
)

#' @rdname NDVI
#' @aliases NDVI,RasterLayer,RasterLayer,RasterLayer-method
#' @importFrom raster overlay
setMethod("NDVI", signature(red="RasterLayer", nir="RasterLayer"),
    function(red, nir, ...) {
        ret  <- overlay(red, nir, fun=function(red, nir) {
                NDVI_calc(red, nir)
            } , ...)
        return(ret)
    }
)

#' Calculates the Enhanced Vegetation Index (EVI)
#'
#' @export EVI
#' @importFrom raster overlay
#' @param blue blue
#' @param red red
#' @param nir near-infrared
#' @param ... additional arguments as for \code{\link{writeRaster}}
#' @references Huete, A. R., HuiQing Liu, and W. J. D. van Leeuwen. 1997. The
#' use of vegetation indices in forested regions: issues of linearity and
#' saturation. Pages 1966-1968 vol.4 Geoscience and Remote Sensing, 1997.
#' IGARSS '97. Remote Sensing - A Scientific Vision for Sustainable
#' Development., 1997 IEEE International.
#' @examples
#' EVI_img <- EVI(blue=raster(L5TSR_1986, layer=1), red=raster(L5TSR_1986, layer=3), 
#'                nir=raster(L5TSR_1986, layer=4))
#' plot(EVI_img)
setGeneric("EVI", function(blue, red, nir, ...) {
    standardGeneric("EVI")
})

EVI_calc <- function(blue, red, nir) {
    v <- (2.5*(nir - red)) / (1 + nir + 6*red - 7.5*blue)
    return(v)
}

#' @rdname EVI
#' @aliases EVI,numeric,numeric,numeric-method
setMethod("EVI", signature(blue="numeric", red="numeric", nir="numeric"),
    function(blue, red, nir) {
        EVI_calc(blue, red, nir)
    }
)

#' @rdname EVI
#' @aliases EVI,numeric,numeric,numeric-method
setMethod("EVI", signature(blue="matrix", red="matrix", nir="matrix"),
    function(blue, red, nir) {
        ret <- EVI_calc(blue, red, nir)
        return(ret)
    }
)

#' @rdname EVI
#' @aliases EVI,RasterLayer,RasterLayer,RasterLayer-method
#' @importFrom raster overlay
setMethod("EVI", signature(blue="RasterLayer", red="RasterLayer", 
                           nir="RasterLayer"),
    function(blue, red, nir, ...) {
        ret <- overlay(blue, red, nir, fun=function(blue, red, nir) {
                EVI_calc(blue, red, nir)
            }, ...)
        return(ret)
    }
)

#' Calculates the Modified Soil-Adjusted Vegetation Index (MSAVI)
#'
#' Note that this avoids the need for calculating L by using the equation for 
#' MSAVI2 from Qi et al. (1994).
#'
#' @export MSAVI2
#' @importFrom raster overlay
#' @param red red
#' @param nir near-infrared
#' @param ... additional arguments as for \code{\link{writeRaster}}
#' @references Qi, J., A. Chehbouni, A. R. Huete, Y. H. Kerr, and S.
#' Sorooshian. 1994. A modified soil adjusted vegetation index. Remote Sensing
#' of Environment 48:119-126.
#' @examples
#' MSAVI_img <- MSAVI2(red=raster(L5TSR_1986, layer=3),
#'                     nir=raster(L5TSR_1986, layer=4))
#' plot(MSAVI_img)
setGeneric("MSAVI2", function(red, nir, ...) {
    standardGeneric("MSAVI2")
})

MSAVI2_calc <- function(red, nir) {
    v <- (2*nir + 1 - sqrt((2*nir + 1)^2 - 8*(nir - red)))/2
    return(v)
}

#' @rdname MSAVI2
#' @aliases MSAVI2,numeric,numeric,numeric-method
setMethod("MSAVI2", signature(red="numeric", nir="numeric"),
    function(red, nir) {
        MSAVI2_calc(red, nir)
    }
)

#' @rdname MSAVI2
#' @aliases MSAVI2,matrix,matrix,matrix-method
setMethod("MSAVI2", signature(red="matrix", nir="matrix"),
    function(red, nir) {
        ret <- MSAVI2_calc(red, nir)
        return(ret)
    }
)

#' @rdname MSAVI2
#' @aliases MSAVI2,RasterLayer,RasterLayer,RasterLayer-method
#' @importFrom raster overlay
setMethod("MSAVI2", signature(red="RasterLayer", nir="RasterLayer"),
    function(red, nir, ...) {
        ret  <- overlay(red, nir, fun=function(red, nir) {
                MSAVI2_calc(red, nir)
            } , ...)
        return(ret)
    }
)

#' Calculates the Atmospherically Resistant Vegetation Index (ARVI)
#'
#' @export ARVI
#' @importFrom raster overlay
#' @param blue blue
#' @param red red
#' @param nir near-infrared
#' @param ... additional arguments as for \code{\link{writeRaster}}
#' @references Kaufman, Y. J., and D. Tanre. 1996. Strategy for direct and
#' indirect methods for correcting the aerosol effect on remote sensing: from
#' AVHRR to EOS-MODIS. Remote Sensing of Environment:65-79.
#' @examples
#' ARVI_img <- ARVI(blue=raster(L5TSR_1986, layer=1), red=raster(L5TSR_1986, 
#'                  layer=3), nir=raster(L5TSR_1986, layer=4))
#' plot(ARVI_img)
setGeneric("ARVI", function(blue, red, nir, ...) {
    standardGeneric("ARVI")
})


ARVI_calc <- function(blue, red, nir) {
    v <- (nir - 2*red - blue) / (nir + 2*red - blue)
    return(v)
}

#' @rdname ARVI
#' @aliases ARVI,numeric,numeric,numeric-method
setMethod("ARVI", signature(blue="numeric", red="numeric", nir="numeric"),
    function(blue, red, nir) {
        ARVI_calc(blue, red, nir)
    }
)

#' @rdname ARVI
#' @aliases ARVI,matrix,matrix,matrix-method
setMethod("ARVI", signature(blue="matrix", red="matrix", nir="matrix"),
    function(blue, red, nir) {
        ret <- ARVI_calc(blue, red, nir)
        return(ret)
    }
)

#' @rdname ARVI
#' @aliases ARVI,RasterLayer,RasterLayer,RasterLayer-method
#' @importFrom raster overlay
setMethod("ARVI", signature(blue="RasterLayer", red="RasterLayer", 
                            nir="RasterLayer"),
    function(blue, red, nir, ...) {
        ret <- overlay(blue, red, nir, fun=function(blue, red, nir) {
                ARVI_calc(blue, red, nir)
            }, ...)
        return(ret)
    }
)
tasscap <-
function(basename, sat=7)

{
    # basename is the name of the band data files, which will have the band number appended
    # should be in at-sensor reflectance (rc1)
    # sat: 5 = Landsat 5 (TM) or 7 = Landsat 7 (ETM+)
        
        # original papers
        # Kauth and Thomas
        # Crist and Cicone

    band1 <- get(paste(basename, "1", sep=""))
    band2 <- get(paste(basename, "2", sep=""))
    band3 <- get(paste(basename, "3", sep=""))
    band4 <- get(paste(basename, "4", sep=""))
    band5 <- get(paste(basename, "5", sep=""))
    band7 <- get(paste(basename, "7", sep=""))

    if(class(band1) == "SpatialGridDataFrame") {
    	output.sgdf <- band1
	    use.sgdf <- TRUE
    	band1 <- band1@data[,1]
    	band2 <- band2@data[,1]
    	band3 <- band3@data[,1]
    	band4 <- band4@data[,1]
    	band5 <- band5@data[,1]
    	band7 <- band7@data[,1]
    }

    all.bands <- cbind(band1, band2, band3, band4, band5, band7)

        
    if(sat == 7) {
        tc.coef <- matrix(c(
        # Tasseled cap coefficients for Landsat 7 ETM+ at-satellite reflectance from HWY+2002
    # Band 1     Band 2       Band 3     Band 4     Band 5        Band 7     Index
     0.3561,     0.3972,      0.3904,    0.6966,    0.2286,       0.1596,    #  Brightness       
    -0.3344,    -0.3544,     -0.4556,    0.6966,   -0.0242,      -0.2630,    #  Greenness       
     0.2626,     0.2141,      0.0926,    0.0656,   -0.7629,      -0.5388,    #  Wetness          
     0.0805,    -0.0498,      0.1950,   -0.1327,    0.5752,      -0.7775,    #  Fourth           
    -0.7252,    -0.0202,      0.6683,    0.0631,   -0.1494,      -0.0274,    #  Fifth           
     0.4000,    -0.8172,      0.3832,    0.0602,   -0.1095,       0.0985     #  Sixth            
    ), ncol=6, byrow=TRUE)
    } else if(sat == 5) {
        tc.coef <- matrix(c(
        # TM Tasseled Cap Equivalent Transformation Matrix for Band Reflectance Factor from Crist1985
    # Band 1     Band 2       Band 3     Band 4     Band 5        Band 7     Index
     0.2043,     0.4158,      0.5524,    0.5741,    0.3124,       0.2303,    #  Brightness
    -0.1603,    -0.2819,     -0.4934,    0.7940,    0.0002,      -0.1446,    #  Greenness
     0.0315,     0.2021,      0.3102,    0.1594,    0.6806,      -0.6109,    #  Wetness
    -0.2117,    -0.0284,      0.1302,   -0.1007,    0.6529,      -0.7078,    #  Fourth
    -0.8669,    -0.1835,      0.3856,    0.0408,    0.1132,       0.2272,    #  Fifth
     0.3677,    -0.8200,      0.4354,    0.0518,    0.0066,      -0.0104     #  Sixth
    ), ncol=6, byrow=TRUE)
    } else {
        stop("sat not recognized.\n")
    }

    colnames(tc.coef) <- c("band1", "band2", "band3", "band4", "band5", "band7")
    rownames(tc.coef) <- c("Brightness", "Greenness", "Wetness", "Fourth", "Fifth", "Sixth")
    tc.coef <- t(tc.coef)

    output <- all.bands %*% tc.coef
    output <- as.data.frame(output[,1:3])

    if(use.sgdf) {
    	Brightness <- output.sgdf
	Brightness@data[,1] <- output[, "Brightness"]
    	Greenness <- output.sgdf
	Greenness@data[,1] <- output[, "Greenness"]
    	Wetness <- output.sgdf
	Wetness@data[,1] <- output[, "Wetness"]
	output <- list(Brightness=Brightness, Greenness=Greenness, Wetness=Wetness)
    }

    output
}

#' TEAM land use and cover change data processing toolkit
#'
#' teamlucc is a set of routines to support analyzing land use and cover change 
#' (LUCC) in R. The package was designed to support analyzing LUCC in the Zone 
#' of Interaction (ZOIs) of monitoring sites in the Tropical Ecology Assessment 
#' and Monitoring (TEAM) Network.
#'
#' @name teamlucc-package
#' @aliases teamlucc
#' @docType package
#' @title TEAM Remote Sensing Processing Tools
#' @author Alex Zvoleff, \email{azvoleff@@conservation.org}
#' @keywords package
#' @useDynLib teamlucc
NULL
#' Landsat 5 Surface Reflectance Image from February 6, 1986 (path 15, row 53)
#' 
#' Portion of Landsat 5 Surface Reflectance image from the Landsat Climate Data 
#' Record archive. This subset of the image includes only bands 1-4, and pixel 
#' values have been scaled by 10000 and rounded off.
#'
#' @docType data
#' @name L5TSR_1986
#' @seealso L5TSR_2001
NULL
#' Landsat 5 Surface Reflectance Image from January 14, 2001 (path 15, row 53)
#' 
#' Portion of Landsat 5 Surface Reflectance image from the Landsat Climate Data 
#' Record archive. This subset of the image includes only bands 1-4, and pixel 
#' values have been scaled by 10000 and rounded off.
#'
#' @docType data
#' @name L5TSR_2001
#' @seealso L5TSR_1986
NULL
#' Subset of ASTER Digital Elevation Model V002
#' 
#' @docType data
#' @name ASTER_V002_WEST
#' @seealso ASTER_V002_EAST
NULL
#' Training polygons for 1986 and 2001 Landsat 5 Surface Reflectance images
#' 
#' Polygons digitized from 1986 and 2001 Landsat 5 Surface Reflectance image 
#' from the Landsat Climate Data Record archive. The training polygons can be 
#' used for testing classification algorithms.
#'
#' There are three columns in the dataset. "class_1986" is the cover class for 
#' the pixels in the polygon from the 1986 image. "class_2001" is the cover class 
#' for the pixels in the polygon from the 2001 image.
#'
#' @docType data
#' @name L5TSR_1986_2001_training
NULL
#' Subset of ASTER Digital Elevation Model V002
#' 
#' @docType data
#' @name ASTER_V002_WEST
#' @seealso ASTER_V002_EAST
NULL
#' Subset of ASTER Digital Elevation Model V002
#' 
#' @docType data
#' @name ASTER_V002_EAST
#' @seealso ASTER_V002_WEST
NULL
thermalband <-
function(x, band)
{
## convert thermal band from DN to temperature

	# coefs: gain, bias, K1, K2 from Chander et al. 2009
	if(band == 6) band.coefs <- c(0.055376, 1.18, 607.76, 1260.56)
	if(band == 61) band.coefs <- c(0.067087, -0.07, 666.09, 1282.71)
	if(band == 62) band.coefs <- c(0.037205, 3.16, 666.09, 1282.7)

	results <- x
	x <- as.vector(as.matrix(x))

	# at-sensor radiance
	x <- x * band.coefs[1] + band.coefs[2]

	x <- band.coefs[4] / log(band.coefs[3]/x + 1)

    # return the same structure as the input values
    if(class(results) == "SpatialGridDataFrame")
        results@data[,1] <- x
    else if(is.data.frame(results))
        results <- data.frame(matrix(x, nrow=nrow(results), ncol=ncol(results)))
    else if(is.matrix(results))
        results <- matrix(x, nrow=nrow(results), ncol=ncol(results))
    else # return a vector 
        results <- x
    
    results
}

#' Automatically determine value for image thresholding
#'
#' The only method currently implemented is Huang's fuzzy thresholding method.  
#' The code for Huang's method was ported to C++ for \code{teamlucc} from the 
#' code in the Auto_threshold imageJ plugin by Gabriel Landini. See original 
#' code at:
#' http://www.mecourse.com/landinig/software/autothreshold/autothreshold.html
#'
#' This function will run in parallel if a parallel backend is registered with 
#' \code{\link{foreach}}.
#'
#' @export
#' @import foreach
#' @importFrom iterators iter
#' @param x the input image, as a matrix or raster
#' @param method the thresholding method. Currently only "huang" is 
#' implemented.
#' @param n_bin number of bins to use when calculating histogram
#' @param maxpixels maximum number of pixels size to use when calculating 
#' histogram
#' @return integer threshold value
#' @references Huang, L.-K., and M.-J. J. Wang. 1995. Image thresholding by 
#' minimizing the measures of fuzziness. Pattern recognition 28 (1):41--51.
threshold <- function(x, method="huang", n_bin=1000, maxpixels=5e5) {
    stopifnot(method %in% c("huang"))

    if (ncell(x) > maxpixels) {
        x <- sampleRegular(x, maxpixels, useGDAL=TRUE, asRaster=TRUE)
    }

    x <- stack(x)
    x <- setMinMax(x)
    mins <- minValue(x)
    maxs <- maxValue(x)

    bys <- (maxs - mins) / (n_bin)

    bandnum=minval=maxval=NULL
    thresholds <- foreach(bandnum=iter(1:nlayers(x)), minval=iter(mins), 
                          maxval=iter(maxs), by=iter(bys),
                          .packages=c('teamlucc'),
                          .combine=c) %dopar% {
        image_hist <- hist(x[[bandnum]], breaks=seq(minval, maxval+by, by=by), 
                           plot=FALSE, maxpixels=maxpixels)
        threshold_index <- threshold_Huang(image_hist$counts)
        image_hist$breaks[threshold_index]
    }

    return(thresholds)
}
#' creates a raster with the brightness temperature extracted from Landsat tirs1 or tirs 2 band
#' @description Creates a raster with the at satellite brightness temperature extracted from Landsat tirs1 or tirs2 band.
#'
#' @param landsat8 list returned by rLandsat8::ReadLandsat8
#' @param band Landsat 2 bandname (one of "tirs1", "tirs2")
#' @return brightness temperature raster
#' @examples \dontrun{
#' ls8 <- ReadLandsat8("LC80522102014165LGN00")
#' ToAtSatelliteBrightnessTemperature(ls8, "tirs1")
#' }
#'
#' @export
#' @import raster

ToAtSatelliteBrightnessTemperature <- function(landsat8, band) {
  
  bandnames <-c("aerosol", "blue", "green", "red",
  "nir", "swir1", "swir2",
  "panchromatic",
  "cirrus",
  "tirs1", "tirs2")
  
  allowedbands <- c("tirs1", "tirs2")
  
  if (!band %in% allowedbands)
  {
       stop(paste(band, "band not allowed"))
  }
  
  toarad <- ToTOARadiance(landsat8, band)
  
  idx <- seq_along(bandnames)[sapply(bandnames, function(x) band %in% x)]

  k1 <- as.numeric(landsat8$metadata[[paste0("k1_constant_band_",idx)]])
  k2 <- as.numeric(landsat8$metadata[[paste0("k2_constant_band_",idx)]])

  bt <- k2 / log(k1 / toarad + 1) 

  return(bt)

}
#' creates a raster with the LSWI vegetation index
#' @description Creates a raster with the with the LSWI vegetation index: LSWI=(??NIR ?????SWIR1)/(??NIR +??SWIR1)
#'
#' @param product name of the product, e.g. LC80522102014165LGN00. It must be in the working directory.
#' @param sun angle correction, default is.suncorrected = FALSE
#' @return brightness temperature raster
#' @examples \dontrun{
#' ls8 <- ReadLandsat8("LC81880342014174LGN00")
#' r <- ToLSWI(ls8)
#' }
#'
#' @export
#' @import raster

ToLSWI <- function(landsat8, is.suncorrected = FALSE) {

  # LSWI=(??NIR ?????SWIR1)/(??NIR +??SWIR1) 
  nir <- ToTOAReflectance(landsat8, "nir", is.suncorrected)
  swir1 <- ToTOAReflectance(landsat8, "swir1", is.suncorrected)
  
  lswi <- (nir - swir1) / (nir + swir1)  
  
  return(lswi)

}
#' creates a raster with the MNDWI vegetation index
#' @description Creates a raster with the with the MNDWI vegetation index: MNDWI=(??Green ??? ??SWIR1)/(??Green + ??SWIR1) 
#'
#' @param product name of the product, e.g. LC80522102014165LGN00. It must be in the working directory.
#' @param sun angle correction, default is.suncorrected = FALSE
#' @return brightness temperature raster
#' @examples \dontrun{
#' ls8 <- ReadLandsat8("LC81880342014174LGN00")
#' r <- ToMNDWI(ls8)
#' }
#'
#' @export
#' @import raster

ToMNDWI <- function(landsat8, is.suncorrected = FALSE) {

  # MNDWI=(??Green ??? ??SWIR1)/(??Green + ??SWIR1) 
  green <- ToTOAReflectance(landsat8, "green", is.suncorrected)
  swir1 <- ToTOAReflectance(landsat8, "swir1", is.suncorrected)
  
  mndwi <- (green - swir1) / (green + swir1)
  
  return(mndwi)

}
#' creates a raster with the classified Normalized Burn Ratio (NBR) index
#' @description Creates a raster with the classified Normalized Burn Ratio (NBR) index 
#'
#' @param prefire name of the prefire product, e.g. LC80522102014165LGN00. It must be in the working directory.
#' @param postfire name of the postfire product, e.g. LC80522102014165LGN00. It must be in the working directory.
#' @param sun angle correction, default is.suncorrected = FALSE
#' @return classified Normalized Burn Ratio (NBR) raster
#' @examples \dontrun{
#' prefire <- "LC82040322013219LGN00"
#' postfire <- "LC82040322013267LGN00"
#'
#' lspre <- ReadLandsat8(prefire)
#' lspost <- ReadLandsat8(postfire)
#' r <- ToNBRClass(lspre, lspost)
#' }
#'
#' @export
#' @import raster

ToNBRClass <- function(prefire, postfire, is.suncorrected = FALSE) {

  # classify
  m <- c(-Inf, -500, -1, -500, -251, 1, -251, -101, 2, -101, 99, 3, 99, 269, 
    4, 269, 439, 5, 439, 659, 6, 659, 1300, 7, 1300, +Inf, -1)
  class.mat <- matrix(m, ncol=3, byrow=TRUE)

  reclass <- reclassify(10^3 * dNBR(prefire, postfire, is.suncorrected), class.mat)
  
  reclass <- ratify(reclass)
  rat <- levels(reclass)[[1]]
  rat$legend  <- c("NA", "Enhanced Regrowth, High", "Enhanced Regrowth, Low", "Unburned", "Low Severity", "Moderate-low Severity", "Moderate-high Severity", "High Severity")
  levels(reclass) <- rat
  
  return(reclass)

}
#' creates a raster with the Normalized Burn Ratio (NBR) index
#' @description Creates a raster with with the Normalized Burn Ratio (NBR) index: NBR =(ÏNIR âˆ’ÏSWIR2)/(ÏNIR +ÏSWIR2)
#'
#' @param product name of the product, e.g. LC80522102014165LGN00. It must be in the working directory.
#' @param sun angle correction, default is.suncorrected = FALSE
#' @return Normalized Burn Ratio (NBR) index raster
#' @examples \dontrun{
#' ls8 <- ReadLandsat8("LC81880342014174LGN00")
#' r <- ToNBR(ls8)
#' }
#'
#' @export
#' @import raster

ToNBR <- function(landsat8, is.suncorrected = FALSE) {

  # NBR =(ÏNIR âˆ’ÏSWIR2)/(ÏNIR +ÏSWIR2)
  nir <- ToTOAReflectance(landsat8, "nir", is.suncorrected)
  swir2 <- ToTOAReflectance(landsat8, "swir2", is.suncorrected)
  
  nbr <- (nir - swir2) / (nir + swir2)
  
  return(nbr)

}
#' creates a raster with the NDVI vegetation index
#' @description Creates a raster with the with the NDVI vegetation index: NDVI=(??NIR ?????Red)/(??NIR +??Red)
#'
#' @param product name of the product, e.g. LC80522102014165LGN00. It must be in the working directory.
#' @param sun angle correction, default is.suncorrected = FALSE
#' @return brightness temperature raster
#' @examples \dontrun{
#' ls8 <- ReadLandsat8("LC81880342014174LGN00")
#' r <- ToNDVI(ls8)
#' }
#'
#' @export
#' @import raster

ToNDVI <- function(landsat8, is.suncorrected = FALSE) {

  # NDVI=(??NIR ?????Red)/(??NIR +??Red)
  nir <- ToTOAReflectance(landsat8, "nir", is.suncorrected)
  red <- ToTOAReflectance(landsat8, "red", is.suncorrected)
  
  ndvi <- (nir - red) / (nir + red)
  
  return(ndvi)

}
topocorr <-
function(x, slope, aspect, sunelev, sunazimuth, method="cosine", na.value=NA, GRASS.aspect=FALSE, IL.epsilon=0.000001)
{
# topographic correction for image x based on
# topography and sun location

# IL.epsilon: if IL == 0, the corrected value is Inf (division by zero)
# adding a tiny increment eliminates the Inf

## aspect may be GRASS output: counterclockwise from east
## or nonGRASS output: clockwise from north
## require the latter for further calculations
## because sunazimuth is always measured clockwise from north
    if(GRASS.aspect) {
        aspect <- as.matrix(aspect)
        aspect <- -1 * aspect + 90
        aspect <- (aspect + 360) %% 360
    }

# all inputs are in degrees, but we need radians
    slope <- (pi/180) * as.matrix(slope)
    aspect <- (pi/180) * as.matrix(aspect)
    sunzenith <- (pi/180) * (90 - sunelev)
    sunazimuth <- (pi/180) * sunazimuth

    x.orig <- x
    x <- as.matrix(x)
    x[x == na.value] <- NA

    IL <- cos(slope) * cos(sunzenith) + sin(slope) * sin(sunzenith) * cos(sunazimuth - aspect)
    IL[IL == 0] <- IL.epsilon

        METHODS <- c("cosine", "improvedcosine", "minnaert", "minslope", "ccorrection", "gamma", "SCS", "illumination")
        method <- pmatch(method, METHODS)
        if (is.na(method)) 
            stop("invalid method")
        if (method == -1) 
            stop("ambiguous method")

    if(method == 1){
        ## Cosine method
        xout <- x * (cos(sunzenith)/IL)
    }
    else if(method == 2) {
    ## Improved cosine method
        ILmean <- mean(as.vector(IL), na.rm=TRUE)
        xout <- x + (x * (ILmean - IL)/ILmean)
    }
    else if(method == 3) {
        ## Minnaert
        ## K is between 0 and 1
        ## only use points with greater than 5% slope
        targetslope <- atan(.05)

        if(all(x[slope >= targetslope] < 0, na.rm=TRUE)) {
            K <- 1
        }
        else {
            # IL can be <=0 under certain conditions
            # but that makes it impossible to take log10 so remove those elements
            K <- data.frame(y = as.vector(x[slope >= targetslope]), x = as.vector(IL[slope >= targetslope])/cos(sunzenith))
            K <- K[!apply(K, 1, function(x)any(is.na(x))),]
            K <- K[K$x > 0, ]
            K <- K[K$y > 0, ]

            K <- lm(log10(K$y) ~ log10(K$x))
            K <- coefficients(K)[[2]] # need slope
            if(K > 1) K <- 1
            if(K < 0) K <- 0
        }

        xout <- x * (cos(sunzenith)/IL) ^ K
    }
    else if(method == 4) {
        ## Minnaert with slope
        ## K is between 0 and 1
        ## only use points with greater than 5% slope
        targetslope <- atan(.05)

        if(all(x[slope >= targetslope] < 0, na.rm=TRUE)) {
            K <- 1
        }
        else {
            # IL can be <=0 under certain conditions
            # but that makes it impossible to take log10 so remove those elements
            K <- data.frame(y=as.vector(x[slope >= targetslope]), x=as.vector(IL[slope >= targetslope])/cos(sunzenith))
            K <- K[!apply(K, 1, function(x)any(is.na(x))),]
            K <- K[K$x > 0, ]
            K <- K[K$y > 0, ]

            K <- lm(log10(K$y) ~ log10(K$x))
            K <- coefficients(K)[[2]] # need slope
            if(K > 1) K <- 1
            if(K < 0) K <- 0
        }

        xout <- x * cos(slope) * (cos(sunzenith) / (IL * cos(slope))) ^ K
    }
    else if(method == 5) {
        ## C correction
        band.lm <- lm(as.vector(x) ~ as.vector(IL))
        C <- coefficients(band.lm)[[1]]/coefficients(band.lm)[[2]]

        xout <- x * (cos(sunzenith) + C) / (IL + C)
    }
    else if(method == 6) {
        ## Gamma
        ## assumes zenith viewing angle
        viewterrain <- pi/2 - slope
        xout <- x * (cos(sunzenith) + cos(pi / 2)) / (IL + cos(viewterrain))
    }
    else if(method == 7) {
        ## SCS method from GZ2009
        xout <- x * (cos(sunzenith) * cos(slope))/IL
    }
    else if(method == 8) {
        ## illumination only
        xout <- IL
    }

    ## if slope is zero, reflectance does not change
    if(method != 8) 
        xout[slope == 0 & !is.na(slope)] <- x[slope == 0 & !is.na(slope)]

    ## if x was a SpatialGridDataFrame, return an object of the same class
    if(class(x.orig) == "SpatialGridDataFrame") {
        x.orig@data[,1] <- as.vector(xout)
        xout <- x.orig
    }


    xout

}

.calc_IL_vector <- function(slope, aspect, sunzenith, sunazimuth, IL.epsilon) {
    IL <- cos(slope) * cos(sunzenith) + sin(slope) * sin(sunzenith) * 
        cos(sunazimuth - aspect)
    IL[IL == 0] <- IL.epsilon
    return(IL)
}

.calc_IL <- function(slope, aspect, sunzenith, sunazimuth, IL.epsilon) {
    overlay(slope, aspect,
            fun=function(slope_vals, aspect_vals) {
                .calc_IL_vector(slope_vals, aspect_vals, sunzenith, sunazimuth, 
                               IL.epsilon)
            })
}

#' Topographic correction for satellite imagery
#'
#' Perform topographic correction using a number of different methods. This 
#' code is modified from the code in the \code{landsat} package by Sarah 
#' Goslee.  This version of the code has been altered from the \code{landsat} 
#' version to allow the option of using a sample of pixels for calculation of k 
#' in the Minnaert correction (useful when dealing with large images).
#' 
#' See the help page for \code{topocorr} in the \code{landsat} package for 
#' details on the parameters.
#'
#' @export
#' @param x image as a \code{RasterLayer}
#' @param slope the slope in radians as a \code{RasterLayer}
#' @param aspect the aspect in radians as a \code{RasterLayer}
#' @param sunelev sun elevation in degrees
#' @param sunazimuth sun azimuth in degrees
#' @param method the method to use for the topographic correction:
#' cosine, improvedcosine, minnaert, minslope, ccorrection, gamma, SCS, or 
#' illumination
#' @param na.value the value used to code no data values
#' @param IL.epsilon a small amount to add to calculated illumination values 
#' that are equal to zero to avoid division by zero resulting in Inf values
#' @param sampleindices (optional) row-major indices of sample pixels to use in 
#' regression models used for some topographic correction methods (like 
#' Minnaert). Useful when handling very large images. See
#' \code{\link{gridsample}} for one method of calculating these indices.
#' @param DN_min minimum allowable pixel value after correction (values less 
#' than \code{DN_min} are set to NA)
#' @param DN_max maximum allowable pixel value after correction (values less 
#' than \code{DN_max} are set to NA)
#' @return RasterBrick with two layers: 'slope' and 'aspect'
#' @author Sarah Goslee and Alex Zvoleff
#' @references
#' Sarah Goslee. Analyzing Remote Sensing Data in {R}: The {landsat} Package.  
#' Journal of Statistical Software, 2011, 43:4, pg 1--25.  
#' http://www.jstatsoft.org/v43/i04/
#' @examples
#' #TODO: add examples
topocorr_samp <- function(x, slope, aspect, sunelev, sunazimuth, method="cosine", 
                          na.value=NA, IL.epsilon=0.000001,
                          sampleindices=NULL, DN_min=NULL, DN_max=NULL) {
    # some inputs are in degrees, but we need radians
    stopifnot((sunelev >= 0) & (sunelev <= 90))
    stopifnot((sunazimuth >= 0) & (sunazimuth <= 360))
    sunzenith <- (pi/180) * (90 - sunelev)
    sunazimuth <- (pi/180) * sunazimuth

    x[x == na.value] <- NA

    IL <- .calc_IL(slope, aspect, sunzenith, sunazimuth, IL.epsilon)
    rm(aspect, sunazimuth)

    if (!is.null(sampleindices) && !(method %in% c('minnaert', 'minslope', 
                                                   'ccorrection'))) {
        warning(paste0('sampleindices are not used when method is "', method,
                       '". Ignoring sampleindices.'))
    }

    METHODS <- c("cosine", "improvedcosine", "minnaert", "minslope", 
                 "ccorrection", "gamma", "SCS", "illumination")
    method <- pmatch(method, METHODS)
    if (is.na(method)) 
        stop("invalid method")
    if (method == -1) 
        stop("ambiguous method")


    if(method == 1){
        ## Cosine method
        xout <- x * (cos(sunzenith)/IL)
    } else if(method == 2) {
        ## Improved cosine method
        ILmean <- cellStats(IL, stat='mean', na.rm=TRUE)
        xout <- x + (x * (ILmean - IL)/ILmean)
    } else if(method == 3) {
        ## Minnaert
        ## K is between 0 and 1
        ## only use points with greater than 5% slope
        targetslope <- atan(.05)

        if(all(x[slope >= targetslope] < 0, na.rm=TRUE)) {
            K <- 1
        } else {
            if (!is.null(sampleindices)) {
                K <- data.frame(y=x[slope >= targetslope][sampleindices],
                                x=IL[slope >= targetslope][sampleindices]/cos(sunzenith))
            } else {
                K <- data.frame(y=x[slope >= targetslope],
                                x=IL[slope >= targetslope]/cos(sunzenith))
            }
            # IL can be <=0 under certain conditions
            # but that makes it impossible to take log10 so remove those 
            # elements
            K <- K[!apply(K, 1, function(x)any(is.na(x))),]
            K <- K[K$x > 0, ]
            K <- K[K$y > 0, ]

            K <- lm(log10(K$y) ~ log10(K$x))
            K <- coefficients(K)[[2]] # need slope
            if(K > 1) K <- 1
            if(K < 0) K <- 0
        }

        xout <- x * (cos(sunzenith)/IL) ^ K
    } else if(method == 4) {
        ## Minnaert with slope
        ## K is between 0 and 1
        ## only use points with greater than 5% slope
        targetslope <- atan(.05)

        if(all(x[slope >= targetslope] < 0, na.rm=TRUE)) {
            K <- 1
        } else {
            if (!is.null(sampleindices)) {
                K <- data.frame(y=x[slope >= targetslope][sampleindices],
                                x=IL[slope >= targetslope][sampleindices] / cos(sunzenith))
            } else {
                K <- data.frame(y=x[slope >= targetslope], 
                                x=IL[slope >= targetslope]/cos(sunzenith))
            }
            # IL can be <=0 under certain conditions
            # but that makes it impossible to take log10 so remove those elements
            K <- K[!apply(K, 1, function(x) any(is.na(x))),]
            K <- K[K$x > 0, ]
            K <- K[K$y > 0, ]

            K <- lm(log10(K$y) ~ log10(K$x))
            K <- coefficients(K)[[2]] # need slope
            if(K > 1) K <- 1
            if(K < 0) K <- 0
        }
        xout <- x * cos(slope) * (cos(sunzenith) / (IL * cos(slope))) ^ K
    } else if(method == 5) {
        ## C correction
        if (!is.null(sampleindices)) {
            band.lm <- lm(x[sampleindices] ~ IL[sampleindices])
        } else {
            band.lm <- lm(getValues(x) ~ getValues(IL))
        }
        C <- coefficients(band.lm)[[1]]/coefficients(band.lm)[[2]]

        xout <- x * (cos(sunzenith) + C) / (IL + C)
    } else if(method == 6) {
        ## Gamma
        ## assumes zenith viewing angle
        viewterrain <- pi/2 - slope
        xout <- x * (cos(sunzenith) + cos(pi / 2)) / (IL + cos(viewterrain))
    } else if(method == 7) {
        ## SCS method from GZ2009
        xout <- x * (cos(sunzenith) * cos(slope))/IL
    } else if(method == 8) {
        ## illumination only
        xout <- IL
    }

    ## if slope is zero, reflectance does not change
    if(method != 8) 
        xout[slope == 0 & !is.na(slope)] <- x[slope == 0 & !is.na(slope)]

    if ((!is.null(DN_min)) || (!is.null(DN_max))) {
        xout <- calc(xout, fun=function(vals) {
                        if (!is.null(DN_min)) vals[vals < DN_min] <- NA
                        if (!is.null(DN_max)) vals[vals > DN_max] <- NA
                        return(vals)
                     })
    }
    return(xout)
}

#' Topographically correct a raster
#'
#' Performs topographic correction using code based on \code{topocorr} from the 
#' \code{landsat} package by Sarah Goslee. The code in this package has been 
#' modifed from \code{topocorr} to allow using a subsample of the image for 
#' Minnaert k calculations, and to provide the option of running the 
#' topographic correction in parallel using \code{foreach}.
#'
#' This function will run in parallel if a parallel backend is registered with 
#' \code{\link{foreach}}.
#'
#' @export
#' @import foreach
#' @param x an image to correct
#' @param slopeaspect a \code{RasterBrick} or \code{RasterStack} with two 
#' layers.  The first layer should be the slope, the second layer should be 
#' the aspect. The slope and aspect are defined as in \code{terrain} in the 
#' \code{raster} package, and both should be in radians.
#' @param sunelev sun elevation in degrees
#' @param sunazimuth sun azimuth in degrees
#' @param method the topographic correction method to use. See the help for 
#' \code{topocorr} for more guidance on this.
#' @param sampleindices (optional) row-major indices of sample pixels to use in 
#' regression models used for some topographic correction methods (like 
#' Minnaert). Useful when handling very large images. See
#' \code{\link{gridsample}} for one method of calculating these indices.
#' @param scale_factor factor by which to multiply results. Useful if rounding 
#' results to integers (see \code{asinteger} argument).
#' @param asinteger whether to round results to nearest integer. Can be used to 
#' save space by saving results as, for example, an 'INT2S' \code{raster}.
#' @param DN_min minimum allowable pixel value after correction (values less 
#' than \code{DN_min} are set to NA)
#' @param DN_max maximum allowable pixel value after correction (values less 
#' than \code{DN_max} are set to NA)
#' @param ... additional arguments to pass to \code{minnaert_samp} or 
#' \code{topocorr_samp}, depending on chosen topographic correction method
#' @return The topographically corrected image as a \code{RasterLayer} or 
#' \code{RasterStack}
#' @references
#' Sarah Goslee. Analyzing Remote Sensing Data in {R}: The {landsat} Package.  
#' Journal of Statistical Software, 2011, 43:4, pg 1--25.  
#' http://www.jstatsoft.org/v43/i04/
#' @examples
#' \dontrun{
#' # Mosaic the two ASTER DEM tiles needed to a Landsat image
#' DEM_mosaic <- mosaic(ASTER_V002_EAST, ASTER_V002_WEST, fun='mean')
#' 
#' # Crop and extend the DEM mosaic to match the Landsat image
#' matched_DEM <- match_rasters(L5TSR_1986, DEM_mosaic)
#' slopeaspect <- terrain(matched_DEM, opt=c('slope', 'aspect')
#' 
#' # Apply the topographic correction
#' sunelev <- 90 - 44.97 # From metadata file
#' sunazimuth <- 124.37 # From metadata file
#' L5TSR_1986_topocorr <- topographic_corr(L5TSR_1986, slopeaspect, sunelev, 
#'                                         sunazimuth, method='minslope')
#' 
#' plotRGB(L5TSR_1986, stretch='lin', r=3, g=2, b=1)
#' plotRGB(L5TSR_1986_topocorr, stretch='lin', r=3, g=2, b=1)
#' }
topographic_corr <- function(x, slopeaspect, sunelev, sunazimuth, 
                             method='minnaert_full', sampleindices=NULL, 
                             scale_factor=1, asinteger=FALSE, DN_min=NULL, 
                             DN_max=NULL, ...) {
    if (!(class(x) %in% c('RasterLayer', 'RasterStack', 'RasterBrick'))) {
        stop('x must be a Raster* object')
    }
    if (!(class(slopeaspect) %in% c('RasterBrick', 'RasterStack'))) {
        stop('slopeaspect must be a RasterBrick or RasterStack object')
    }
    slope <- raster(slopeaspect, layer=1)
    aspect <- raster(slopeaspect, layer=2)
    stopifnot((sunelev >= 0) & (sunelev <= 90))
    stopifnot((sunazimuth >= 0) & (sunazimuth <= 360))

    # Set uncorr_layer to NULL to pass R CMD CHECK without notes
    uncorr_layer=NULL
    was_rasterlayer <- class(x) == "RasterLayer"
    # Convert x to stack so foreach will run
    x <- stack(x)
    corr_img <- foreach(uncorr_layer=unstack(x), .combine='addLayer', 
                        .multicombine=TRUE, .init=raster(), 
                        .packages=c('teamlucc', 'rgdal')) %dopar% {
        if (method == 'minnaert_full') {
            minnaert_data <- minnaert_samp(uncorr_layer, slope, aspect, 
                                           sunelev=sunelev, 
                                           sunazimuth=sunazimuth, 
                                           sampleindices=sampleindices, 
                                           DN_min=DN_min, DN_max=DN_max, ...)
            corr_layer <- minnaert_data$minnaert
        } else {
            corr_layer <- topocorr_samp(uncorr_layer, slope, aspect, 
                                        sunelev=sunelev, sunazimuth=sunazimuth, 
                                        method=method, 
                                        sampleindices=sampleindices,
                                        DN_min=DN_min, DN_max=DN_max, ...)
        }
    }
    if (was_rasterlayer) corr_img <- corr_img[[1]]
    names(corr_img) <- paste0(names(x), 'tc')
    if (scale_factor != 1) {
        corr_img <- corr_img * scale_factor
    }
    if (asinteger) {
        corr_img <- round(corr_img)
    }
    return(corr_img)
}
#' creates a raster with the TOA radiance 
#' @description Creates a raster with the TOA radiance
#'
#' @param landsat8 list returned by rLandsat8::ReadLandsat8
#' @param band Landsat 11 bandname (one of "aerosol", "blue", "green", "red", "nir", "swir1", "swir2", "panchromatic", "cirrus", "tirs1", "tirs2" 
#' @return TOA Radiance raster
#' @examples \dontrun{
#' ls8 <- ReadLandsat8("LC81880342014174LGN00")
#' r <- ToTOARadiance(ls8, "red")
#' }
#'
#' @export
#' @import raster

ToTOARadiance <- function(landsat8, band) {

  bandnames <-c("aerosol", "blue", "green", "red",
  "nir", "swir1", "swir2",
  "panchromatic",
  "cirrus",
  "tirs1", "tirs2")
  
  allowedbands <- bandnames
  
  if (!band %in% allowedbands)
  {
       stop(paste(band, "band not allowed"))
  }
  
  idx <- seq_along(bandnames)[sapply(bandnames, function(x) band %in% x)]

  ml <- as.numeric(landsat8$metadata[[paste0("radiance_mult_band_",idx)]])
  al <- as.numeric(landsat8$metadata[[paste0("radiance_add_band_",idx)]])
  
  TOArad <- landsat8$band[[band]] * ml + al
  
  return(TOArad)
  
}
#' creates a raster with the TOA reflectance
#' @description Creates a raster with the TOA reflectance
#'
#' @param landsat8 list returned by rLandsat8::ReadLandsat8
#' @param band Landsat 9 bandname (one of "aerosol", "blue", "green", "red", "nir", "swir1", "swir2", "panchromatic", "cirrus")
#' @param sun angle correction, default is.suncorrected = FALSE
#' @return TOA Reflectance raster
#' @examples \dontrun{
#' ls8 <- ReadLandsat8("LC81880342014174LGN00")
#' r <- ToTOAReflectance(ls8, "blue")
#' }
#'
#' @export
#' @import raster

ToTOAReflectance <- function(landsat8, band, is.suncorrected = FALSE) {

  bandnames <-c("aerosol", "blue", "green", "red",
  "nir", "swir1", "swir2",
  "panchromatic",
  "cirrus",
  "tirs1", "tirs2")
  
  allowedbands <- c("aerosol", "blue", "green", "red",
  "nir", "swir1", "swir2",
  "panchromatic",
  "cirrus")
  
  if (!band %in% allowedbands)
  {
       stop(paste(band, "band not allowed"))
  }
  
  idx <- seq_along(bandnames)[sapply(bandnames, function(x) band %in% x)]

  ml <- as.numeric(landsat8$metadata[[paste0("reflectance_mult_band_",idx)]])
  al <- as.numeric(landsat8$metadata[[paste0("reflectance_add_band_",idx)]])
  
  # sun_elevation is in degree, need to convert into radians
  sun.correction.factor <- 1
  if(is.suncorrected)
       sun.correction.factor <- sin(as.numeric(landsat8$metadata$sun_elevation) * pi /180)

  TOAref <- (landsat8$band[[band]] * ml + al)/sun.correction.factor
  
  return(TOAref)

  
}
#' Function to convert TIMESAT .tpa binary format file to an R dataframe.
#'
#' @export
#' @param x A string giving the location of a .tpa file output by 
#' TIMESAT
#' @param max_num_seasons the maximum number of seasons for any of the pixels 
#' in the file
#' @return A data.frame containing 14 columns: row, col, season, start, end, 
#' length, base_value, peak_time, peak_value, amp, left_deriv, right_deriv, 
#' large_integ, and small_integ
#' @examples
#' # TODO: Need to add examples here, and need to include a sample TIMESAT tpa 
#' # file in the package data.
tpa2df <- function(x, max_num_seasons) {
    if (missing(x) || !grepl('[.]tpa$', tolower(x))) {
        stop('must specify a .tpa file')
    }
    if (missing(max_num_seasons) || max_num_seasons < 1) {
        stop('must specify maximum number of seasons represented in tpa file')
    }

    # The number of seasonal indicators output by TIMESAT.
    NUM_SEASONAL_INDICATORS <- 11

    # Number of elements in the tpa file line header (which are normally: row, 
    # column, number of seasons).
    LINE_HEADER_SIZE <- 3

    tpa_file_obj <- file(x, "rb")
    raw_vector <- readBin(tpa_file_obj, n=file.info(x)$size, raw())
    close(tpa_file_obj)

    # This function is used to track the offset within the binary vector as readBin 
    # does not track position except for file objects
    offset <- 1
    raw_vec_length <- length(raw_vector)
    offset_readBin <- function(raw_vec, what, n=n, size=size, increment_offset=TRUE, ...) {
        bin_data <- readBin(raw_vec[offset:(n * size + offset)], what, n, size, ...)
        # Use a global variable to track the offset
        if (increment_offset) {assign("offset", offset + (size*n), inherits=TRUE)}
        return(bin_data)
    }

    # File header format is: nyears nptperyear rowstart rowstop colstart colstop
    file_header <- offset_readBin(raw_vector, integer(), n=6, size=4)
    num_years <- file_header[1]
    rowstart <- file_header[3]
    rowstop <- file_header[4]
    colstart <- file_header[5]
    colstop <- file_header[6]

    num_rows <- rowstop - rowstart + 1
    num_cols <- colstop - colstart + 1

    num_pixels <- num_cols * num_rows

    tpa_data <- matrix(nrow=num_pixels*max_num_seasons,
                       ncol=(LINE_HEADER_SIZE + NUM_SEASONAL_INDICATORS))

    # Read the data and save it in the tpa_data matrix
    for (pixelnum in 1:num_pixels) {
        line_header <- offset_readBin(raw_vector, integer(), n=3, size=4)
        # Line header format is: rownum colnum numseasons
        num_seasons <- line_header[3]
        if (num_seasons > max_num_seasons) {
            stop(paste('pixel', pixelnum, 'has', num_seasons,
                       'seasons, but max_num_seasons was set to ', max_num_seasons, 
                       'seasons'))
        }
        if (num_seasons == 0) {
            # No seasons identified for this pixel - skip
            next
        }
        for (seasonnum in 1:max_num_seasons) {
            # Seasons were found for this pixel - read them
            tpa_data_row <- (pixelnum - 1)*num_seasons + seasonnum
            line_data <- offset_readBin(raw_vector, numeric(), 
                                        n=NUM_SEASONAL_INDICATORS, size=4)
            tpa_data[tpa_data_row, ] <- c(line_header[1], line_header[2], 
                                          seasonnum, line_data)
        }
    }

    tpa_data <- data.frame(tpa_data)
    tpa_data <- tpa_data[!(rowSums(is.na(tpa_data)) == ncol(tpa_data)), ]
    names(tpa_data) <- c("row", "col", "season", "start", "end", "length",
                         "base_value", "peak_time", "peak_value", "amp", "left_deriv",
                         "right_deriv", "large_integ", "small_integ")
    return(tpa_data)
}
#' Function to convert TIMESAT .tpa data.frame to an R raster. 
#'
#' @export
#' @param x A TPA data.frame as output by tpa2df
#' @param base_image A string giving the location of a raster file to use 
#' for georeferencing the output raster. Use one of the original raster files 
#' that was input to TIMESAT.
#' @param variable A string giving the variable name to write to a raster.  Can 
#' be one of: start, end, length, base_value, peak_time, peak_value, amp, 
#' left_deriv, right_deriv, large_integ, and small_integ.
#' @return A raster object
#' @examples
#' # TODO: Need to add examples here, and need to include a sample TIMESAT tpa 
#' # file in the package data.
tpadf2raster <- function(x, base_image, variable) {
    if (missing(x) || !is.data.frame(x)) {
        stop('must specify a tpa data.frame')
    } else if (missing(base_image) || !file.exists(base_image)) {
        stop('must specify a valid base image raster')
    }

    var_col <- grep(paste('^', variable, '$', sep=''), names(x))
    if (length(var_col) == 0) {
        stop(paste(variable, 'not found in tpa dataframe'))
    }
    base_image <- raster(base_image)
    ncol(base_image) * nrow(base_image) * 2

    seasons <- sort(unique(x$season))
    out_rasters <- c()
    for (season in sort(unique(x$season))) {
        season_data <- x[x$season == season, ]
        data_matrix <- matrix(NA, nrow(base_image), ncol(base_image))
        vector_indices <- (nrow(data_matrix) * season_data$col) - 
            (nrow(data_matrix) - season_data$row)
        data_matrix[vector_indices] <- season_data[, var_col]
        out_raster <- raster(data_matrix, template=base_image)
        out_rasters <- c(out_rasters, out_raster)
    }
    out_rasters <- stack(out_rasters)
    names(out_rasters) <- paste0('season_', seasons)

    return(out_rasters)
}
#' A class for tracking running time of individual sections of an R script
#' @slot timers a \code{data.frame} tracking timer names and start times
#' @slot notify function to use for outputting timers (defaults to 
#' \code{\link{print}}
#' @import methods
#' @importFrom lubridate now
#' @export Track_time
#' @name Track_time-class
setClass('Track_time', slots=c(timers='data.frame', notify="function"),
    prototype=list(timers=data.frame(label='Default', starttime=now()), 
                   notify=print)
)

#' Instantiate a new Track_time object
#'
#' Creates a new Track_time object for use in tracking and printing status the 
#' running time of processes in an R script.
#'
#' @export Track_time
#' @import methods
#' @importFrom lubridate now
#' @param notify a function to handle the string output from Track_time.  This 
#' function should accept a string as an argument. Default is the
#' \code{\link{print}} function.
#' @return Track_time object
#' @seealso \code{\link{start_timer}}, \code{\link{stop_timer}}
#' @examples
#' timer <- Track_time()
#' print(timer)
Track_time <- function(notify=print) {
    return(new('Track_time',
               timers=data.frame(label='Default', starttime=now()),
               notify=notify))
}

#' Print a Track_time object
#'
#' @export
#' @importFrom lubridate now as.duration
#' @import methods
#' @param x a Track_time object
#' @param label (optional) selects a specific tracking timer to print
#' @param ... ignored
#' @method print Track_time
print.Track_time <- function(x, label, ...) {
    timers <- x@timers
    if (!missing(label)) {
        if (!(label %in% timers$label)) {
            stop(paste0('"', label, '"', ' timer not defined'))
        } else {
            timers <- timers[timers$label == label, ] 
        }
    }
    for (n in 1:nrow(timers)) {
       x@notify(paste(timers$label[n], 'timer:',
                round(as.duration(now() - timers$starttime[n]), 3),
                'elapsed'))
    }
}

setMethod("show", signature(object="Track_time"), function(object) print(object))

#' @importFrom lubridate now as.duration
.start_timer <- function(x, label) {
    if (!missing(label)) {
        if (label %in% x@timers$label) {
            stop(paste0('"', label, '"', ' timer already defined'))
        }
        x@timers <- rbind(x@timers, data.frame(label=label, starttime=now()))
        x@notify(paste0(x@timers$starttime[x@timers$label == label], ': started "', label, '"'))
    } else {
        x@timers$starttime[x@timers$label == 'Default'] <- now()
        x@notify(paste0(x@timers$starttime[x@timers$label == "Default"], ': started'))
    }
    return(x)
}

#' Start a tracking timer
#'
#' The \code{label} is optional. If not supplied the default timer (labelled 
#' "Default") will be used.
#'
#' @export start_timer
#' @param x a \code{Track_time} object
#' @param label an optional label used to maintain multiple tracking timers
#' @return Track_time object
#' @seealso \code{\link{stop_timer}}
#' @examples
#' timer <- Track_time()
#' print(timer)
#'
#' timer <- start_timer(timer, 'test')
#'
#' print(timer, 'test')
#'
#' timer <- stop_timer(timer, 'test')
#'
#' print(timer)
setGeneric("start_timer", function(x, label) {
    standardGeneric("start_timer")
})

#' @rdname start_timer
#' @aliases start_timer,Track_time-method
setMethod("start_timer", signature(x="Track_time"),
    function(x) .start_timer(x)
)

#' @rdname start_timer
#' @aliases start_timer,Track_time,character-method
setMethod("start_timer", signature(x="Track_time", label="character"),
    function(x, label) .start_timer(x, label)
)

#' @importFrom lubridate now as.duration
.stop_timer <- function(x, label='Default') {
    if (!(label %in% x@timers$label)) {
        stop(paste0('"', label, '"', ' timer not defined'))
    }
    elapsed <- as.duration(now() - x@timers$starttime[x@timers$label == label])
    if (label == 'Default') {
        # Never delete the default timer. Only reset it.
        x@timers$starttime[x@timers$label == 'Default'] <- now()
    } else {
        x@timers <- x@timers[x@timers$label != label, ] 
    }
    x@notify(paste0(now(), ': finished "', label, '" (', round(elapsed, 3),' elapsed)'))
    return(x)
}

#' Stop a tracking timer
#'
#' The \code{label} is optional. If not supplied, the default timer, labelled 
#' 'Default' will be used.
#'
#' @export stop_timer
#' @param x a \code{Track_time} object
#' @param label an optional label used to maintain multiple tracking timers
#' @seealso \code{\link{start_timer}}
#' @return Track_time object
#' @examples
#' timer <- Track_time()
#' print(timer)
#'
#' timer <- start_timer(timer, 'test')
#'
#' print(timer, 'test')
#'
#' timer <- stop_timer(timer, 'test')
#'
#' print(timer)
setGeneric("stop_timer", function(x, label='Default') {
    standardGeneric("stop_timer")
})

#' @rdname stop_timer
#' @aliases stop_timer,Track_time-method
setMethod("stop_timer", signature(x="Track_time"),
    function(x) .stop_timer(x)
)

#' @rdname stop_timer
#' @aliases stop_timer,Track_time,character-method
setMethod("stop_timer", signature(x="Track_time", label="character"),
    function(x, label) .stop_timer(x, label)
)
#' Train a random forest or SVM classifier
#'
#' This function trains a Support Vector Machine (SVM) or Random Forest (RF) 
#' classifier for use in an image classification.
#'
#' For \code{type='svm'}, \code{tunegrid} must be a \code{data.frame} with two 
#' columns: ".sigma" and ".C". For \code{type='rf'}, must be a 
#' \code{data.frame} with one column: '.mtry'.
#'
#' This function will run in parallel if a parallel backend is registered with 
#' \code{\link{foreach}}.
#'
#' @export
#' @import caret randomForest e1071 kernlab
#' @param train_data a \code{link{pixel_data}} object
#' @param type either "svm" (to fit a support vector machine) or "rf" (to fit a
#' random forest).
#' @param use_training_flag indicates whether to exclude data flagged as 
#' testing data when training the classifier. For this to work the input 
#' train_data \code{data.frame} must have a column named 'training_flag' that 
#' indicates, for each pixel, whether that pixel is a training pixel (coded as 
#' TRUE) or testing pixel (coded as FALSE).
#' @param train_control default is NULL (reasonable values will be set 
#' automatically).  For details see \code{\link{trainControl}}.
#' @param tune_grid the training grid to be used for training the classifier.  
#' See Details.
#' @param use_rfe whether to use Recursive Feature Extraction (RFE) as 
#' implemented in the \code{caret} package to select a subset of the input 
#' features to be used in the classification. NOT YET SUPPORTED.
#' @param factors a list of character vector giving the names of predictors 
#' (layer names from the images used to build \code{train_data}) that should be 
#' treated as factors, and specifying the levels of each factor. For example, 
#' \code{factors=list(year=c(1990, 1995, 2000, 2005, 2010))}.
#' @param ... additional arguments (such as \code{ntree} for random forest 
#' classifier) to pass to \code{train}
#' @return a trained model (as a \code{train} object from the \code{caret} 
#' package)
#' @examples
#' train_data <- get_pixels(L5TSR_1986, L5TSR_1986_2001_training, "class_1986", 
#'                          training=.6)
#' model <- train_classifier(train_data)
train_classifier <- function(train_data, type='rf', use_training_flag=TRUE, 
                             train_control=NULL, tune_grid=NULL,
                             use_rfe=FALSE, factors=list(), ...) {
    stopifnot(type %in% c('svm', 'rf'))

    predictor_names <- names(train_data@x)

    # Convert predictors in training data to factors as necessary
    stopifnot(length(factors) == 0 || all(names(factors) %in% predictor_names))
    stopifnot(length(unique(names(factors))) == length(factors))
    for (factor_var in names(factors)) {
        pred_index <- which(predictor_names == factor_var)
        train_data@x[, pred_index] <- factor(train_data@x[, pred_index], 
                                             levels=factors[[factor_var]])
    }

    # Build the formula, excluding the training flag column (if it exists) from 
    # the model formula
    model_formula <- formula(paste('y ~', paste(predictor_names, collapse=' + ')))

    if (use_rfe) {
        stop('recursive feature extraction not yet supported')
        # This recursive feature elimination procedure follows Algorithm 19.5 
        # in Kuhn and Johnson 2013
        svmFuncs <- caretFuncs
        # First center and scale
        normalization <- preProcess(train_data@x, method='range')
        scaled_predictors <- predict(normalization, train_data@x)
        scaled_predictors <- as.data.frame(scaled_predictors)
        subsets <- c(1:length(predictor_names))
        ctrl <- rfeControl(method="repeatedcv",
                           repeats=5,
                           verbose=TRUE,
                           functions=svmFuncs)
        # For the rfe modeling, extract the training data from the main
        # train_data dataset - no need to pass the testing data to rfe
        rfe_x <- scaled_predictors[train_data@training_flag, ]
        rfe_y <- train_data@y[train_data@training_flag, ]
        rfe_res <- rfe(x=rfe_x, rfe_y,
                       sizes=subsets,
                       metric="ROC",
                       rfeControl=ctrl,
                       method="svmRadial",
                       trControl=train_control,
                       tuneGrid=tune_grid)
        #TODO: Extract best model from rfe_res
    } else {
        rfe_res <- NULL
    }

    train_data <- cbind(y=train_data@y,
                        train_data@x,
                        training_flag=train_data@training_flag,
                        poly_src=train_data@pixel_src$src,
                        poly_ID=train_data@pixel_src$ID)

    if (type == 'rf') {
        if (is.null(train_control)) {
            train_control <- trainControl(method="oob", classProbs=TRUE)
        }
        model <- train(model_formula, data=train_data, method="rf",
                       subset=train_data$training_flag,
                       trControl=train_control, tuneGrid=tune_grid, ...)
    } else if (type == 'svm') {
        if (is.null(train_control)) {
            train_control <- trainControl(method="cv", classProbs=TRUE)
        }
        model <- train(model_formula, data=train_data, method="svmRadial",
                       preProc=c('center', 'scale'), subset=train_data$training_flag,
                       trControl=train_control, tuneGrid=tune_grid, ...)
    } else {
        # should never get here
        stop("model type not recognized")
    }

    return(model)
}
#' Function to convert TIMESAT .tts binary format to an R dataframe.
#'
#' @export
#' @param x A .tts file output by TIMESAT
#' @return A data.frame containing 'row' and 'col' columns giving the the row 
#' and column of a pixel in the input image to timesat, and then a number of 
#' columns named 't1', 't2', ...'tn', where n is the total number of image 
#' dates input to TIMESAT.
#' @examples
#' # TODO: Need to add examples here, and need to include a sample TIMESAT tts 
#' # file in the package data.
tts2df <- function(x) {
    if (missing(x) || !grepl('[.]tts$', tolower(x))) {
        stop('must specify a .tts file')
    }

    # Number of elements in the tts file line header (which are normally: row, 
    # column).
    LINE_HEADER_SIZE <- 2

    tts_file_obj <- file(x, "rb")
    raw_vector <- readBin(tts_file_obj, n=file.info(x)$size, raw())
    close(tts_file_obj)

    # This function is used to track the offset within the binary vector as readBin 
    # does not track position except for file objects
    offset <- 1
    raw_vec_length <- length(raw_vector)
    offset_readBin <- function(raw_vec, what, n=n, size=size, ...) {
        bin_data <- readBin(raw_vec[offset:(n * size + offset)], what, n, size, ...)
        # Be lazy and use a global variable to track the offset
        assign("offset", offset + (size*n), inherits=TRUE)
        return(bin_data)
    }

    # File header format is: nyears nptperyear rowstart rowstop colstart colstop
    file_header <- offset_readBin(raw_vector, integer(), n=6, size=4)
    num_years <- file_header[1]
    n_pts_per_year <- file_header[2]
    rowstart <- file_header[3]
    rowstop <- file_header[4]
    colstart <- file_header[5]
    colstop <- file_header[6]

    num_pixels <- (colstop - colstart) * (rowstop - rowstart)

    # Include 2 extra columns to code the row and col IDs
    tts_data <- matrix(nrow=num_pixels, ncol=(2 + n_pts_per_year * num_years))
    for (pixelnum in 1:num_pixels) {
        line_header <- offset_readBin(raw_vector, integer(), n=2, size=4)
        # Line header format is: rownum colnum
        tts_data[pixelnum, ] <- c(line_header[1], line_header[2], 
                                  offset_readBin(raw_vector, numeric(), 
                                                 n=n_pts_per_year*num_years, 
                                                 size=4))
    }
    tts_data <- data.frame(tts_data)

    names(tts_data) <- c("row", "col",
                         paste('t', seq(1, n_pts_per_year*num_years), sep=''))
    return(tts_data)
}
#' Function to convert TIMESAT tts data.frame an R raster.
#'
#' @export
#' @param x A TTS data.frame as output by tts2df
#' @param base_image A string giving the location of a raster file to use 
#' for georeferencing the output raster. Use one of the original raster files 
#' that was input to TIMESAT.
#' @return A raster object
#' @examples
#' # TODO: Need to add examples here, and need to include a sample TIMESAT tpa 
#' # file in the package data.
ttsdf2raster <- function(x, base_image) {
    if (missing(x) || !is.data.frame(x)) {
        stop('must specify a tts data.frame')
    } else if (missing(base_image) || !file.exists(base_image)) {
        stop('must specify a valid base image raster')
    }

    t_cols <- grep('^t[0-9]{1,4}$', names(x))

    base_image <- raster(base_image)

    out_rasters <- c()
    for (t_col in t_cols) {
        this_time_data <- x[, t_col]
        data_matrix <- matrix(NA, nrow(base_image), ncol(base_image))
        vector_indices <- (nrow(data_matrix) * x$col) - 
            (nrow(data_matrix) - x$row)
        data_matrix[vector_indices] <- this_time_data
        out_raster <- raster(data_matrix, template=base_image)
        out_rasters <- c(out_rasters, out_raster)
    }
    out_rasters <- stack(out_rasters)
    names(out_rasters) <- names(x[t_cols])

    return(out_rasters)
}
#' Convert Landsat CDR images from HDF4 to GeoTIFF format
#'
#' This function converts a Landsat surface reflectance (SR) image from the 
#' Landsat Climate Data Record (CDR) archive into a series of single band 
#' images in GeoTIFF format.
#'
#' This function uses \code{gdalUtils}, which requires a local GDAL 
#' installation.  See http://trac.osgeo.org/gdal/wiki/DownloadingGdalBinaries 
#' or http://trac.osgeo.org/osgeo4w/ to download the appropriate installer for 
#' your operating system.
#'
#'
#' @export
#' @importFrom gdalUtils gdal_translate get_subdatasets
#' @importFrom tools file_path_sans_ext
#' @param x input HDF4 file
#' @param output_folder output folder (if \code{NULL}, defaults to input folder)
#' @param overwrite whether to overwrite existing files
#' @param rmhdf whether to remove hdf files after unstacking them
#' @return nothing (used for side effect of converting Landsat CDR HDF files)
#' @examples
#' \dontrun{
#' # Unstack files downloaded from CDR:
#' unstack_ledapscdr('lndsr.LT50150531986021XXX03.hdf')
#' 
#' # Unstack files downloaded from CDR, overwriting any existing files, and 
#' # deleting original HDF files after unstacking:
#' unstack_ledapscdr('lndsr.LT50150531986021XXX03.hdf', overwrite=TRUE, 
#'                   rmhdf=TRUE)
#' }
unstack_ledapscdr <- function(x, output_folder=NULL, overwrite=FALSE, 
                              rmhdf=FALSE) {
    if (is.null(output_folder)) {
        output_folder <- dirname(x)
    }

    if ((!file_test('-f', x)) | (tolower(extension(x)) != '.hdf')) {
        stop('x must be an existing file ending in ".hdf"')
    }

    if (!file_test('-d', output_folder)) {
        stop('output_folder must be a directory')
    }
    out_basename <- file_path_sans_ext(basename(x))

    sds <- get_subdatasets(x)
    loc <- regexpr('[a-zA-Z0-9_-]*$', sds)
    for (n in 1:length(sds)) {
        start_char <- loc[n]
        stop_char <- start_char + attr(loc, 'match.length')[n]
        band_name <- substr(sds[[n]], start_char, stop_char)
        this_out <- paste0(file.path(output_folder, out_basename), '_', band_name, '.tif')
        if (file.exists(this_out)) {
            if (overwrite) {
                unlink(this_out)
                if (file.exists(extension(this_out, 'hdr'))) 
                    unlink(extension(this_out, 'hdr'))
                if (file.exists(extension(this_out, 'tif.aux.xml'))) 
                    unlink(extension(this_out, 'tif.aux.xml'))
                if (file.exists(extension(this_out, 'tif.enp'))) 
                    unlink(extension(this_out, 'tif.enp'))
            } else {
                warning(paste(this_out, 'already exists - skipping file'))
                next
            }
        }
        out_rast <- gdal_translate(x, of="GTiff", sd_index=n, this_out, 
                                   outRaster=TRUE)
    }
    if (rmhdf) {
        if (file.exists(extension(x, 'hdf'))) 
            unlink(extension(x, 'hdf'))
        if (file.exists(extension(x, 'hdr'))) 
            unlink(extension(x, 'hdr'))
        if (file.exists(paste0(file_path_sans_ext(x), '.hdf.hdr')))
            unlink(paste0(file_path_sans_ext(x), '.hdf.hdr'))
        if (file.exists(extension(x, 'txt'))) 
            unlink(extension(x, 'txt'))
    }
}
#'==================================================================================================
#' Unsupervised Classification with R
#'## http://blog.remote-sensing-conservation.org/unsupervised-classifcation-in-r/
#'## http://www.digital-geography.com/unsupervised-classification-of-a-landsat-image-in-r-the-whole-story-or-part-two/
#'## http://stackoverflow.com/questions/10075122/ordering-clustered-points-using-kmeans-and-r
#'
#'# Cluster with k-means -------------------------------------------------------------
#'## http://link.springer.com/chapter/10.1007/978-3-642-24466-7_7
#'## http://link.springer.com/article/10.1007/s00357-010-9049-5
#'
#'# To go further with k-means
#'## validations: http://www.r-statistics.com/2013/08/k-means-clustering-from-r-in-action/
#'## Other sources: http://manuals.bioinformatics.ucr.edu/home/R_BioCondManual#TOC-Clustering-and-Data-Mining-in-R
#'
#'# Particularly relevant: parallel processing
#'## http://www.glennklockwood.com/di/R-para.php
#'
#'# Limitations of raster::calc (from ?calc)
#'## _not_ do, for Raster object x: calc(x, function(x)scale(x, scale=FALSE))
#'## Because the mean value of each chunk will likely be different.
#'## Rather do something like m <- cellStats(x, 'mean'); x - m
#'===================================================================================

#'# Utiliza o stack l8files produzidos pela funcao Landasat_CreateIdrisiFiles_vx.R

#' ADD Vegetation Index -------------------------------------------------------------
#' x: stack object, Default will be the l8files object, with bands 1 to 7
f.VegIndex <- function(x = x, index = 'ndvi'){
  x <- x
  #istk <- x[[1:7]]
  # Vegetation Index (Lansdat 8 OLI)
  ## NDVI = (5-4)/(5+4)
  ## LSWI = (5-6)/(5+6)
  ## NBR - (5-7)/(5+7)
  i.index <- c('ndvi', 'lswi', 'nbr')
  index <- pmatch(index, i.index)
  if (is.na(index)) stop("invalid vegetation index")
  if (index == -1) stop("ambiguous index")
  if(index == 1) {
    out.ind <- (x[[5]]-x[[4]])/(x[[5]]+x[[4]])
  } else if(index == 2){
    out.ind <- (x[[5]]-x[[6]])/(x[[5]]+x[[6]])
  } else if(index == 3){
    out.ind <- (x[[5]]-x[[7]])/(x[[5]]+x[[7]])      
  }
  out.ind
}

#' Specify sensible and meaningful object names
vegind <- f.VegIndex(x = l8files, index = 'ndvi') # stack Bands + VegIndex
vegind2 <- f.VegIndex(x = l830_01_2014bija, index = 'ndvi') # stack Bands + VegIndex
vegind_dif <- vegind30_01_2014bij - vegind03_05_2013bij

plot(vegind_dif)
plot(vegind)

## Export Vegetation Index ---------------------------------------------------------
## 30-01-2014
writeRaster(vegind_dif_bij, file.path(dir.work, dir.tif30012014, dir.rst,
                                      paste0('ndvibij2014-2013.rst')),
            datatype = 'FLT4S', format = 'RST',
            overwrite = TRUE, NAflag = -999)
writeRaster(vegind_dif_bij, file.path(dir.work, dir.tif30012014, dir.rst,
                                      paste0('ndvi2013_2014_2')),
            datatype = 'FLT4S', format = 'RST',
            overwrite = TRUE, NAflag = -999)

## 03-05-2013
writeRaster(vegind03_05_2013, file.path(dir.work, dir.tif03052013, dir.rst,
                                        paste0('ndvi03052013.rst')),
            datatype = 'FLT4S', format = 'RST',
            overwrite = TRUE, NAflag = -999)

#' Build Stacks ---------------------------------------------------------------------
#' Replace NA's with a small real number to run kmeans.
## l8files (1,2,3,4,5,6,7) #
stkfile <- l8files # Sem NDVI
#stk20140214s <- raster::scale(stk20140214)
stkfile <-  reclassify(stkfile, matrix(c(NA, -0.01), nrow = 1))

## Com NDVI
stk30_01_2014 <- l830_01_2014[[2:7]]
stk30_01_2014 <- addLayer(stk30_01_2014, vegind_dif)
stk30_01_2014 <- raster::scale(stk30_01_2014)
stk30_01_2014 <-  reclassify(stk30_01_2014, cbind(NA, NA, -99))

# PCA on Bands 1:6 and retain first 3 Components with > 99% expl var ---------------
f.Pca <- function(x=x, cor = F){
  xdf <- as.data.frame(x)
  pca1 <-  princomp(xdf, cor=cor) 
  pcastk <- stack()
  for(i in 1:3){
    pcax <- matrix(pca1$scores[ ,i], nrow = nrow(x), ncol = ncol(x),
                   byrow = TRUE)
    pcax <- raster(pcax, xmn=x@extent@xmin, ymn=x@extent@ymin,
                   xmx=x@extent@xmax, ymx=x@extent@ymax,
                   crs = CRS(proj4string(mask.ae)))
    pcastk <- addLayer(pcastk, pcax)
  }
  pcastk
}
#'# Provide the stack object for analysis
stkpca <- f.Pca(x=stackobject, cor = F)
plot(stkpca)

#' base::kmeans ---------------------------------------------------------------------
f.Kmeans <- function(x = x, ncl = num.clss, niter.max = 5, nstarts = 5){
  xdf <- as.data.frame(x)
  #xdf <- scale(xdf)
  ikm <- kmeans(xdf, ncl, iter.max = niter.max, nstart = nstarts)
  il8m <- matrix(ikm$cluster, nrow = nrow(x), ncol = ncol(x),
                 byrow = TRUE)
  i.kraster <- raster(il8m, xmn=x@extent@xmin, ymn=x@extent@ymin,
                      xmx=x@extent@xmax, ymx=x@extent@ymax,
                      crs = CRS(proj4string(mask.ae)))
  i.kraster
}

#' Run kmeans function of selected stack object: create a ikmeans raster file with
#'# classes = num.clss
num.clss <- 6
ikmeans <- f.Kmeans(x = stkfile, ncl = num.clss,
                           niter.max = 100, nstarts = 200)
plot(ikmeans)
writeRaster(ikmeans, file.path(dir.work, dir.landsat, dir.tif,
                                      paste0('ikmeans', num.clss,'kumbira','.rst')),
            datatype = 'FLT4S', format = 'RST',
            overwrite = TRUE, NAflag = -9999)

# Flexcluster based on neural gas algorithm ----------------------------------------
f.NgasKmeans <- function(x = x, ncl = num.clss){
  xdf <- as.data.frame(x)
  #xdf <- scale(xdf)
  ikm <- cclust(xdf, ncl, )
  il8m <- matrix(ikm$cluster, nrow = nrow(x), ncol = ncol(x),
                 byrow = TRUE)
  i.kraster <- raster(il8m, xmn=x@extent@xmin, ymn=x@extent@ymin,
                      xmx=x@extent@xmax, ymx=x@extent@ymax,
                      crs = CRS(proj4string(mask.ae)))
  i.kraster
}
num.clss <- 10
ngkmeanspca20140214 <- f.Kmeans(x = stkpca, ncl = num.clss)
plot(ngkmeanspca20140214)
writeRaster(ngkmeanspca20140214, file.path(dir.work, dir.landsat, dir.rst,
                                           paste0('ngkmeans20140214ae_', num.clss,'.rst')),
            datatype = 'FLT4S', format = 'RST',
            overwrite = TRUE, NAflag = -9999)
            
#' Given a spatial object, calculate the UTM zone of the centroid
#'
#' For a line or polygon, the UTM zone of the centroid is given, after 
#' reprojecting the object into WGS-84.
#'
#' Based on the code on gis.stackexchange.com at http://bit.ly/17SdcuN.
#'
#' @export utm_zone
#' @import methods
#' @param x a longitude (with western hemisphere longitudes negative), or a 
#' \code{Spatial} object
#' @param y a latitude (with southern hemisphere latitudes negative), or 
#' missing (if x is a \code{Spatial} object)
#' @param proj4string if FALSE (default) return the UTM zone as a string (for 
#' example "34S" for UTM Zone 34 South). If TRUE, return a proj4string using 
#' the EPSG code as an initialization string.
#' @examples
#' utm_zone(45, 10)
#' utm_zone(45, -10)
#' utm_zone(45, 10, proj4string=TRUE)
setGeneric("utm_zone", function(x, y, proj4string=FALSE) {
    standardGeneric("utm_zone")
})

utm_zone_calc <- function(x, y, proj4string) {
    if (x < -180 || x > 180) {
        stop("longitude must be between -180 and 180")
    }
    if (y < -90 || y > 90) {
        stop("latitude must be between -90 and 90")
    }

    zone_num <- floor((x + 180)/6) + 1
    if (y >= 56.0 && y < 64.0 && x >= 3.0 && x < 12.0) {
        zone_num <- 32
    }

    # Special zone_nums for Svalbard
    if (y >= 72.0 && y < 84.0) {
        if (x >= 0.0 && x < 9.0) {
            zone_num <- 31
        } else if (x >= 9.0 && x < 21.0) {
            zone_num <- 33
        } else if (x >= 21.0 && x < 33.0) {
            zone_num <- 35
        } else if (x >= 33.0 && x < 42.0) {
            zone_num <- 37
        }
    }

    if (y >= 0) {
        ns <- 'N'
    } else {
        ns <- 'S'
    }

    if (proj4string) {
        if (ns == 'N') {
            return(paste0('+init=epsg:326', sprintf('%02i', zone_num)))
        } else {
            return(paste0('+init=epsg:327', sprintf('%02i', zone_num)))
        }
    } else {
        return(paste0(zone_num, ns))
    }
}

#' @rdname utm_zone
#' @aliases utm_zone,numeric,numeric,logical-method
setMethod("utm_zone", signature("numeric", "numeric"),
    function(x, y, proj4string) {
        return(utm_zone_calc(x, y, proj4string))
    }
)

#' @rdname utm_zone
#' @importFrom rgeos gCentroid
#' @importFrom sp Spatial coordinates spTransform
#' @aliases utm_zone,Spatial,missing,logical-method
setMethod("utm_zone", signature(x='Spatial', y='missing'),
    function(x, proj4string) {
        x <- spTransform(x, CRS('+init=epsg:4236'))
        centroid <- coordinates(gCentroid(x))
        return(utm_zone_calc(centroid[1], centroid[2], proj4string))
    }
)
#' Polygon outlining TEAM site in CaxiuanÃ£, Brazil
#' 
#' Contains a SpatialPolygonsDataFrame with a simplified polygon of the area 
#' within the Tropical Ecology Assessment and Monitoring (TEAM) network site in 
#' CaxiuanÃ£, Brazil.
#'
#' @encoding UTF-8
#' @docType data
#' @name test_poly
NULL
.onLoad <- function(libname, pkgname) {
    load(system.file("data", "wrs1_asc_desc.RData", package="wrspathrowData"), 
         envir=parent.env(environment()))
    load(system.file("data", "wrs2_asc_desc.RData", package="wrspathrowData"),
         envir=parent.env(environment()))
}
