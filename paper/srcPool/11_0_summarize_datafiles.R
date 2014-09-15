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
