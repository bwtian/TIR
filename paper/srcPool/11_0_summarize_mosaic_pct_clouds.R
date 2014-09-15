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
