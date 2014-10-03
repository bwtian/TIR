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
