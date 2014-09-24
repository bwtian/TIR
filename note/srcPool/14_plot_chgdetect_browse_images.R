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
