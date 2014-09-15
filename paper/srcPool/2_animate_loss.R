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
