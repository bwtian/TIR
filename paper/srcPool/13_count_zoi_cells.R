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
