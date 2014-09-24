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
