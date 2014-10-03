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
