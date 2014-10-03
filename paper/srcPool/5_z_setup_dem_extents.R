source('0_settings.R')

dem_path <- file.path(prefix, 'CGIAR_SRTM')

dems <- lapply(dir(dem_path, pattern='.tif$', full.names=TRUE), raster)
dem_extents <- get_extent_polys(dem_path)

save(dem_extents, file='dem_extents.RData')
