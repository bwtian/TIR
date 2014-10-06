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
