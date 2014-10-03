source('0_settings.R')

library(foreach)
library(iterators)
library(doParallel)

registerDoParallel(n_cpus)

library(inline)
library(RcppArmadillo)
library(abind)
library(gdalUtils)
library(rgdal)
library(rgeos)
library(stringr)
library(tools)
library(lubridate)

reprocess <- TRUE
overwrite <- TRUE
builddem <- TRUE
#imgtype <- 'normalized'
imgtype <- 'raw'

stopifnot(imgtype %in% c('normalized', 'raw'))

input_dir <- file.path(prefix, 'Landsat', 'Cloud_Filled')
output_dir <- file.path(prefix, 'Landsat', 'Composites', 'Mosaics')

notify('Starting mosaicking.')
for (sitecode in sitecodes) {
    raster_tmpdir <- file.path(temp, paste0('raster_',
                            paste(sample(c(letters, 0:9), 15), collapse='')))
    dir.create(raster_tmpdir)
    rasterOptions(tmpdir=raster_tmpdir)

    message(paste0('Mosaicking images for ', sitecode, '...'))

    if (imgtype == 'normalized') {
        pattern=paste0('^', sitecode, '_[0-9]{3}-[0-9]{3}_[0-9]{4}-[0-9]{3}_cf(_((normbase)|(normalized))).tif$')
    } else {
        pattern=paste0('^', sitecode, '_[0-9]{3}-[0-9]{3}_[0-9]{4}-[0-9]{3}_cf.tif$')
    }
    image_files <- dir(input_dir, pattern=pattern, full.names=TRUE)
    image_stacks <- lapply(image_files, stack)

    mask_files <- paste0(file_path_sans_ext(image_files), '_masks.tif')
    if (any(!file_test('-f', mask_files))) {
        stop('could not locate mask files')
    }
    mask_stacks <- lapply(mask_files, stack)

    image_date_strings <- unique(str_extract(basename(image_files), 
                                             '_[0-9]{4}-[0-9]{3}_'))

    # Function to get maximal extent from a list of extents
    get_total_extent <- function(extents) {
        full_ext <- extents[[1]]
        extents <- extents[-1]
        while (length(extents) > 0) {
            full_ext <- merge(full_ext, extents[[1]])
            extents <- extents[-1]
        }
        return(full_ext)
    }
    # First calculate maximal extent of each stack of images, considering all 
    # the dates to ensure that the mosaic extents match for each epoch even if 
    # some epochs are missing path/rows
    mos_exts <- foreach(image_date_string=iter(image_date_strings),
                        .packages=c('raster', 'rgdal', 'lubridate', 'tools', 
                                    'foreach', 'iterators')) %dopar% {
        epoch_image_files <- image_files[grepl(image_date_string, image_files)]
        # Calculate full extent of mosaic
        extents <- lapply(epoch_image_files, function(x) {
            x <- stack(x)
            extent(x)
        })
        get_total_extent(extents)
    }
    mos_ext <- get_total_extent(mos_exts)

    # Calculate extents, with origins at 0,0
    mosaic_te <- as.numeric(bbox(mos_ext))
    tr <- c(30, 30)
    # Setup xmin
    mosaic_te[1] <- round(mosaic_te[1] - mosaic_te[1] %% tr[1])
    # Setup ymin
    mosaic_te[2] <- round(mosaic_te[2] - mosaic_te[2] %% tr[2])
    # Setup xmax
    mosaic_te[3] <- round(mosaic_te[3] + tr[1] - mosaic_te[3] %% tr[1])
    # Setup ymax
    mosaic_te[4] <- round(mosaic_te[4] + tr[2] - mosaic_te[4] %% tr[2])
    stopifnot(all(round(mosaic_te / 30) == (mosaic_te / 30)))

    ###########################################################################
    # Iterate over the images to mosaick
    mosaic_stacks <- foreach(image_date_string=iter(image_date_strings),
                             .packages=c('raster', 'rgdal', 'lubridate', 
                                         'tools', 'foreach', 'iterators',
                                         'gdalUtils', 'RcppArmadillo', 
                                         'inline', 'abind'),
                             .combine=c) %dopar% {
        raster_tmpdir <- file.path(temp, paste0('raster_',
                                   paste(sample(c(letters, 0:9), 15), collapse='')))
        dir.create(raster_tmpdir)
        rasterOptions(tmpdir=raster_tmpdir)

        image_date_object <- as.Date(image_date_string, '_%Y-%j_')

        epoch_image_files <- image_files[grepl(image_date_string, image_files)]
        epoch_mask_files <- paste0(file_path_sans_ext(epoch_image_files), '_masks.tif')
        stopifnot(all(file_test('-f', epoch_image_files)))
        stopifnot(all(file_test('-f', epoch_mask_files)))

        if (imgtype == 'normalized') {
            mosaic_out_file <- file.path(output_dir,
                                         paste0(sitecode, '_mosaic_normalized_', 
                                                year(image_date_object),  
                                            extension(epoch_image_files[1])))
        } else {
            mosaic_out_file <- file.path(output_dir,
                                         paste0(sitecode, '_mosaic_', 
                                                year(image_date_object),  
                                            extension(epoch_image_files[1])))
        }
        if (file_test('-f', mosaic_out_file) & !reprocess) {
            return()
        }

        mask_out_file <- paste0(file_path_sans_ext(mosaic_out_file), '_masks', 
                                extension(mosaic_out_file))

        #######################################################################
        # Need to align images prior to mosaicking if origins are not identical
        image_origins <- lapply(epoch_mask_files, function(x) 
                                origin(raster(x)))
        image_origins_eq  <- lapply(image_origins, function(x) {
            identical(x, c(0, 0))
        })

        if (all(unlist(image_origins_eq))) {
            epoch_image_files_aligned <- epoch_image_files
            epoch_mask_files_aligned <- epoch_mask_files
        } else {
            aligned_files <- foreach (epoch_image_file=iter(epoch_image_files),
                                      epoch_mask_file=iter(epoch_mask_files),
                                      .combine=rbind) %do% {
                mask_vrtfile <- extension(rasterTmpFile(), '.vrt')
                # The hidenodata and vrtnodata lines below ensure that no data 
                # areas are coded 255 in the output mosaic (consistent with the 
                # Landsat CDR mask coding for fill area).
                gdalbuildvrt(epoch_mask_file, mask_vrtfile, vrtnodata=255, 
                             hidenodata=FALSE, te=mosaic_te, tr=c(30, 30))
                mask_tiffile <- extension(rasterTmpFile(), '.tif')
                gdalwarp(mask_vrtfile, dstfile=mask_tiffile, r='near', 
                         of='GTiff', overwrite=overwrite, ot='Byte', 
                         co="COMPRESS=LZW")

                image_tiffile <- extension(rasterTmpFile(), '.tif')
                gdalwarp(epoch_image_file, dstfile=image_tiffile, 
                         r='cubicspline', of='GTiff', overwrite=overwrite, 
                         te=mosaic_te, tr=c(30, 30), ot='Int16', 
                         co="COMPRESS=LZW BIGTIFF=IF_SAFER")
                return(data.frame(msk=mask_tiffile, img=image_tiffile, 
                                  stringsAsFactors=FALSE))
            }
            epoch_mask_files_aligned <- aligned_files$msk
            epoch_image_files_aligned <- aligned_files$img
        }

        #######################################################################
        # Mosaick the images

        # Define a function to determine the output value for a block of 
        # pixels, based on a stack of masks and reflectance from multiple 
        # overlapping images. Use RcppArmadillo for speed.
        src <- '
            using namespace arma;

            // Read input data into arrays
            Rcpp::NumericVector img_vecArray(img);
            Rcpp::IntegerVector img_arrayDims = img_vecArray.attr("dim");
            cube img_cube(img_vecArray.begin(), img_arrayDims[0], 
                          img_arrayDims[1], img_arrayDims[2], false);

            Rcpp::NumericVector msk_vecArray(msk);
            Rcpp::IntegerVector msk_arrayDims = msk_vecArray.attr("dim");
            cube msk_cube(msk_vecArray.begin(), msk_arrayDims[0], 
                          msk_arrayDims[1], msk_arrayDims[2], false);

            mat fillnofill_mat = msk_cube.tube(0, 0, msk_cube.n_rows - 1, 0);
            mat fmask_mat = msk_cube.tube(0, 1, msk_cube.n_rows - 1, 1);

            //Rcpp::Rcout << "fillnofill 0" << sum(fillnofill_mat == 0) << std::endl;
            //Rcpp::Rcout << "fillnofill 1" << sum(fillnofill_mat == 1) << std::endl;
            //Rcpp::Rcout << "fillnofill 2" << sum(fillnofill_mat == 2) << std::endl;
            //Rcpp::Rcout << "fillnofill 3" << sum(fillnofill_mat == 3) << std::endl;
            //Rcpp::Rcout << "fillnofill 4" << sum(fillnofill_mat == 4) << std::endl;
            //Rcpp::Rcout << "fillnofill 255" << sum(fillnofill_mat == 255) << std::endl;
            //Rcpp::Rcout << "fmask 0" << sum(fmask_mat == 0) << std::endl;
            //Rcpp::Rcout << "fmask 1" << sum(fmask_mat == 1) << std::endl;
            //Rcpp::Rcout << "fmask 2" << sum(fmask_mat == 2) << std::endl;
            //Rcpp::Rcout << "fmask 3" << sum(fmask_mat == 3) << std::endl;
            //Rcpp::Rcout << "fmask 4" << sum(fmask_mat == 4) << std::endl;
            //Rcpp::Rcout << "fmask 255" << sum(fmask_mat == 255) << std::endl;

            //Rcpp::Rcout << fillnofill_mat.n_rows << std::endl;
            //Rcpp::Rcout << fillnofill_mat.n_cols << std::endl;
            //Rcpp::Rcout << fmask_mat.n_rows << std::endl;
            //Rcpp::Rcout << fmask_mat.n_cols << std::endl;

            // Create output arrays
            // cols in mask are fill mask (col 0) and fmask (col 1)
            mat msk_out(msk_arrayDims[0], msk_arrayDims[1]);
            // cols in image are bands
            mat img_out(img_arrayDims[0], img_arrayDims[1]);

            // Make a uvec to track cloud/shadow/fill pixels
            uvec gooddata_indices(img_out.n_cols);
            for (unsigned pixelnum = 0; pixelnum < img_cube.n_rows; pixelnum++) {

                // Load the fill/not fill band just to mosaick it
                rowvec fillnofill_pixel = fillnofill_mat.row(pixelnum);

                // Load the fmask band and use this band for masking the image
                // during mosaicking
                rowvec fmask_pixel = fmask_mat.row(pixelnum);

                gooddata_indices = vectorise((fmask_pixel != 2) &&
                                             (fmask_pixel != 4) &&
                                             (fmask_pixel != 255));
                gooddata_indices = find(gooddata_indices);

                // Note that img_pixel is a mat with n_layers x n_images
                mat img_pixel = img_cube.tube(pixelnum, 0, pixelnum, img_out.n_cols - 1);

                if (gooddata_indices.n_elem > 0) {
                    //Rcpp::Rcout << "got here" << std::endl;
                    if (img_cube.n_slices == 1) {
                        img_out.row(pixelnum) = img_pixel;
                    } else {
                        img_out.row(pixelnum) = trans(mean(img_pixel.cols(gooddata_indices), 1));
                    }
                    // Take a conservative approach to merging masks - highest code 
                    // takes precedence (snow (3) over water (1) over clear (0)).
                    msk_out(pixelnum, 0) = 0;
                    msk_out(pixelnum, 1) = max(fmask_pixel(gooddata_indices));
                } else {
                    //Rcpp::Rcout << "got here 1" << std::endl;
                    img_out.row(pixelnum).fill(datum::nan);
                    // No data in this pixel. Use the lowest code in the
                    // output mosaic. This means cloud takes precedence
                    // over cloud shadow, which takes precedence over fill
                    msk_out(pixelnum, 0) = min(fillnofill_pixel);
                    msk_out(pixelnum, 1) = min(fmask_pixel);
                }
            }
             
            return Rcpp::List::create(Rcpp::Named("img") = img_out,
                                      Rcpp::Named("msk") = msk_out);
        '
        mosaic_block <- cxxfunction(signature(img="numeric", msk="numeric"),
                                     body=src, plugin="RcppArmadillo")

        # results <- mosaic_block(image_array, mask_array)
        #
        # plot(raster(matrix(results$img[, 1], nrow=bs$nrows[block_num], 
        #                    byrow=TRUE)))
        #
        # plot(raster(matrix(results$msk[, 1], nrow=bs$nrows[block_num], 
        #                    byrow=TRUE)))
        #
        # plot(raster(matrix(results$msk[, 2], nrow=bs$nrows[block_num], 
        #                    byrow=TRUE)))
        #
        # plot(raster(matrix(image_array[, 1, 1], nrow=bs$nrows[block_num], 
        #                    byrow=TRUE)))
        #
        # plot(raster(matrix(mask_array[, 1, 1], nrow=bs$nrows[block_num], 
        #                    byrow=TRUE)))
        #
        # plot(raster(matrix(mask_array[, 2, 1], nrow=bs$nrows[block_num], 
        #                    byrow=TRUE)))

        if (length(epoch_image_files_aligned) > 1) {
            epoch_mask_stacks <- lapply(epoch_mask_files_aligned, stack)
            epoch_image_stacks <- lapply(epoch_image_files_aligned, stack)
        } else {
            epoch_mask_stacks <- list(stack(epoch_mask_files_aligned))
            epoch_image_stacks <- list(stack(epoch_image_files_aligned))
        }
        sample_mask <- epoch_mask_stacks[[1]]
        sample_image <- epoch_image_stacks[[1]]

        mask_out <- brick(sample_mask, values=FALSE)
        # Set NAflag to 99 as a kludge - writeRaster doesn't allow omitting an 
        # NAflag, and I don't want 255 to be flagged as nodata
        mask_out <- writeStart(mask_out, filename=mask_out_file, 
                               overwrite=overwrite, datatype='INT1U', 
                               NAflag=99)
        image_out <- brick(sample_image, values=FALSE)
        image_out <- writeStart(image_out, filename=mosaic_out_file, 
                                overwrite=overwrite, datatype='INT2S')
        bs <- blockSize(sample_image)
        for (block_num in 1:bs$n) {
            image_dims <- c(bs$nrows[block_num], ncol(sample_image), 
                            nlayers(sample_image))
            mask_dims <- c(bs$nrows[block_num], ncol(sample_mask), 
                           nlayers(sample_mask))
            # Make image and mask arrays. Image array has nrow*ncol rows, 
            # n_layers columns, and n_images in z direction. Mask array has 
            # nrow*ncol rows, and n_images columns
            for (image_num in 1:length(epoch_image_files_aligned)) {
                image_bl <- array(getValuesBlock(epoch_image_stacks[[image_num]], 
                                                 row=bs$row[block_num], 
                                                 nrows=bs$nrows[block_num]),
                                  dim=c(image_dims[1] * image_dims[2], image_dims[3], 1))
                mask_bl <- array(getValuesBlock(epoch_mask_stacks[[image_num]], 
                                                row=bs$row[block_num], 
                                                nrows=bs$nrows[block_num]),
                                 dim=c(mask_dims[1] * mask_dims[2], mask_dims[3], 1))
                if (image_num == 1) {
                    image_array <- image_bl
                    mask_array <- mask_bl
                } else {
                    image_array <- abind(image_array, image_bl, along=3)
                    mask_array <- abind(mask_array, mask_bl, along=3)
                }
            }
            mosaicked_block <- mosaic_block(image_array, mask_array)
            mask_out <- writeValues(mask_out, mosaicked_block$msk, 
                                    bs$row[block_num])
            image_out <- writeValues(image_out, mosaicked_block$img, 
                                     bs$row[block_num])
        }
        mask_out <- writeStop(mask_out)
        image_out <- writeStop(image_out)

#         epoch_images <- lapply(epoch_image_files, brick)
#         epoch_masks <- lapply(epoch_mask_files, brick)
#
#         masked_epoch_image_files <- foreach(epoch_image=iter(epoch_images), 
#                                             epoch_mask=iter(epoch_masks), 
#                                             .packages=c('raster', 'rgdal', 
#                                                         'gdalUtils'),
#                                             .combine=c) %do% {
#             # Make sure clouds are masked out (NA) and make sure missing values 
#             # and SLC-off is masked out.
#             #
#             # Remember layer 2 is fmask
#             out_file <- extension(rasterTmpFile(), '.tif')
#             epoch_image <- overlay(epoch_image, epoch_mask[[2]], 
#                                    fun=function(img, msk) {
#                 img[(msk == 2) | (msk == 4) | (msk == 255)] <- NA 
#                 img[is.na(msk)] <- NA 
#                 return(img)
#             }, datatype=dataType(epoch_image)[1], filename=out_file)
#             return(out_file)
#         }
#
#         mask_vrtfile <- tempfile(fileext='.vrt')
#         # The hidenodata and vrtnodata lines below ensure that no data areas 
#         # are coded 255 in the output mosaic (consistent with the Landsat CDR 
#         # mask coding for fill area).
#         gdalbuildvrt(epoch_mask_files, mask_vrtfile, vrtnodata=255, 
#                      hidenodata=FALSE, te=mosaic_te, tr=c(30, 30))
#         mask_stack <- gdalwarp(mask_vrtfile, dstfile=mask_out_file, 
#                                r='near', output_Raster=TRUE, of='GTiff', 
#                                overwrite=overwrite, multi=TRUE, 
#                                wo=paste0("NUM_THREADS=", n_cpus), 
#                                ot='Byte', co="COMPRESS=LZW")
#
#         image_stack <- gdalwarp(masked_epoch_image_files,
#                                 dstfile=mosaic_out_file,
#                                 r='cubicspline', output_Raster=TRUE, 
#                                 of='GTiff',
#                                 overwrite=overwrite, multi=TRUE, 
#                                 wo=paste0("NUM_THREADS=", n_cpus), 
#                                 te=mosaic_te, tr=c(30, 30),
#                                 ot='Int16', co="COMPRESS=LZW")
        removeTmpFiles(h=0)
        unlink(raster_tmpdir)
    }

    # Check extents of all mosaics are equal
    if (imgtype == 'normalized') {
        pattern <- paste0('^', sitecode, '_mosaic_normalized_[0-9]{4}.tif$')
    } else {
        pattern <- paste0('^', sitecode, '_mosaic_[0-9]{4}.tif$')
    }
    mosaic_files <- dir(output_dir, pattern=pattern, full.names=TRUE)
    mosaic_stacks <- lapply(mosaic_files, stack)
    mos_exts <- lapply(mosaic_stacks, extent)
    for (mos_ext in mos_exts) {
        stopifnot(mos_ext == mos_exts[[1]])
    }

    dem_mosaic_filename <- file.path(output_dir,
                                     paste0(sitecode, '_mosaic_dem.tif'))
    if (builddem & (!file_test('-f', dem_mosaic_filename) | reprocess)) {
        message(paste0('Mosaicking DEMs for ', sitecode, '...'))
        
        mos_ext <- as(mos_exts[[1]], 'SpatialPolygons')

        proj4string(mos_ext) <- proj4string(mosaic_stacks[[1]])

        mos_ext_dem_proj <- spTransform(mos_ext, CRS(proj4string(dem_extents)))

        intersecting <- as.logical(gIntersects(dem_extents, 
                                               gUnaryUnion(mos_ext_dem_proj), byid=TRUE))
        if (sum(intersecting) == 0) {
            stop('no intersecting dem extents found')
        }

        dem_list <- dem_extents[intersecting, ]$filename
        dem_rasts <- lapply(dem_list, raster)

        to_srs <- proj4string(mosaic_stacks[[1]])

        # Calculate minimum bounding box coordinates:
        dem_te <- as.numeric(bbox(mos_ext))
        to_res <- c(30, 30)
        dem_mosaic <- gdalwarp(dem_list, dstfile=dem_mosaic_filename,
                               te=dem_te, t_srs=to_srs, tr=to_res, 
                               r='cubicspline', output_Raster=TRUE, multi=TRUE, 
                               of='GTiff', wo=paste0("NUM_THREADS=", n_cpus), 
                               overwrite=overwrite)

        # Note that the default output of 'terrain' is in radians
        slopeaspect <- terrain(dem_mosaic, opt=c('slope', 'aspect'))
        slopeaspect$aspect <- calc(slopeaspect$aspect, fun=function(vals) {
            vals[vals >= 2*pi] <- 0
            vals
            })
        # Note that slopeaspect is scaled - slope by 10000, and aspect by 1000 so 
        # that the layers can be saved as INT2S
        slopeaspect <- stack(round(raster(slopeaspect, layer=1) * 10000),
                             round(raster(slopeaspect, layer=2) * 1000))

        slopeaspect_mosaic_filename <- file.path(output_dir,
                                         paste0(sitecode, '_mosaic_slopeaspect.tif'))
        slopeaspect <- writeRaster(slopeaspect, filename=slopeaspect_mosaic_filename, 
                                 overwrite=overwrite, datatype='INT2S')
    }

    removeTmpFiles(h=0)
    unlink(raster_tmpdir)
}
notify('Finished mosaicking.')
