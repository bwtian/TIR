source('0_settings.R')

library(stringr)

library(foreach)
library(doParallel)

registerDoParallel(n_cpus)

scene_topocorr_key <- read.csv('Scene_topocorr_key.csv')

overwrite <- TRUE
reprocess <- TRUE

verbose <- TRUE
algorithm <- 'CLOUD_REMOVE_FAST'
#algorithm <- 'simple'

start_dates <- as.Date(c('1988/1/1',
                         '1993/1/1',
                         '1998/1/1',
                         '2003/1/1',
                         '2008/1/1'))
end_dates <- as.Date(c('1992/12/31',
                       '1997/12/31',
                       '2002/12/31',
                       '2007/12/31', 
                       '2012/12/31'))
sensors_bydate <- list(c('L4T', 'L5T', 'L7E', 'L8E'),
                       c('L4T', 'L5T', 'L7E', 'L8E'),
                       c('L4T', 'L5T', 'L7E', 'L8E'),
                       c('L4T', 'L5T', 'L7E', 'L8E'),
                       c('L4T', 'L5T', 'L8E'))
# start_dates <- as.Date(c('1988/1/1', '1998/1/1', '2008/1/1'))
# end_dates <- as.Date(c('1992/12/31', '2002/12/31', '2012/12/31'))
# sensors_bydate <- list(c('L4T', 'L5T', 'L7E', 'L8E'),
#                        c('L4T', 'L5T', 'L7E', 'L8E'),
#                        c('L4T', 'L5T', 'L8E'))
# start_dates <- as.Date(c('1998/1/1', '2008/1/1'))
# end_dates <- as.Date(c('2002/12/31', '2012/12/31'))
# sensors_bydate <- list(c('L4T', 'L5T', 'L7E', 'L8E'),
#                        c('L4T', 'L5T', 'L8E'))
stopifnot(length(start_dates) == length(end_dates))
stopifnot(length(start_dates) == length(sensors_bydate))

output_dir <- file.path(prefix, 'Landsat', 'Cloud_Filled')

sitecodes_rep <- c()
base_dirs <- c()
wrspaths <- c()
wrsrows <- c()
tcs <- c()
for (sitecode in sitecodes) {
    this_base_dir <- file.path(prefix, 'Landsat', sitecode)
    image_dirs <- dir(this_base_dir, pattern='^[0-9]{3}-[0-9]{3}_[0-9]{4}-[0-9]{3}_((LE)|(LT))[4578]$')
    wrspathrows <- unique(str_extract(image_dirs, '^[0-9]{3}-[0-9]{3}_'))
    these_wrspaths <- as.numeric(gsub('[-]', '', str_extract(wrspathrows, '^[0-9]{3}-')))
    these_wrsrows <- as.numeric(gsub('[_-]', '', str_extract(wrspathrows, '-[0-9]{3}_')))

    tc_key_rows <- match(paste(sitecode, these_wrspaths, these_wrsrows),
                         with(scene_topocorr_key, paste(sitecode, wrspath, wrsrow)))
    new_tcs <- scene_topocorr_key$do_tc[tc_key_rows]
    tcs <- c(tcs, new_tcs)

    wrspaths <- c(wrspaths, these_wrspaths)
    wrsrows <- c(wrsrows, these_wrsrows)
    base_dirs <- c(base_dirs, rep(this_base_dir, length(these_wrspaths)))
    sitecodes_rep <- c(sitecodes_rep, rep(sitecode, length(these_wrspaths)))
}

# sitecode <- sitecodes_rep[1]
# base_dir <- base_dirs[1]
# wrspath <- wrspaths[1]
# wrsrow <- wrsrows[1]
#
# start_date <- start_dates[1]
# end_date <- end_dates[1]

stopifnot(length(sitecodes_rep) == length(base_dirs))
stopifnot(length(sitecodes_rep) == length(wrspaths))
stopifnot(length(sitecodes_rep) == length(wrsrows))
stopifnot(length(sitecodes_rep) == length(tcs))

foreach (sitecode=iter(sitecodes_rep), base_dir=iter(base_dirs), 
         wrspath=iter(wrspaths), wrsrow=iter(wrsrows), tc=iter(tcs),
         .inorder=FALSE)  %:% 
    foreach (start_date=iter(start_dates), end_date=(end_dates), 
             sensors=iter(sensors_bydate),
             .packages=c('teamlucc', 'raster', 'sp'),
             .inorder=FALSE) %dopar% {
        mid_date <- (end_date - start_date)/2 + start_date
        out_base <- file.path(output_dir,
                              paste0(sitecode,
                                     sprintf('_%03i-%03i_', wrspath, wrsrow),
                                     format(mid_date, '%Y-%j'), '_cf'))

        status_line <- paste0(sitecode, ' ', wrspath, '/', wrsrow, ' (', 
                              format(start_date, '%Y/%d/%m'), ' - ', 
                              format(end_date, '%Y/%d/%m'), ')')

        output_file <- paste0(out_base, ext)
        if (file_test('-f', output_file)) {
            if (!reprocess) return()
            if (!overwrite) stop(paste(output_file, 'already exists'))
        }

        # Set a separate raster temp dir for each worker, so that temp 
        # files can be cleared after each iteration
        rasterOptions(tmpdir=paste0(tempdir(), '_raster'))

        tryCatch(cf <- auto_cloud_fill(base_dir, wrspath, wrsrow, start_date, 
                                       end_date, out_name=out_base, tc=tc, 
                                       sensors=sensors, n_cpus=1, 
                                       overwrite=overwrite, verbose=verbose, 
                                       DN_min=-100, DN_max=16000, 
                                       algorithm=algorithm, byblock=FALSE),
                 error=function(e) {
                     print(paste(status_line, 'FAILED'))
                 })

        removeTmpFiles(h=0)
    }
