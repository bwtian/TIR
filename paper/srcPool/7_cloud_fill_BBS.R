library(teamlucc)

# library(tools)
# library(lubridate)
# library(stringr)
# library(SDMTools)

prefix <- 'D:/azvoleff/Data'
sitecode <- 'BBS'
data_dir <- file.path(prefix, 'Landsat', sitecode)
wrspath <- 124
wrsrow <- 64
start_date <- as.Date('1988/1/1')
end_date <- as.Date('1992/12/31')
base_date <- NULL
mid_date <- (end_date - start_date)/2 + start_date
out_base <- file.path(data_dir,
                      paste0(sitecode,
                             sprintf('_%03i-%03i_', wrspath, wrsrow),
                             format(mid_date, '%Y-%j'), '_cf'))
out_name <- paste0(out_base, '.tif')
tc <- TRUE
sensors <- list('L4T', 'L5T', 'L7E', 'L8E')
overwrite <- TRUE
verbose <- 2
byblock <- FALSE
notify <- print
overwrite <- TRUE
threshold <- 1
ext <- 'tif'
max_iter <- 5
DN_min <- -100
DN_max <- 16000
algorithm <- 'CLOUD_REMOVE_FAST'

options(error=recover)

auto_cloud_fill(data_dir, wrspath, wrsrow, start_date, end_date, 
                out_name=out_name, tc=tc, sensors=sensors,
                overwrite=TRUE, verbose=2, DN_min=-100, 
                DN_max=16000, algorithm='CLOUD_REMOVE_FAST', byblock=FALSE)
