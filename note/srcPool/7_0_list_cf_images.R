source('0_settings.R')

library(stringr)

input_dir <- file.path(prefix, 'Landsat', 'Cloud_Filled')

image_files <- dir(input_dir, 
                   pattern=paste0('^[a-zA-Z]*_[0-9]{3}-[0-9]{3}_[0-9]{4}-[0-9]{3}_cf.tif$'))

# First figure out path/rows and dates
wrspathrows <- gsub('[_]', '', str_extract(basename(image_files), 
                                            '_[0-9]{3}-[0-9]{3}_'))
wrspath <- substr(wrspathrows, 1, 3)
wrsrow <- substr(wrspathrows, 5, 7)

image_dates <- as.Date(str_extract(basename(image_files), 
                                   '_[0-9]{4}-[0-9]{3}_'), '_%Y-%j_')
site <- str_extract(image_files, '^[a-zA-Z]*')

cf_file_list <- data.frame(site=site, path=wrspath, row=wrsrow, 
                           date=image_dates, rating='', notes='', 
                           file=image_files)

cf_file_list <- cf_file_list[order(cf_file_list$site,
                                   cf_file_list$path,
                                   cf_file_list$row,
                                   cf_file_list$date), ]

# Ensure existing file is not overwritten
csv_output_file <- '7_0_cf_image_list.csv'
stopifnot(!(file_test('-f', csv_output_file)))

write.csv(cf_file_list, file=csv_output_file, row.names=FALSE)
