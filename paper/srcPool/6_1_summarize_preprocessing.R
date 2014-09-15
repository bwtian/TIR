source('0_settings.R')

scene_topocorr_key <- read.csv('Scene_topocorr_key.csv')

total_preprocessed <- 0
total_not_preprocessed <- 0
writeLines('Site\tDone\tRemaining')
writeLines('-------------------------')
for (sitecode in sitecodes) {
    base_dir <- file.path(prefix, 'Landsat', sitecode)
    image_dirs <- dir(base_dir,
                      pattern='^[0-9]{3}-[0-9]{3}_[0-9]{4}-[0-9]{3}_((LT[45])|(LE7))$', 
                      full.names=TRUE)

    total_dirs <- length(image_dirs)

    if (length(image_dirs) >= 1) {
        not_preprocessed <- image_dirs[!unlist(lapply(image_dirs, is_preprocessed))]
        preprocessed <- image_dirs[unlist(lapply(image_dirs, is_preprocessed))]
    } else {
        preprocessed <- c()
        not_preprocessed <- c()
    }
    total_preprocessed <- total_preprocessed + length(preprocessed)
    total_not_preprocessed <- total_not_preprocessed + length(not_preprocessed)

    # print(not_preprocessed)

    writeLines(paste0(sitecode, '\t', length(preprocessed), '\t', 
                      length(not_preprocessed)))

}
writeLines('-------------------------')
writeLines(paste0('Total:\t', total_preprocessed, '\t', 
                  total_not_preprocessed))
