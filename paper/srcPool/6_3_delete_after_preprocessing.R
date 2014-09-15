source("0_settings.R")

#in_folder <- 'H:/Data/Landsat'
#in_folder <- 'Z:/Data/Landsat'
in_folder <- 'O:/Data/CI_H_Data/Landsat'

for (sitecode in sitecodes) {
    print(paste0('*** Processing ', sitecode, ' ***'))
    image_dirs <- dir(file.path(in_folder, sitecode),
                      pattern='^[0-9]{3}-[0-9]{3}_[0-9]{4}-[0-9]{3}_((LT[45])|(LE7))$', 
                      full.names=TRUE)

    if (length(image_dirs) >= 1) {
        # Do nothing if not preprocessed
        image_dirs <- image_dirs[unlist(lapply(image_dirs, is_preprocessed))]
        total_dirs <- length(image_dirs)
    }

    print(paste(total_dirs, 'images are preprocessed'))
    for (image_dir in image_dirs) {
        lndsr_regex <- '^lndsr.((LT4)|(LT5)|(LE7)|(LE8))[0-9]{6}[12][0-9]{6}[a-zA-Z]{3}[0-9]{2}((.hdf(.hdr)?)|(.txt))'
        lndsr_files <- dir(image_dir, pattern=lndsr_regex)
        if (length(lndsr_files) > 1) {
            print(paste('Deleting files from', image_dir))
            ret <- unlink(file.path(image_dir, lndsr_files))
            if (ret != 0) {
                stop(paste('failed to delete files in', image_dir))
            }
        }
    }
}
