source('0_settings.R')

library(stringr)
library(tools)
library(dplyr)
library(reshape2)

training_pixel_files <- dir(file.path(prefix, 'Landsat', 'Composites', 'Models'),
                            pattern='trainingpixels.RData$', full.names=TRUE)

out_file <- 'training_pixel_summary.txt'

sink(out_file)
for (training_pixel_file in training_pixel_files) {
    cat('*******************************************************************************\n')
    cat(training_pixel_file)
    cat('\n')
    load(training_pixel_file)
    print(summary(tr_pixels))
    cat('\n')
}
sink()
