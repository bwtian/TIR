source('0_settings.R')

library(foreach)
library(itertools)

library(raster)

library(ggplot2)
library(dplyr)

chgdetect_dir <- file.path(prefix, 'Landsat', 'Composites', 'Change_Detection')
out_dir <- chgdetect_dir

thresholds_files <- dir(chgdetect_dir,
                        pattern='^[a-zA-Z]*_[0-9]{4}-[0-9]{4}_chgdetect_threshold.txt$',
                        full.names=TRUE)
thresholds <- foreach(thresholds_file=iter(thresholds_files),
                      .combine=rbind, .inorder=FALSE) %do% {
    read.csv(thresholds_file)
}

summarize(group_by(thresholds, sitecode), 
          min=min(threshold),
          max=max(threshold),
          mean=mean(threshold),
          range=diff(range(threshold)))

ggplot(thresholds, aes(year_1, threshold, colour=sitecode)) + geom_line()

write.csv(thresholds, file='thresholds.csv', row.names=FALSE)

# chgmag_files <- dir(chgdetect_dir,
#                     pattern='^[a-zA-Z]*_[0-9]{4}-[0-9]{4}_chgdetect_chgmag.tif$',
#                     full.names=TRUE)
# chgmag_hists <- foreach(chgmag_file=iter(chgmag_files),
#                         .packages=c('ggplot2', 'dplyr'),
#                         .combine=rbind, .inorder=FALSE) %do% {
#
#     chgmag <- raster(chgmag_file)
#     
#     vals <- sampleRegular(chgmag, 100000, useGDAL=TRUE)
#     hist(vals)
#     hiu
#     sitecode <- str_extract(basename(chgmag_file), '^[a-zA-Z]*')
