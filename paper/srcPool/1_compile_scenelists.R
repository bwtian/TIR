source('0_settings.R')

library(stringr)

order_folder <- 'ESPA_orders'
scenelist_dir <- file.path(prefix, 'Landsat_scenelists')

###############################################################################
# Load scene lists as output by Earth Explorer

scenelist_regex <- '[A-Z]{2,3}_L((4-5)|7)_[0-9]{8}_scenelist.csv$'
scenelist_files <- dir(scenelist_dir, pattern=scenelist_regex, full.names=TRUE)
codes <- str_extract(basename(scenelist_files), '[A-Z]{2,3}')
sats <- str_extract(basename(scenelist_files), '(L4-5)|(L7)')

n <- 1
for (scenelist_file in scenelist_files) {
    message(paste0('Progress: ', round(n/length(scenelist_files)*100), '%'))
    these_scenes <- ee_read(scenelist_file)
    these_scenes$scenelist_file <- scenelist_files[n]
    these_scenes$sitecode <- codes[n]
    these_scenes$sats <- sats[n]
    if (n == 1) {
        scenes <- these_scenes
    } else {
        scenes <- merge(scenes, these_scenes, all=TRUE)
    }
    n <- n + 1
}

# # Exclude tiles that are not specifically included
# include <- read.csv('Included_tiles.csv')
# scenes <- scenes[(paste(scenes$WRS.Path, scenes$WRS.Row) %in%
#                   paste(include$path, include$row)), ]
#
# is_included <- paste(include$path, include$row) %in% paste(scenes$WRS.Path, scenes$WRS.Row)
# if (!all(is_included)) {
#     stop('missing some included scenes')
# }

save(scenes, file='1_all_scenes_noexclusions.RData')
