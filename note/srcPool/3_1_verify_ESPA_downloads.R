library(stringr)
library(plyr)

#order_files <- dir('ESPA_orders', pattern='.txt', full.names=TRUE)
order_files <- dir('ESPA_orders', pattern='noexclusions.txt', full.names=TRUE)
read_order_file <- function(order_file) {
    ret <- read.table(order_file, col.names='scene_id')
    ret$scene_id_full <- ret$scene_id
    ret$scene_id <- str_extract(ret$scene_id, '((LE[78])|(LT[45]))[0-9]{13}')
    ret <- cbind(order_file=basename(order_file), ret)
}
ordered_scenes <- ldply(order_files, read_order_file)

downloaded_landsats <- dir('I:/Landsat_Originals', pattern='.tar.gz')
downloaded_scenes <- str_extract(downloaded_landsats, '((LE[78])|(LT[45]))[0-9]{13}')

missing_scenes <- ordered_scenes[!(ordered_scenes$scene_id %in% downloaded_scenes), ]

# write.table(missing_scenes$scene_id_full, 'missing_scenes_order.txt', row.names=FALSE, 
#             col.names=FALSE, quote=FALSE, sep='\n')
write.table(missing_scenes$scene_id_full, 'noexclusions_order.txt', row.names=FALSE, 
            col.names=FALSE, quote=FALSE, sep='\n')

table(missing_scenes$order_file)
