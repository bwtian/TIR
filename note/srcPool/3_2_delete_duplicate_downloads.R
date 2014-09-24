library(dplyr)
library(stringr)

base_dir <- 'I:/Landsat_Originals'

dl_landsats <- dir(base_dir, pattern='.tar.gz')
dl_scenes <- str_extract(dl_landsats, '((LE[78])|(LT[45]))[0-9]{13}')
dl_scenes_process_times <- gsub('SC', '', str_extract(dl_landsats, 'SC[0-9]{14}'))
dl_scenes_process_times <- strptime(dl_scenes_process_times, format='%Y%m%d%H%M%S', tz='GMT')
scene_date <- as.Date(substr(dl_scenes, 10, 16), '%Y%j')

dl_scenes <- data.frame(filename=dl_landsats,
                        scene_id=dl_scenes, 
                        process_time=dl_scenes_process_times,
                        date=scene_date)

dim(dl_scenes)

rm_duplicate_scenes <- function(scene_group) {
    if (nrow(scene_group) > 1) {
        scene_group <- scene_group[order(scene_group$scene_id), ]
        del_scenes <- data.frame(path=file.path(base_dir, scene_group$filename[1:(nrow(scene_group) - 1)]))
        unlink(del_scenes$path)
    } else {
        del_scenes <- data.frame()
    }
    return(del_scenes)
}

# Note that below line gives an error if no scenes are deleted
del_scenes <- do(group_by(dl_scenes, scene_id), rm_duplicate_scenes(.))
