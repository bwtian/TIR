library(teamlucc)

# load('1_all_scenes.RData')
load('1_all_scenes_noexclusions.RData')

###############################################################################
# Output ESPA order files

# # Order all scenes with less than 10% cloud cover
# espa_scenelist(scenes, as.Date('1980/1/1'), as.Date('2015/1/1'), min_clear=.9,
#                file.path(order_folder, 'scenes_allsites_lt10cloud.txt'))
# # Order all scenes with less than 5% cloud cover
# espa_scenelist(scenes, as.Date('1980/1/1'), as.Date('2015/1/1'), min_clear=.95,
#                file.path(order_folder, 'scenes_allsites_lt5cloud.txt'))
# # Order all scenes with less than 2% cloud cover
# espa_scenelist(scenes, as.Date('1980/1/1'), as.Date('2015/1/1'), min_clear=.98,
#                file.path(order_folder, 'scenes_allsites_lt2cloud.txt'))
#
# scenes_lt20gt10 <- scenes[scenes$Frac_Clear >= .8 & scenes$Frac_Clear <= .9, ]
# Order all scenes with less than 20% cloud cover
# espa_scenelist(scenes_lt20gt10, as.Date('1980/1/1'), as.Date('2015/1/1'), min_clear=0,
#                file.path(order_folder, 'scenes_allsites_lt20cloud.txt'))

# Order all scenes with less than 20% cloud cover (inclusive of 0-10%
scenes_lt20 <- scenes[scenes$Frac_Clear >= .8 & scenes$Frac_Clear <= .9, ]
espa_scenelist(scenes_lt20, as.Date('1980/1/1'), as.Date('2015/1/1'), 
               min_clear=0, file.path(order_folder, 
                                      'scenes_allsites_lt20cloud_noexclusions.txt'))

