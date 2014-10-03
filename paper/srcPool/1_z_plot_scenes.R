library(teamlucc)
library(ggplot2)

load('1_all_scenes.RData')

plot_folder <- 'Scene_plots'

###############################################################################
# Plot available scenes

for (sitecode in unique(sitecodes)) {
    message(sitecode)
    these_scenes <- scenes[scenes$sitecode == sitecode, ]
    these_scenes$Path_Row <- factor(these_scenes$Path_Row)
    # Make plots to assist in selecting Landsat images
    endpts <- seq(as.Date('1980/1/1'), as.Date('2020/1/1'), '10 years')
    for (i in 1:(length(endpts) - 1)) {
        start_date <- endpts[i]
        end_date <- endpts[i + 1]
        plot_title <- paste0(sitecode, ' (', start_date, ' - ', end_date, ')')
        ee_plot(these_scenes, start_date, end_date, title=plot_title)
        ggsave(file.path(plot_folder, paste0(sitecode, '_scenes_', start_date, 
                                             '--', end_date, '_bar.png')), 
                         width=PLOT_WIDTH, height=PLOT_HEIGHT, 
               dpi=PLOT_DPI)
        ee_plot(these_scenes, start_date, end_date, normalize=TRUE, title=plot_title)
        ggsave(file.path(plot_folder, paste0(sitecode, '_scenes_', start_date, 
                                             '--', end_date, '_line.png')), 
                         width=PLOT_WIDTH, height=PLOT_HEIGHT, 
               dpi=PLOT_DPI)
    }
}
