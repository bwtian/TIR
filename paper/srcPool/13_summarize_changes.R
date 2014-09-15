source('0_settings.R')

library(foreach)
library(itertools)

library(ggplot2)
library(gridExtra) # for unit
library(dplyr)
library(reshape2)
library(stringr)

library(scales) # for percent format

img_width <- 10
img_height <- 7.5
img_dpi <- 300

load("zoi_n_pix_change.RData")

traj_freqs_dir <- file.path(prefix, 'Landsat', 'Composites', 'Change_Detection')

traj_freqs_files <- dir(traj_freqs_dir,
                        pattern='^[a-zA-Z]*_[0-9]{4}-[0-9]{4}_chgdetect_chgtraj_freqs.csv$')
traj_freqs <- foreach(traj_freqs_file=iter(traj_freqs_files),
                 .packages=c('ggplot2'),
                 .combine=rbind, .inorder=FALSE) %do% {
    sitecode <- str_extract(basename(traj_freqs_file), '^[a-zA-Z]*')
    time_string <- str_extract(traj_freqs_file, '[0-9]{4}-[0-9]{4}')
    t0 <- as.numeric(gsub('-', '', str_extract(time_string, '[0-9]{4}-')))
    t1 <- as.numeric(gsub('-', '', str_extract(time_string, '-[0-9]{4}')))
    traj_freqs <- read.csv(file.path(traj_freqs_dir, traj_freqs_file))
    traj_freqs$t0_name <- ordered(traj_freqs$t0_name, levels=class_names_R, 
                                  labels=class_names_pretty)
    traj_freqs$t1_name <- ordered(traj_freqs$t1_name, levels=class_names_R, 
                                  labels=class_names_pretty)
    traj_freqs$t0_name_abbrev <- ordered(class_names_abbrev[match(traj_freqs$t0_name, 
                                                             class_names_pretty)], 
                                         levels=class_names_abbrev)
    traj_freqs$t1_name_abbrev <- ordered(class_names_abbrev[match(traj_freqs$t1_name, 
                                                             class_names_pretty)], 
                                         levels=class_names_abbrev)
    traj_freqs$Transition <- paste(traj_freqs$t0_name_abbrev, 
                                   traj_freqs$t1_name_abbrev, sep=' -> ')
    traj_freqs <- cbind(sitecode=sitecode, t0=t0, t1=t1, nyears=t1-t0, 
                        traj_freqs)
    return(traj_freqs)
}

traj_freqs <- tbl_df(traj_freqs)
traj_freqs$period <- paste(traj_freqs$t0, traj_freqs$t1, sep=" -> ")

traj_freqs_site_period <- paste(traj_freqs$sitecode, traj_freqs$period)
valid_zoi_pix <- zoi_pix[zoi_pix$pixtype == "Valid", ]
valid_zoi_pix_site_period <- paste(valid_zoi_pix$sitecode, valid_zoi_pix$period)
traj_freqs$freq_as_frac <- traj_freqs$freq/valid_zoi_pix$n[match(traj_freqs_site_period, valid_zoi_pix_site_period)]

write.csv(traj_freqs, file="traj_freqs.csv", row.names=FALSE)
save(traj_freqs, file="traj_freqs.RData")

preds_basedir <- file.path(prefix, 'Landsat', 'Composites', 'Predictions')
class_freqs_files <- dir(preds_basedir,
                         pattern='^[a-zA-Z]*_mosaic_[0-9]{4}_predictors_predclasses_classfreqs.csv$')
class_freqs <- foreach(class_freqs_file=iter(class_freqs_files),
                       .packages=c('ggplot2'),
                       .combine=rbind, .inorder=FALSE) %do% {
    class_freqs <- read.csv(file.path(preds_basedir, class_freqs_file), stringsAsFactors=FALSE)
    class_freqs$name[is.na(class_freqs$code)] <- 'Unknown'
    class_freqs$code[class_freqs$name == 'Unknown'] <- '-1'
    # Ignore areas outside ZOI (areas coded 99)
    class_freqs <- class_freqs[!(class_freqs$code == 99), ]
    class_freqs$name <- ordered(class_freqs$name, levels=class_names_R, 
                                labels=class_names_pretty)
    class_freqs$name_abbrev <- ordered(class_names_abbrev[match(class_freqs$name, 
                                                             class_names_pretty)], 
                                       levels=class_names_abbrev)

    return(class_freqs)
}

write.csv(class_freqs, file="class_freqs.csv", row.names=FALSE)
save(class_freqs, file="class_freqs.RData")

# TODO: integrate number of cells in ZOI per image into calculation so that 
# results are normalized.

for (sitecode in unique(traj_freqs$sitecode)) {
    site_traj_freqs <- traj_freqs[traj_freqs$sitecode == sitecode, ]
    # persist <- summarize(group_by(site_traj_freqs, t0, t0_name),
    #                      pct=paste0(round(freq[t0_name == t1_name] / sum(freq), 2)),
    #                      t0_name_abbrev=t0_name_abbrev[1])
    # freqs_zeroed_persist <- site_traj_freqs
    # freqs_zeroed_persist$freq[freqs_zeroed_persist$t0_name == freqs_zeroed_persist$t1_name] <- 0

    ggplot(site_traj_freqs) +
        theme_bw() +
        geom_tile(aes(x=t1_name_abbrev, y=t0_name_abbrev, fill=freq/sum(freq)), colour='black') +
        #geom_text(aes(x=t0_name_abbrev, y=t0_name_abbrev, label=pct), 
        #data=persist) +
        scale_fill_gradientn('Relative\nFrequency', limits=c(0, .25),
                             colours=c('white', 'orange', 'red')) +
        xlab('Class in time 1') + ylab('Class in time 0') +
        theme(axis.text.y=element_text(angle=90, hjust=.5),
              legend.key.size=unit(1.5, "line"),
              panel.grid.major=element_blank()) +
        facet_wrap(~ t0)
    ggsave(file.path(traj_freqs_dir, paste('transitions', sitecode, 
                                           'colorplot.png', sep='_')),
           height=img_height, width=img_width, dpi=img_dpi)
}

classes <- data.frame(label=class_names_pretty,
                      color=class_colors,
                      stringsAsFactors=FALSE)

# Plot trajectory frequencies by site
ggplot(traj_freqs) +
    geom_bar(aes(t0, freq_as_frac, fill=t0_name), stat="identity", position="dodge") +
    facet_wrap(~sitecode) + 
    scale_fill_manual("Time 0 Cover", values=classes$color, breaks=classes$label,
                      labels=classes$label, drop=FALSE) +
    xlab("Start of period") +
    ylab("Fraction of all pixels") +
    ggtitle("Time 0")
ggsave(file.path(traj_freqs_dir, 
                 'transition_frequencies_all_sites_normalized_time0.png'),
       height=img_height, width=img_width, dpi=img_dpi)

# Plot trajectory frequencies by site
ggplot(traj_freqs) +
    geom_bar(aes(t1, freq_as_frac, fill=t1_name), stat="identity", position="dodge") +
    facet_wrap(~sitecode) + 
    scale_fill_manual("Time 1 Cover", values=classes$color, breaks=classes$label,
                      labels=classes$label, drop=FALSE) +
    xlab("End of period") +
    ylab("Fraction of all pixels") +
    ggtitle("Time 1")
ggsave(file.path(traj_freqs_dir, 
                 'transition_frequencies_all_sites_normalized_time1.png'),
       height=img_height, width=img_width, dpi=img_dpi)

# Plot percentage of pixels changing over time
class_freqs <- group_by(class_freqs, sitecode, year)
class_freqs <- mutate(class_freqs, pct=freq/sum(freq[!is.na(name)]))
ggplot(class_freqs) +
    geom_line(aes(year, pct, colour=name, linetype=name)) +
    geom_point(aes(year, pct, colour=name, shape=name)) + 
    scale_colour_manual("Site", values=rep(1:4, length.out=16)) +
    scale_shape_manual("Site", values=rep(1:4, length.out=16)) +
    scale_linetype_manual("Site", values=rep(1:4, each=4, length.out=16)) +
    theme_grey(base_size=18) +
    facet_wrap(~ sitecode) +
    labs(linetype="Class", colour="Class", shape="Class") +
    xlab('Year') + ylab('Percent of Landscape') +
    ylim(c(0, 100)) +
    theme(axis.text.y=element_text(angle=90, hjust=.5),
          legend.key.size=unit(1.5, "line"),
          panel.grid.major=element_blank()) +
    scale_y_continuous(labels=percent_format())
ggsave(file.path(preds_basedir, 'class_frequencies_all_sites.png'),
       height=img_height, width=img_width, dpi=img_dpi)
