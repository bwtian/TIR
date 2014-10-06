###############################################################################
# This script calculates total forest loss, in hectares for the Zone of 
# Interaction of each TEAM site, using data from the University of Maryland 
# Global Forest Change dataset.
#
# After running this script, you can run the '2_animate_loss.R' script to 
# produce a series of animations of forest change at each TEAM site.
#
# Contact Alex Zvoleff (azvoleff@conservation.org) for the datafiles needed to 
# produce these animations.
###############################################################################

library(stringr)
library(plyr)
library(reshape2)
library(ggplot2)
library(scales) # for percent format

###############################################################################
# Setup parameters
###############################################################################

forest_threshold <- 75
to_utm <- FALSE
prefixes <- c('D:/azvoleff/Data', # CI-TEAM
              'H:/Data', # Buffalo drive
              'D:/Data/CI_H_Data/TEAM', # Orange drive
              '/home/azvoleff/Data') # vertica1
# Specify output_folder and data_folder relative to above prefixes
output_folder <- 'GFC_TEAM_Analysis'

###############################################################################
# Script begins below
###############################################################################

prefix <- prefixes[match(TRUE, unlist(lapply(prefixes, function(x) file_test('-d', x))))]
if (to_utm) {
    utm_string <- '_utm'
} else {
    utm_string <- '_wgs84'
}

###############################################################################
# Loss

this_output_folder <- file.path(output_folder, paste0(gsub('_', '', utm_string), '_', forest_threshold, 'pct'))
this_output_folder <- file.path(prefix, this_output_folder)

if (!file_test('-d', this_output_folder)) {
    stop(paste(this_output_folder, 'does not exist'))
}

loss_stats_files <- dir(this_output_folder, pattern='_stats_loss.csv')
site_codes <- str_extract(loss_stats_files, '_[A-Z]{2,3}_')
site_codes <- gsub('_', '', site_codes)

loss_dfs <- lapply(file.path(this_output_folder, loss_stats_files), read.csv)
loss <- ldply(loss_dfs, data.frame)
loss$site_code <- rep(site_codes, each=nrow(loss)/length(site_codes))
loss$threshold <- forest_threshold
loss$threshold <- factor(loss$threshold)

# Add some more data fields needed for plotting
loss$year <- as.Date(paste0(loss$year, '-1-1'))
site_codes_key <- read.csv('sitecode_key.csv')
loss$site_name <- site_codes_key$sitename_short[match(loss$site_code, 
                                                      site_codes_key$sitecode)]

# Add cumulative and percent loss fields
loss <- ddply(loss, .(threshold, site_code, aoi), transform,
                   loss_cum=c(NA, cumsum(loss[2:length(loss)])),
                   loss_pct=(loss/cover[1])*100)
loss <- ddply(loss, .(threshold, site_code, aoi), transform,
                   loss_pct_cum=c(NA, cumsum(loss_pct[2:length(loss_pct)])))
save(loss, file="loss.RData")

###############################################################################
# Gain

this_output_folder <- file.path(output_folder, paste0(gsub('_', '', utm_string), '_', forest_threshold, 'pct'))
this_output_folder <- file.path(prefix, this_output_folder)

if (!file_test('-d', this_output_folder)) {
    stop(paste(this_output_folder, 'does not exist'))
}

fgain_stats_files <- dir(this_output_folder, pattern='_stats_gain.csv')
site_codes <- str_extract(fgain_stats_files, '_[A-Z]{2,3}_')
site_codes <- gsub('_', '', site_codes)

these_fgain_dfs <- lapply(file.path(this_output_folder, fgain_stats_files), read.csv)
fgain <- ldply(these_fgain_dfs, data.frame)
fgain$site_code <- rep(site_codes, each=nrow(fgain)/length(site_codes))
fgain$threshold <- forest_threshold
fgain$threshold <- factor(fgain$threshold)

# Calculate total loss per site
summ_stats <- melt(fgain, id.vars=c('site_code', 'aoi', 'threshold'), 
                   measure.vars=c('gain', 'lossgain'))
summ_stats <- ddply(summ_stats, .(threshold, site_code, aoi, variable), summarize,
                    value=sum(value, na.rm=TRUE))
total_loss <- ddply(loss, .(threshold, site_code, aoi), summarize,
                    variable='loss', 
                    value=sum(loss, na.rm=TRUE))
summ_stats <- rbind(summ_stats, total_loss)
summ_stats <- summ_stats[order(summ_stats$threshold, summ_stats$site_code, 
                               summ_stats$variable, summ_stats$aoi), ]
summ_stats$variable <- factor(summ_stats$variable,
                              levels=c('loss', 'lossgain', 'gain'), 
                              labels=c('Loss', 'Loss & gain', 'Gain'))

# Normalize by initial cover, then multiply by 100, to get percentage of 
# initial forest cover
initial_cover <- loss[loss$year == as.Date('2000-1-1'), ]
initial_cover_row <- match(paste(summ_stats$site_code, summ_stats$aoi),
                           paste(initial_cover$site_code, initial_cover$aoi))
summ_stats$value_pct <- with(summ_stats, (value / 
                                          initial_cover$cover[initial_cover_row])*100)

save(summ_stats, file="summ_stats.RData")
