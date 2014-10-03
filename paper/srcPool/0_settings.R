###############################################################################
# Load packages (do not modify these lines)
library(teamlucc)
library(notifyR)

source('notify.R')

###############################################################################
# General settings (update as necessary)

sites <- read.csv('Site_Code_Key.csv')
sitecodes <- sites$Site.Name.Code
sitecodes <- c("BIF", "CAX", "COU", "CSN",
               "MAS", "PSH", "RNF", "VB",
               "YAN", "YAS", "BCI", "BBS",
               "UDZ", "NAK")

PLOT_WIDTH <- 6.5
PLOT_HEIGHT <- 6.5
PLOT_DPI <- 300

prefixes <- c('D:/azvoleff/Data', # CI-TEAM
              'H:/Data', # Buffalo drive
              'O:/Data', # Blue drive
              '/localdisk/home/azvoleff/Data') # vertica1
prefix <- prefixes[match(TRUE, unlist(lapply(prefixes, function(x) file_test('-d', x))))]

temps <- c('H:/Temp', # Buffalo drive
           'O:/Temp', # Blue drive (HP or thinkpad)
           '/localdisk/home/azvoleff/Temp', # vertica1
           'D:/Temp') # CI-TEAM
temp <- temps[match(TRUE, unlist(lapply(temps, function(x) file_test('-d', x))))]
rasterOptions(tmpdir=temp)

# Specify how many processors to use for parallel processing. On CI-TEAM, this 
# should be set to 6. On your laptop, set it somewhere between 2 and 4.
if (Sys.info()[4] == 'CI-TEAM') {
    n_cpus <- 8
} else if (Sys.info()[4] == 'vertica1.team.sdsc.edu') {
    n_cpus <- 16
} else {
    n_cpus <- 3
}

# Should any existing output files be overwritten as the script runs? If set to 
# FALSE, and there ARE existing files for earlier runs of the script, the 
# script will raise an error and stop running.
overwrite <- TRUE

espa_email <- 'azvoleff@gmail.com'

crop_to_aoi <- TRUE
verbose <- TRUE

# Load the DEM extents needed for the auto_setup_dem function
load('dem_extents.RData')
dem_path <- file.path(prefix, 'CGIAR_SRTM', 'Tiles')
dem_extents$filename <- gsub('H:\\\\Data\\\\CGIAR_SRTM', dem_path, dem_extents$filename)
dem_extents$filename <- gsub('\\\\', '/', dem_extents$filename)

###############################################################################
# Function to check if there are preprocessed files in an image folder
is_preprocessed <- function(image_dir) {
    pathrow_re <- '[0-9]{3}-[0-9]{3}'
    date_re <- '[0-9]{4}-[0-9]{3}'
    sensor_re <- '((L[45]T)|(L[78]E))SR(_tc)?.tif$'
    preprocessed_files <- dir(image_dir,
                              pattern=paste(sitecode, pathrow_re, date_re, 
                                            sensor_re, sep='_'))
    if (length(preprocessed_files) >= 1) {
        return(TRUE)
    } else {
        return(FALSE)
    }
}

class_names_pretty <- c('Urban/built',
                        'Agriculture',
                        'Plantation forest',
                        'Natural forest',
                        'Other vegetation',
                        'Bare',
                        'Water',
                        'Unknown')

class_names_R <- c('Urban.built',
                   'Agriculture',
                   'Plantation.forest',
                   'Natural.forest',
                   'Other.vegetation',
                   'Bare',
                   'Water',
                   'Unknown')

class_names_abbrev <- c('Urban',
                        'Ag',
                        'PlanFor',
                        'NatFor',
                        'OthVeg',
                        'Bare',
                        'Water',
                        'Unk')

class_colors <- c('#CC0000',
                  '#F3F781',
                  '#3366FF',
                  '#088A08',
                  '#82FA58',
                  '#DBA901',
                  '#58D3F7',
                  '#A4A4A4')

