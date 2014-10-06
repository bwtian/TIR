library(stringr)

new_sitecodes <- c('BBS',
                   'KRP',
                   'MAS',
                   'NNN',
                   'YAN',
                   'YAS')
base_dir <- "H:/Data/Landsat/LCLUC_Training"
base_qgs_file <- file.path(base_dir, "BCI_Landsat_Training_Map.qgs")
base_shp_files <- dir(base_dir, pattern="BCI_Landsat_Training_Data")

for (new_sitecode in new_sitecodes) {
    new_shp_files <- gsub('BCI', new_sitecode, base_shp_files)
    new_qgs_file <- gsub('BCI', new_sitecode, base_qgs_file)

    stopifnot(all(!file_test('-f', c(new_shp_files, new_qgs_file))))

    file.copy(file.path(base_dir, base_shp_files),
              file.path(base_dir, new_shp_files))

    f_in <- file(base_qgs_file, 'r')
    base_qgs_file_text <- readLines(f_in)
    close(f_in)

    new_qgs_file_text <- gsub('BCI', new_sitecode, base_qgs_file_text)

    f_out <- file(new_qgs_file, 'w')
    writeLines(new_qgs_file_text, f_out)
    close(f_out)
}
