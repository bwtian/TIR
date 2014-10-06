library(raster)

sitecode <- 'BCI'
base_dir <- file.path(prefix, 'Landsat', sitecode)
image_files <- dir(base_dir, pattern='^[a-zA-Z]*_mosaic_[0-9]{4}.tif$', full.names=TRUE)
mask_files <- dir(base_dir, pattern='^[a-zA-Z]*_mosaic_[0-9]{4}_masks.tif$', full.names=TRUE)

for (n in 1:length(image_files)) {
    pdf(extension(image_files[n], '.pdf'))
    plot(stack(image_files[n]))
    dev.off()

    pdf(extension(mask_files[n], '.pdf'))
    plot(stack(mask_files[n]))
    dev.off()
}
