# The main goal of this script is to create a mean map of a specific snow metric
# To do so, we just have to be really careful about dealing with NA values
rm(list = ls())
gc()

library(terra)

years <- 2001:2025
setwd("/home/filippo/Desktop/Codicini/Master_Thesis/SnowCover_studies/MODIS")


# Importing raster files
files <- paste0("Dataset/annual_maps/SOS/sos_", years, ".tif")
r <- rast(files)


# Checking if some pixels are not assigned for more than 10 years on a 25 year span
valid <- app(r, function(x) sum(!is.na(x)))
mask_faulty <- valid < 15 & valid != 0

n_faulty <- global(mask_faulty, "sum", na.rm = TRUE)$sum
print(paste0("There are ", n_faulty, " with at least one value and at most 15."))

rm(mask_faulty)
gc()


# Taking mean values only considering those pixel which have at least 15 datas on
# a 25 year span. We just have to mask those pixels not reaching our threshold.
mask_NAs <- valid < 15
mean_r <- mean(r, na.rm = TRUE)

correct_r <- mask(mean_r, mask_NAs, maskvalues = 1)
writeRaster(correct_r, "Datas/mean_maps/mean_sos.tif", overwrite=TRUE)