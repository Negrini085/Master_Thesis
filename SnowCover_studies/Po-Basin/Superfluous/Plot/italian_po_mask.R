# The main goal of this script is to create a mask of the italian part of the 
# Po basin to be used in IT-Snow, so that the comparison is feasible
rm(list = ls())
gc()

library(terra)

setwd("/home/filippo/Desktop/Codicini/Master_Thesis/SnowCover_studies/Po-Basin")

# Importing italian mask and po catchment region to be masked
mask_italy <- rast("Dataset/Italy_Mask.tif")
basin <- rast("Dataset/1992/SWE_1991-10-03.tif")

# Masking
maskP <- !is.na(mask_italy)
basin <- ifel(!is.na(basin), 1, NA)
final_mask <- mask(basin, maskP, maskvalues = FALSE)

# Saving raster
writeRaster(final_mask, filename = "Dataset/Italian_Po_Mask.tif", overwrite = TRUE)