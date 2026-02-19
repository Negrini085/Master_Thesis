# The main goal of this script is to remove fill values and not assigned from a raster
rm(list = ls())
gc()

library(terra)

setwd("/home/filippo/Desktop/Codicini/Master_Thesis/SnowCover_studies/MODIS/Dataset/annual_maps/SOS/")
fname <- "sos_avg.tif"

r <- rast(fname)
min_r <- global(r, "min", na.rm = TRUE)$min

mask_ita <- (r == min_r) | is.na(r)
good_r <- mask(r, mask_ita, maskvalues = 1)

writeRaster(good_r, paste0("sos_avg_filtered.tif"), overwrite=TRUE)