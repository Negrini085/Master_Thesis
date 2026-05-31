# The main goal of this script is to create a mean SCD maps.
rm(list = ls())
gc()

library(terra)
setwd("/home/filippo/Desktop/Codicini/Master_Thesis/SnowCover_studies/Po-Basin")


# Opening seasonal swe maps
years <- 1992:2021
fnames <- paste0("Datas/seasonal_swe_maps/swe_map_", years, ".tif")
stack_swe <- rast(fnames)


# Computing standard deviation map
std_swe <- app(stack_swe, sd, na.rm = TRUE)
writeRaster(std_swe, "Datas/swe_std_map.tif", overwrite = TRUE)