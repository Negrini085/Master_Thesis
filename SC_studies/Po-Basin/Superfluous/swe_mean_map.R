# The main goal of this script is to create a mean SCD maps.
rm(list = ls())
gc()

library(terra)
setwd("/home/filippo/Desktop/Codicini/Master_Thesis/SnowCover_studies/Po-Basin")


years <- 1992:2021
mean_swe <- numeric(0)
for(y in years){
  fname <- paste0("Datas/seasonal_swe_maps/swe_map_", y, ".tif")
  if(file.exists(fname)){
    
    # Collecting snow cover days for the whole investigated period
    if(y == 1992) mean_swe <- rast(fname)
    else mean_swe <- mean_swe + rast(fname)
  }
}

# Actually evaluating mean map
mean_swe <- mean_swe/length(years)
writeRaster(mean_swe, "Datas/swe_mean_map.tif", overwrite = TRUE)