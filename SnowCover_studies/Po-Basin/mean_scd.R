# The main goal of this script is to create a mean SCD maps.
rm(list = ls())
gc()

library(terra)
setwd("/home/filippo/Desktop/Codicini/Master_Thesis/SnowCover_studies/Po-Basin")


years <- 1992:2021
mean_scd <- numeric(0)
for(y in years){
  fname <- paste0("Datas/Seasonal_SCD/SCD_", y, ".tif")
  if(file.exists(fname)){
    
    # Collecting snow cover days for the whole investigated period
    if(y == 1992) mean_scd <- rast(fname)
    else mean_scd <- mean_scd + rast(fname)
  }
}

# Actually evaluating mean map
mean_scd <- mean_scd/length(years)
writeRaster(mean_scd, "Datas/mean_SCD.tif", overwrite = TRUE)