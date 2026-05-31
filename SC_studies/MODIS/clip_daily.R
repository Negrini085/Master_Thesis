# The main goal of this script is to clip daily maps to annual ones in order to 
# finally lose all those faulty 2 pixels.
rm(list = ls())
gc()

library(terra)

years <- 2003:2025
setwd("/home/filippo/Desktop/Codicini/Master_Thesis/SnowCover_studies/MODIS/")


# Importing annual los map in order to have something to clip maps on
annual_map <- rast("Dataset/annual_maps/LOS/los_2020.tif")
mask_twos <- is.na(annual_map)


# Starting to cycle over years in order to clip all daily maps
for(y in years){
  
  # Evaluating number of files in a given repository
  nfiles <- length(list.files(paste0("Dataset/daily/", y), pattern = "\\.tif$"))
  for(d in seq_len(nfiles)){
    
    # Opening daily map
    fname <- paste0("Dataset/daily/", y, "/day_",  sprintf("%03d", d), ".tif")
    daily_map <- rast(fname)
    
    # Masking using annual los map
    appo <- mask(daily_map, mask_twos, maskvalues = 1)
    
    # Saving map
    fname <-paste0("Dataset/daily_clipped/", y, "/day_",  sprintf("%03d", d), ".tif")
    writeRaster(appo, fname, overwrite = TRUE)
    print(paste0("Taken care of day ", d, " of ", y))
    
    # Calling garbage cleaner
    rm(daily_map, appo)
    gc()
  }
}