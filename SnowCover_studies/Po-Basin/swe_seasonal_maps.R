# The main goal of this script is to compute annual SWE maps for the investigated
# region, to then later look for significant trend. To deal with missing files, I
# will assume conditions to be equal to the ones of the day before. I'm doing so
# because there are only a few gaps in the dataset.
rm(list = ls())
gc()

library(terra)
setwd("/home/filippo/Desktop/Codicini/Master_Thesis/SnowCover_studies/Po-Basin")



dur <- 272
years <- 2003
months <- c("10", "11", "12", "01", "02", "03", "04", "05", "06", "07")
dayMax <- c(31, 30, 31, 31, 28, 31, 30, 31, 30, 31)


for(y in years){
  
  if(y%%4 == 0){
    dur <- 273
    dayMax[5] <- 29
  }
  
  season_swe <- numeric(0)
  for(i in 1:length(months)){
    
    # Creating the correct file path
    if(i > 3){
      fname <- paste0("Dataset/", toString(y), "/SWE_", toString(y), "-", months[i], "-")
      print(paste0("Considering month  ", months[i], "/", y, " for SWE evaluation."))
    }
    else{
      fname <- paste0("Dataset/", toString(y), "/SWE_", toString(y-1), "-", months[i], "-")
      print(paste0("Considering month  ", months[i], "/", y-1, " for SWE evaluation."))
    }
    
    # Considering single day swe maps
    for(d in 1:dayMax[i]){
      if(d < 3 & i == 1){
        next
      }
      else if(d > 1 & i == 10){
        break
      }
      
      # Final name
      appo <- paste0(fname, sprintf("%02d", d), ".tif")
      
      if(file.exists(appo)){
        # Snow coverage evaluation
        swe_tif <- rast(appo)
        last <- appo
      }
      
      else{
        # Taking care of possible missing files
        swe_tif <- rast(last)
      }
      
      if(i == 1 & d == 3){
        season_swe <- swe_tif
      }
      else{
        season_swe <- season_swe + swe_tif
      }
      
    }
  }
  
  # Saving seasonal SCD in a raster
  season_swe <- season_swe/dur
  names(season_swe)[1] <- "season map"
  writeRaster(season_swe, paste0("Datas/seasonal_swe_maps/swe_map_", y, ".tif"), overwrite = TRUE)
  dayMax[5] <- 28
  dur <- 272
}
