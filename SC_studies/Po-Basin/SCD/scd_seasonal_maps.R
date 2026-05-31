# The main goal of this script is to compute annual SCD maps for the investigated
# region, to then later check for significant trend. To deal with missing files, I
# will assume conditions to be equal to the ones of the day before. I'm doing so
# because there are only a few gaps in the dataset.
rm(list = ls())
gc()

library(terra)
setwd("/home/filippo/Desktop/Codicini/Master_Thesis/SnowCover_studies/Po-Basin")




years <- 1992:2021
months <- c("10", "11", "12", "01", "02", "03", "04", "05", "06", "07")
dayMax <- c(31, 30, 31, 31, 28, 31, 30, 31, 30, 31)


for(y in years){
  
  if(y%%4 == 0){
    dayMax[5] <- 29
  }
  
  season_scd <- numeric(0)
  for(i in 1:length(months)){
    
    # Creating the correct file path
    if(i > 3){
      fname <- paste0("Dataset/", toString(y), "/SWE_", toString(y), "-", months[i], "-")
      print(paste0("Considering month  ", months[i], "/", y, " for SCD evaluation."))
    }
    else{
      fname <- paste0("Dataset/", toString(y), "/SWE_", toString(y-1), "-", months[i], "-")
      print(paste0("Considering month  ", months[i], "/", y-1, " for SCD evaluation."))
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
        scd <- ifel(swe_tif > 0, 1, 0)
        last <- appo
      }
      
      else{
        # Taking care of possible missing files
        swe_tif <- rast(last)
        scd <- ifel(swe_tif > 0, 1, 0)
      }
      
      if(i == 1 & d == 3){
        season_scd <- scd
      }
      else{
        season_scd <- season_scd + scd
      }
      
    }
  }
  
  # Saving seasonal SCD in a raster
  writeRaster(season_scd, paste0("SCD_", y, ".tif"))
  dayMax[5] <- 28
}
