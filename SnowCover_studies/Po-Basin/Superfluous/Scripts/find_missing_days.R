# The main goal of this script is to find missing days, in order to asses how to 
# deal with those exceptions.
rm(list = ls())
gc()

library(terra)
setwd("/home/filippo/Desktop/Codicini/Master_Thesis/SnowCover_studies/Po-Basin")


# Variables needed to create correct dates
years <- 1992:2021
months <- c("10", "11", "12", "01", "02", "03", "04", "05", "06", "07")
dayMax <- c(31, 30, 31, 31, 28, 31, 30, 31, 30, 31)


# Trying to access to every file in the dataset (will it be there or not?)
for(y in years){

  if(y%%4 == 0){
    dayMax[5] <- 29
  }
  
  for(i in 1:length(months)){
    
    # Creating the correct file path
    if(i > 3){
      fname <- paste0("Dataset/", toString(y), "/SWE_", toString(y), "-", months[i], "-")
    }
    else{
      fname <- paste0("Dataset/", toString(y), "/SWE_", toString(y-1), "-", months[i], "-")
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
      
      if(!file.exists(appo)){
       print(paste0("Missing daily file: ", appo))
      }
    }
  }
  
  dayMax[5] <- 28
}