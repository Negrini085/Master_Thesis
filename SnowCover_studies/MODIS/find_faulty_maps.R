# The main goal of this script is to find faulty daily maps, which we intend as
# days with only zero, two or missing data, because I feel the main problem in CSC
# comparison is the presence of some daily faulty maps.
rm(list = ls())
gc()

library(terra)

setwd("/home/filippo/Desktop/Codicini/Master_Thesis/SnowCover_studies/MODIS/")


# Cycle across the whole dataset
faulty_maps <- c()
years <- 2000:2025
for(year in years){
  
  # Counting how many daily maps we have for a given year
  fname <- paste0("Dataset/daily/", year)
  nmaps <- length(list.files(path = fname, pattern = "\\.tif$"))
  print(paste0("Starting faulty maps check for ", year, "!"))
  
  # Selecting daily maps
  for(i in 1:nmaps){
    appo_name <- paste0(fname, "/day_", sprintf("%03d", i), ".tif")
    daily_map <- rast(appo_name)
    
    # Checking if map is faulty or not
    vals <- values(daily_map)
    is_faulty <- all(vals != 1, na.rm = TRUE)
    
    if(is_faulty){
      cat("FAULTY ->", appo_name, "\n")
      faulty_maps <- c(faulty_maps, appo_name)
    }
  }
}

cat("\nTotale mappe faulty:", length(faulty_maps), "\n")