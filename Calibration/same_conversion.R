# The main goal of this script is to check whether I made some errors while converting
# from solar year to hydrological format.
rm(list = ls())
gc()

setwd("/home/filippo/Desktop/Codicini/Master_Thesis/Calibration/")


files <- list.files(path = "Results/after", full.names = TRUE)
station_names <- sub("Results/after/", "", files)

for(name in station_names){
  
  # Importing both data-frame
  fname_before <- paste0("Results/before/", name)
  df_before <- read.table(fname_before, header = FALSE)
  
  fname_after <- paste0("Results/after/", name)
  df_after <- read.table(fname_after, header = FALSE)
  
  
  # Check
  mask <- as.numeric(df_before$V1) == as.numeric(df_after$V1)
  if(!all(mask)) stop(paste0("Year problems at ", name))
  
  mask <- as.numeric(df_before$V2) == as.numeric(df_after$V2)
  if(!all(mask)) stop(paste0("SWE problems at ", name))
}