# The main goal of this script is to find SWE NAs which could be a model faulty output.
rm(list = ls())
gc()

setwd("/home/filippo/Desktop/Codicini/Master_Thesis/Calibration/")


# Making swe list
files <- list.files(path = "Results/raw", full.names = TRUE)
files <- sub("Results/raw/", "", files)


# Cycle on station list
for(name in files){
  
  # Importing swe dataset
  fname <- paste0("Results/raw/", name)
  df <- read.table(fname, header = FALSE)
  
  # Looking for NAs
  mask <- is.na(as.numeric(df$V4))
  if(any(mask, na.rm = TRUE)) print(paste0("Found some NAs for ", name))
  
}