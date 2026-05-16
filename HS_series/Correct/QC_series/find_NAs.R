# This script enables the user to check whether there are still some gaps (hopefully no).
rm(list = ls())
gc()

setwd("/home/filippo/Desktop/Codicini/Master_Thesis/HS_series/Correct/")
files <- list.files(path = "Dataset", full.names = TRUE)
files <- sub("Dataset/", "", files)



# Cycle across stations
for(name in files){
  
  # Importing hs series
  fname <- paste0("Dataset/", name)
  df <- read.table(fname)
  
  # Finding out if some NAs are present
  hs_series <- as.numeric(df$V2)
  if(any(is.na(hs_series))) print(paste0("Some NAs for: ", name))
}