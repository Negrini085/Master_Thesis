# This script enables the user to check whether there are still some gaps (hopefully no).
rm(list = ls())
gc()

setwd("/home/filippo/Desktop/Codicini/Master_Thesis/HS_series/Correct/")
files <- list.files(path = "../../HS_3_6_2026/Dataset", full.names = TRUE)
files <- sub("../../HS_3_6_2026/Dataset/", "", files)



# Cycle across stations
for(name in files){
  
  # Importing hs series
  fname <- paste0("Dataset/", name)
  df <- read.table(fname)
  
  # Finding out if some NAs are present
  hs_series <- as.numeric(df$V2)
  if(any(is.na(hs_series))) print(paste0("Some NAs for: ", name, " in ", which(is.na(hs_series)), " during ", df$V1[which(is.na(hs_series))]))
}