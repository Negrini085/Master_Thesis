# The main goal of this script is to compare MODIS series which where extracted 
# with different methods, so that I'm sure about what I saw on LOS dispersion graphs.
rm(list = ls())
gc()

setwd("/home/filippo/Desktop/Codicini/Master_Thesis/MODIS_vs_HS/")
files <- list.files(path = "../HS_series/Correct/MODIS_series/Dataset/modis_hydrological", full.names = TRUE)
names <- sub("../HS_series/Correct/MODIS_series/Dataset/modis_hydrological/", "", files)


# Cycle over station names
for(name in names){
  
  # Importing both files (in order to compare methods)
  fname_one <- paste0("Dataset/sc_hydro/", name)
  df_one <- read.table(fname_one, header = FALSE)
  
  fname_two <- paste0("../HS_series/Correct/MODIS_series/Dataset/modis_hydrological/", name)
  df_two <- read.table(fname_two, header = FALSE)
  
  
  # Selecting only the needed part for every dataframe
  mask <- as.numeric(df_two$V1) < 2024
  df_two <- df_two[mask, ]
  
  mask <- as.numeric(df_one$V1) %in% unique(as.numeric(df_two$V1))
  df_one <- df_one[mask, ]
  
  if(nrow(df_one) != nrow(df_two)) stop(paste0("Mismatch in data-frame lenght for ", name, "! ", nrow(df_one), " & ", nrow(df_two)))
  
  
  # Looking for series match
  mask <- as.numeric(df_one$V2) == as.numeric(df_two$V2)
  if(any(!mask)) stop(paste0("SC series mismatch for ", name))
}