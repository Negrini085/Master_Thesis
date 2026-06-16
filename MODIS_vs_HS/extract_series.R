# The main goal of this script is to extract snow cover values for every italian 
# station of our dataset. In particular, I would like to do it simultaneously in
# order to open as few files as possible
rm(list = ls())
gc()

library(terra)

fname_ana <- "Dataset/ANAGRAFICA_ITA"
setwd("/home/filippo/Desktop/Codicini/Master_Thesis/MODIS_vs_HS/")


# Importing station names and coordinates
df <- read.table(fname_ana, header = TRUE)
station_names <- df$name
coord_sta <- vect(cbind(df$lon, df$lat), crs = "+proj=longlat +datum=WGS84")


# Cycle over maps
year_list <- list()
years <- 2000:2023
for(y in years){

  # How many maps I expect to have?
  len <- 366
  if(y == 2000) len <- 312
  if(y %% 4 != 0) len <- 365
  
  
  # Getting ready to extract files
  repo_year <- paste0("../SC_studies/MODIS/Dataset/daily/", y)
  year_names <- list.files(repo_year, pattern = "\\.tif$", full.names = TRUE)

  
  # Looking for some missing days in MODIS dataset
  ndays <- length(year_names)
  if(ndays != len) stop(paste0("Not enough maps for a given year of our dataset, namely ", y, "!"))
  print(paste0("Starting data extraction for ", y, "!"))
  
  
  # Opening maps
  year_stack <- rast(year_names)
  if(crs(coord_sta) != crs(year_stack)) coord_sta <- project(coord_sta, crs(year_stack))
  
  # SC extraction and save
  appo <- extract(year_stack, coord_sta, ID = FALSE)
  year_list[[as.character(y)]] <- appo
}


# Final matrix
sc_values <- do.call(cbind, year_list)
rownames(sc_values) <- station_names
sc_values <- t(sc_values)
print("Extraction completed successfully!")


# Saving SC matrix
df_print <- as.data.frame(sc_values)
write.table(df_print, paste0("Dataset/sc_matrix.dat"), row.names = FALSE, col.names = TRUE, quote = FALSE)