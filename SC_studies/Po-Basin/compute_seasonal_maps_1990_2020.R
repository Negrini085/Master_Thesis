# The main goal of this script is to compute seasonal average maps for 1992-2001 
# and 2012-2021 in order to assess whether more or less snow is stored within the 
# snowpack.
rm(list = ls())
gc()

library(terra)

last_years <- 2012:2021
first_years <- 1992:2001
setwd("/home/filippo/Desktop/Codicini/Master_Thesis/SC_studies/Po-Basin/")

total_fnames <- character(0)
for(y in first_years){
  
  # Selecting filenames
  dates_first <-seq(as.Date(paste0(y-1, "-11-01")), as.Date(paste0(y-1, "-12-31")))
  fnames_first <- paste0("Dataset/", y, "/SWE_", dates_first, ".tif")
  
  dates_second <-seq(as.Date(paste0(y, "-01-01")), as.Date(paste0(y, "-05-31")))
  fnames_second <- paste0("Dataset/", y, "/SWE_", dates_second, ".tif")
  total_fnames <- c(total_fnames, fnames_first, fnames_second)
}

first_maps <- rast(total_fnames)
first_mean_map <- mean(first_maps)
writeRaster(
  first_mean_map, "Datas/seasonal_map_1992_2001.tif", datatype = "FLT4S",
  gdal = c("COMPRESS=DEFLATE", "PREDICTOR=3", "TILED=YES"), overwrite = TRUE)


rm(first_maps, first_mean_map); invisible(gc());
total_fnames <- character(0)
for(y in last_years){
  
  # Selecting filenames
  dates_first <-seq(as.Date(paste0(y-1, "-11-01")), as.Date(paste0(y-1, "-12-31")))
  fnames_first <- paste0("Dataset/", y, "/SWE_", dates_first, ".tif")
  
  dates_second <-seq(as.Date(paste0(y, "-01-01")), as.Date(paste0(y, "-05-31")))
  fnames_second <- paste0("Dataset/", y, "/SWE_", dates_second, ".tif")
  total_fnames <- c(total_fnames, fnames_first, fnames_second)
}

last_maps <- rast(total_fnames)
last_mean_map <- mean(last_maps)
writeRaster(
  last_mean_map, "Datas/seasonal_map_2012_2021.tif", datatype = "FLT4S",
  gdal = c("COMPRESS=DEFLATE", "PREDICTOR=3", "TILED=YES"), overwrite = TRUE)