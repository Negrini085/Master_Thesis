# The main goal of this script is to check for difference in italian territory
# coverage between modis mean map and mine. Those differences could be linked to 
# the chosen threshold for pixel mean computation.
rm(list = ls())
gc()

library(terra)

setwd("/home/filippo/Desktop/Codicini/Master_Thesis/SnowCover_studies/MODIS")
fname_mod <- "Dataset/annual_maps/SOS/sos_avg_filtered.tif"
fname_me <- "Datas/mean_maps/mean_sos.tif"


# Importing raster files & testing if the geo-ref is the same for both files
mod_mean <- rast(fname_mod)
me_mean <- rast(fname_me)

if(compareGeom(me_mean, mod_mean)){
  print("Raster have same georef!")
}


# Checking for NAs in modis map and actual datas in mine
mask_val <- is.na(mod_mean) & !is.na(me_mean)
n_pix <- global(mask_val, "sum", na.rm = TRUE)$sum

print(paste0("Number of pixel which are NA in modis and actual datas in mine is ", n_pix))


# Checking for actual datas in modis map and NAs in mine
mask_val <- !is.na(mod_mean) & is.na(me_mean)
n_pix <- global(mask_val, "sum", na.rm = TRUE)$sum

print(paste0("Number of pixel which are actual datas in modis and NAs in mine is ", n_pix))                                   