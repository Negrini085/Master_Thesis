# The main goal of this script is to check if the digitalized elevation model covers
# all stations. If that's not the case I will have to download a broader region
# that covers all data points.
rm(list = ls())
gc()

library(terra)

setwd("/home/filippo/Desktop/Codicini/Master_Thesis/Degree_Day_factor")



# Evaluating stations longitude and latitude ranges
stations <- as.matrix(read.table("Dataset/coord.dat"))
print(paste0("Minimum station longitude: ", min(stations[, 2])))
print(paste0("Maximum station longitude: ", max(stations[, 2])))
print(paste0("Minimum station latitude: ", min(stations[, 3])))
print(paste0("Maximum station latitude: ", max(stations[, 3])))



# Evaluating dem longitude and latitude ranges
dem <- rast("appo.tif")
coord_range <- ext(dem)
print(coord_range)