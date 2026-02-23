# The main goal of this script is to create a DEM with 200 meter resolution in 
# order to filter our station based on elevation accuracy
rm(list = ls())
gc()

library(terra)
setwd("/home/filippo/Desktop/Codicini/Master_Thesis/Degree_Day_factor")


# Importing original digital elevation model
dem30 <- rast("DEM/DEM_stations_30.tif")


# Creating a template raster having the correct angular resolution (needed in degrees)
res <- 7.5/3600
dem200 <- rast(ext(dem30), res = res, crs = crs(dem30))


# Resampling using bilinear (to obtain a continuous and smooth elevation profile)
dem200 <- resample(dem30, dem200, method = "bilinear")
writeRaster(dem200, "DEM/DEM_stations_200.tif")