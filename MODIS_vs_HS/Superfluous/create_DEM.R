# The main goal of this script is to create a DEM which has the same cells as 
# MODIS maps (same resolution, same grid points etc) starting form a DEM whose 
# cells are 30 x 30 meters.
rm(list = ls())
gc()

library(terra)

fname_DEM <- "../HS_series/Correct/DEM/DEM_stations_30.tif"
fname_MOD <- "../SC_studies/MODIS/Dataset/daily/2004/day_005.tif"
setwd("/home/filippo/Desktop/Codicini/Master_Thesis/MODIS_vs_HS/")


# Importing both DEM and MODIS map
dem <- rast(fname_DEM)
map <- rast(fname_MOD)

dem_modis <- project(dem, map, method = "average")
writeRaster(dem_modis, "DEM_MODIS.tif", overwrite = TRUE)