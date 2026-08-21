# The main goal of this script is to project the DEM onto MOIS grid, in ordet to 
# do elevation-based analysis
rm(list = ls())
gc()

library(terra)

fname_map <- "Datas/mean_maps/mean_los.tif"
fname_dem <- "../IT-Snow/DEM/DEM_region.tif"
setwd("/home/filippo/Desktop/Codicini/Master_Thesis/SC_studies/MODIS/")


# Importing MODIS map and DEM
map <- rast(fname_map) 
dem <- rast(fname_dem)


# Projecting dem on map crs
dem_proj <- project(dem, map, method = "bilinear")


# Save projected DEM
fname_dem_proj <- "DEM/MODIS_dem.tif"
writeRaster(dem_proj, fname_dem_proj,overwrite = TRUE)