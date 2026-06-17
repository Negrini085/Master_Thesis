# The main goal of this script is to align TCD and MODIS raster in order to 
# actually check what is modis seeing.
rm(list = ls())
gc()

library(terra)

setwd("/home/filippo/Desktop/Codicini/Master_Thesis/MODIS_vs_HS/")


# Importing tcd and modis raster
tcd <- rast("DEM/TCD_2012_100m.tif")
map <- rast("../SC_studies/MODIS/Dataset/daily/2003/day_006.tif")


# Looking for nodata flag
tcd_mod <- project(tcd, map, method = "average")
compareGeom(tcd_mod, map)
writeRaster(tcd_mod, "DEM/TCD_MOD.tif", filetype = "GTiff", overwrite = TRUE)