# The main goal of this script is to crop and project a DEM model in order to 
# make fair comparison with SWE metrics.
rm(list = ls())
gc()

library(terra)

fname_dem <- "DEM/DEM_region.tif"
fname_map <- "Results/years_continuous_snow.nc"
setwd("/home/filippo/Desktop/Codicini/Master_Thesis/SWE_analysis/")


# Importing DEM and SWE maps
map <- rast(fname_map)
dem <- rast(fname_dem)


# Projection procedure (using bilinear method)
dem <- project(dem, map, method = "near")
paste0("Map compatibility: ", compareGeom(dem, map))
writeRaster(
  dem, "DEM/DEM_compatible.tif", overwrite = TRUE,
  datatype = "FLT4S", gdal = c("COMPRESS=DEFLATE", "PREDICTOR=2")
)