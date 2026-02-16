# The main goal of this script is to aggregate pixels of a digitalized elevation
# model in order to check if the number of faulty stations decreases with lower
# resolution or not. I feel that this is kind of important, because if precipitation
# and temperature field have a resolution of about 200 meters, we maybe would like
# to check station reliability on a similar-sized DEM and not on asuper precise one.
rm(list = ls())
gc()

library(terra)

dem30 <- rast("DEM/DEM_stations_30.tif")
dem60 <- aggregate(dem30, fact = 2, fun = mean)
writeRaster(dem60, "DEM/DEM_stations_60.tif", overwrite = TRUE)
