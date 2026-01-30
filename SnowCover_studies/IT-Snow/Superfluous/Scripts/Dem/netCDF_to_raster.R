# The main goal of this script is to convert from netCDF format to raster. We need to
# do so to be able to open this file with QGIS and to use this SWE map in order 
# to create the best DEM
rm(list = ls())
gc()

library(ncdf4)
library(terra)

setwd("/home/filippo/Desktop/Codicini/Master_Thesis/SnowCover_studies/IT-Snow")

# Opening netCDF and getting variables
nc <- nc_open("singleMap.nc")
lon <- ncvar_get(nc, "lon")
lat <- ncvar_get(nc, "lat")
swe <- ncvar_get(nc, "SWE")
nc_close(nc)

# Raster requires latitude to be decreasing (i.e. from north to south), otherwise 
# the image will be created upside down
if (lat[2] > lat[1]) {
  lat <- rev(lat)
  swe <- swe[, ncol(swe):1]
}
swe <- t(swe)

# Creating raster
r <- rast(swe, extent = ext(min(lon), max(lon), min(lat), max(lat)),
          crs = "EPSG:4326")

# Saving raster in GEOTiff format
writeRaster(r, "SWE_map.tif", overwrite = TRUE)

