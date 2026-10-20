# The main goal of this script is to make river basin rasters in order to compute 
# climatological swe values for each of them.
rm(list = ls())
gc()

library(sf)
library(terra)
library(dplyr)

fname <- "DEM/river_basin_shapefiles.shp"
setwd("/home/filippo/Desktop/Codicini/Master_Thesis/SWE_analysis/")



# Importing river basin  maps and extracting the desired one
shp <- st_read(fname)
names(shp)
unique(shp$id)
sel <- shp %>% filter(id == 5)


# Rasterization
v <- vect(sel)
r_model <- rast(v, res = 1/120)
r <- rasterize(v, r_model)
writeRaster(r, "output.tif", overwrite = TRUE)