# The main goal of this script is to plot SWE distribution, while also having
# plotted Italian topography.
rm(list = ls())
gc()
  
library(sf)
library(ncdf4)
library(terra)
library(ggplot2)
library(rnaturalearth)
  
setwd("/home/filippo/Desktop/Codicini/Master_Thesis/SnowCover_studies/IT-Snow/SCD-from-SWE")


# Opening raster and getting ready to plot DEM model
dem <- rast("Datas/DEM_Italy.tif")
lon <- xFromCol(dem, 1:ncol(dem))   
lat <- yFromRow(dem, 1:nrow(dem))

dem <- as.matrix(dem, wide = TRUE)
dem <- t(dem)
  

# Plotting procedure
europe <- ne_countries(continent = "Europe", scale = 10, returnclass = "sf")

grid <- expand.grid(lon = lon, lat = lat)
grid$dem <- as.vector(dem)
  
ggplot() +
  geom_sf(data = europe, fill = "grey90", color = "black", inherit.aes = FALSE) +
  coord_sf(xlim = c(6, 19), ylim = c(37, 46.8)) +
  geom_raster(data = grid, aes(x = lon, y = lat, fill = dem)) +
  scale_fill_viridis_c(option = "C", na.value = "transparent") +
  labs(title = "DEM Italian territory", x = "Longitude", y = "Latitude", fill = "Elevation [m]") +
  theme_minimal()
  