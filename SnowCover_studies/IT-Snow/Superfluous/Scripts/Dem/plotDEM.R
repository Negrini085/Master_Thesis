# The goal of this script is to plot resized DEM
rm(list = ls())
gc()

library(ncdf4)
library(terra)
library(ggplot2)

setwd("/home/filippo/Desktop/Codicini/Master_Thesis/SnowCover_studies/IT-Snow")

# Opening resized DEM
demR <- rast("DEM_Italy_resized.tif")
dem <- as.matrix(demR, wide = TRUE)
dem <- dem[nrow(dem):1, ]
dem <- t(dem)

# Opening netCDF file containing snow cover duration across the whole period
f_name = "Datas/yearlySCD.nc"
nc <- nc_open(f_name)
lat <- ncvar_get(nc, "lat")
lon <- ncvar_get(nc, "lon")
nc_close(nc)

grid <- expand.grid(lon = lon, lat = lat)
grid$dem <- as.vector(dem)

# Second step is creating a plot. The first thing you have to specify is the data
# you want to plot, then it's just a matter of specifying aesthetic issues
ggplot(grid, aes(x = lon, y = lat, fill = dem)) +
  geom_raster() +
  scale_fill_viridis_c(option = "C") +   # Continuous color palette
  coord_fixed() +
  labs(title = "DEM Italy", x = "Latitude", y = "Longitude", fill = "SWE (mm w.e.)") +     # Titles
  theme_minimal()
