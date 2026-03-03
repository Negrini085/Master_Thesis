# The main goal of this script is to plot a single SWE map, in order to understand 
# which region is actually simulated!
rm(list = ls())
gc()

library(sf)
library(ncdf4)
library(terra)
library(ggplot2)
library(rnaturalearth)

fname <- "Dataset/2016/OSHD_DATA_2016-02.nc"
setwd("/home/filippo/Desktop/Codicini/Master_Thesis/SnowCover_studies/Alpine3D/Recent/")


# Importing dataset
nc <- nc_open(fname)
lon <- ncvar_get(nc, "easting")
lat <- ncvar_get(nc, "northing")
swe <- ncvar_get(nc, "swe", start = c(1, 1, 1), count = c(1, -1, -1))
nc_close(nc)

print(max(swe, na.rm = TRUE))

# Selecting the geographical background, in order to really understand where the
# snow coverage actually is
europe <- ne_countries(continent = "Europe", scale = 10, returnclass = "sf")

# Plotting snapshot without Italian boundaries. We covered this topic in the
# previous file, so we will skip documenting lines.
grid <- expand.grid(lon = lon, lat = lat)
grid$swe <- as.vector(swe)

ggplot() +
  geom_sf(data = europe, fill = "grey90", color = "black", inherit.aes = FALSE) +
  coord_sf(xlim = c(6, 11), ylim = c(45.5, 48)) +
  geom_raster(data = grid, aes(x = lon, y = lat, fill = swe)) +
  scale_fill_viridis_c(option = "C", na.value = "transparent") +
  labs(title = "Mean SWE map", x = "Longitude", y = "Latitude", fill = "SWE (mm w.e.)") +
  theme_minimal()