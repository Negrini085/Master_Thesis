# The main goal of this script is to plot mean an italian map of SCD.
rm(list = ls())
gc()
  
library(sf)
library(ncdf4)
library(ggplot2)
library(rnaturalearth)
  
  setwd("/home/filippo/Desktop/Codicini/Master_Thesis/SnowCover_studies/IT-Snow/SCD-from-RHO")
  
# Opening netCDF file and loading what we want to plot (this being the SWE snapshot
# of the first day of March). We will not get into details, because we already covered
# them in the previous couple of R files.
f_name <- "Datas/mean_SCD_map.nc"
nc <- nc_open(f_name)
lat_name = "lat"
lon_name = "lon"
scd_name = "SCD"
lat <- ncvar_get(nc, lat_name)
lon <- ncvar_get(nc, lon_name)
scd <- ncvar_get(nc, scd_name)
scd[scd == 0] <- NA
  
  
# Selecting the geographical background, in order to really understand where the
# snow coverage actually is
europe <- ne_countries(continent = "Europe", scale = 10, returnclass = "sf")
  
# Plotting snapshot without Italian boundaries. We covered this topic in the
# previous file, so we will skip documenting lines.
grid <- expand.grid(lon = lon, lat = lat)
grid$scd <- as.vector(scd)
  
ggplot() +
  geom_sf(data = europe, fill = "grey90", color = "black", inherit.aes = FALSE) +
  coord_sf(xlim = c(6, 19), ylim = c(37, 46.8)) +
  geom_raster(data = grid, aes(x = lon, y = lat, fill = scd)) +
  scale_fill_viridis_c(option = "C", na.value = "transparent") +
  labs(title = "Mean SCD: 2011 to 2025", x = "Longitude", y = "Latitude", fill = "Days") +
  theme_minimal()
  
nc_close(nc)