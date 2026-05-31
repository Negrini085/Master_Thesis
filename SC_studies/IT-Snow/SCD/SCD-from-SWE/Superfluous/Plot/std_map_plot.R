# The main goal of this script is to plot standard deviation map across Italy.
rm(list = ls())
gc()
  
library(sf)
library(ncdf4)
library(ggplot2)
library(rnaturalearth)
  
setwd("/home/filippo/Desktop/Codicini/Master_Thesis/SnowCover_studies/IT-Snow/SCD/SCD-from-SWE")



# Opening netCDF file of the variable we want to plot and importing dem model that
# will be used to clip standard deviation map to the actual extent of italian territory
f_name <- "Datas/std_map.nc"
nc <- nc_open(f_name)
lat_name = "lat"
lon_name = "lon"
std_name = "STD"
lat <- ncvar_get(nc, lat_name)
lon <- ncvar_get(nc, lon_name)
std <- ncvar_get(nc, std_name)
nc_close(nc)

dem <- rast("Datas/DEM_Italy.tif")
dem <- as.matrix(dem, wide = TRUE)
dem <- dem[nrow(dem):1, ]
dem <- t(dem)

std[is.na(dem)] <- NA

  
  
# Selecting the geographical background, in order to really understand where the
# snow coverage actually is
europe <- ne_countries(continent = "Europe", scale = 10, returnclass = "sf")
  
# Plotting snapshot without Italian boundaries. We covered this topic in the
# previous file, so we will skip documenting lines.
grid <- expand.grid(lon = lon, lat = lat)
grid$std <- as.vector(std)

breaks  <- c(-Inf, 10, 15, 20, 25, 30, Inf)
labels  <- c("0 - 10", "10 - 15", "15 - 20", "20 - 25", "25 - 30", "> 30")

grid$std_class <- cut(grid$std, breaks = breaks, labels = labels, right = TRUE)
cols <- c(
  "0 - 10"  = "#b44b3a",
  "10 - 15" = "#f39c12",
  "15 - 20" = "#ffe600",
  "20 - 25" = "#00c400",
  "25 - 30" = "#27b4b4",
  "> 30"    = "#0b2a7a"
)
  
ggplot() +
  geom_sf(data = europe, fill = "grey90", color = "black", inherit.aes = FALSE) +
  coord_sf(xlim = c(6, 19), ylim = c(37, 46.8)) +
  geom_raster(data = grid, aes(x = lon, y = lat, fill = std_class)) +
  scale_fill_manual(values = cols, na.value = "transparent", drop = FALSE) +
  labs(title = "SCD standard deviation map", x = "Longitude", y = "Latitude", fill = "Standard\ndeviation (days)") +
  theme_minimal()