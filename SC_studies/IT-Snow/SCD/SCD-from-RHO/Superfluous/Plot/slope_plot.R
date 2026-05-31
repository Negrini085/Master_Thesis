# The main goal of this script is to plot trend significance
rm(list = ls())
gc()
  
library(sf)
library(ncdf4)
library(terra)
library(ggplot2)
library(rnaturalearth)
  
setwd("/home/filippo/Desktop/Codicini/Master_Thesis/SnowCover_studies/IT-Snow/SCD/SCD-from-RHO")



# Opening netCDF file of the variable we want to plot and importing dem model that
# will be used to clip standard deviation map to the actual extent of italian territory
f_name <- "Datas/slope_map.nc"
nc <- nc_open(f_name)
lat_name = "lat"
lon_name = "lon"
std_name = "Slope"
lat <- ncvar_get(nc, lat_name)
lon <- ncvar_get(nc, lon_name)
slope <- ncvar_get(nc, std_name)
nc_close(nc)

dem <- rast("Datas/DEM_Italy.tif")
dem <- as.matrix(dem, wide = TRUE)
dem <- dem[nrow(dem):1, ]
dem <- t(dem)

slope <- 10 * slope
slope[is.na(dem)] <- NA

  
  
# Selecting the geographical background, in order to really understand where the
# snow coverage actually is
europe <- ne_countries(continent = "Europe", scale = 10, returnclass = "sf")
  
# Plotting snapshot without Italian boundaries. We covered this topic in the
# previous file, so we will skip documenting lines.
grid <- expand.grid(lon = lon, lat = lat)
grid$slope <- as.vector(slope)

breaks  <- c(-Inf, -15, -10, -5, -2.5, 0, 2.5, 5, 10, 15, Inf)
labels  <- c(
  "< -15", "-15 to -10", "-10 to -5", "-5 to -2.5", "-2.5 to 0",
  "0 to 2.5", "2.5 to 5", "5 to 10", "10 to 15", "> 15"
  )

grid$slope_class <- cut(grid$slope, breaks = breaks, labels = labels, right = TRUE)
cols <- c(
  "< -15"      = "#8B0000",
  "-15 to -10" = "#D73027",
  "-10 to -5"  = "#F46D43",
  "-5 to -2.5" = "#FDAE61",
  "-2.5 to 0"  = "#FEE090",
  "0 to 2.5"   = "#E0F3F8",
  "2.5 to 5"   = "#ABD9E9",
  "5 to 10"    = "#74ADD1",
  "10 to 15"   = "#4575B4",
  "> 15"       = "#313695"
)
  
ggplot() +
  geom_sf(data = europe, fill = "grey90", color = "black", inherit.aes = FALSE) +
  coord_sf(xlim = c(6, 19), ylim = c(37, 46.8)) +
  geom_raster(data = grid, aes(x = lon, y = lat, fill = slope_class)) +
  scale_fill_manual(values = cols, na.value = "transparent", drop = FALSE) +
  labs(title = "Slope map", x = "Longitude", y = "Latitude", fill = "Slope [days/decade]") +
  theme_minimal()