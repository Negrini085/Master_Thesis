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
f_name <- "Datas/pval_map.nc"
nc <- nc_open(f_name)
lat_name = "lat"
lon_name = "lon"
std_name = "Pval"
lat <- ncvar_get(nc, lat_name)
lon <- ncvar_get(nc, lon_name)
pval <- ncvar_get(nc, std_name)
nc_close(nc)

dem <- rast("Datas/DEM_Italy.tif")
dem <- as.matrix(dem, wide = TRUE)
dem <- dem[nrow(dem):1, ]
dem <- t(dem)

pval[is.na(pval)] <- 1
pval[is.na(dem)] <- NA



# Selecting the geographical background, in order to really understand where the
# snow coverage actually is
europe <- ne_countries(continent = "Europe", scale = 10, returnclass = "sf")

# Plotting snapshot without Italian boundaries. We covered this topic in the
# previous file, so we will skip documenting lines.
grid <- expand.grid(lon = lon, lat = lat)
grid$pval <- as.vector(pval)

breaks  <- c(0, 0.01, 0.02, 0.03, 0.04, 0.05, 0.06, 0.07, 0.08, 0.09, 0.1, Inf)
labels  <- c(
  "99% confidence", "98% confidence", 
  "97% confidence", "96% confidence",
  "95% confidence", "94% confidence",
  "93% confidence", "92% confidence",
  "91% confidence", "90% confidence",
  "Non significant"
)

grid$pval_class <- cut(grid$pval, breaks = breaks, labels = labels, right = TRUE)
cols <- c(
  "99% confidence" = "#8B0000",
  "98% confidence" = "#D73027",
  "97% confidence" = "#F46D43",
  "96% confidence" = "#FDAE61",
  "95% confidence" = "#FEE090",
  "94% confidence" = "#E0F3F8",
  "93% confidence" = "#ABD9E9",
  "92% confidence" = "#74ADD1",
  "91% confidence" = "#4575B4",
  "90% confidence" = "#313695", 
  "Non significant" = "grey"
)

ggplot() +
  geom_sf(data = europe, fill = "grey90", color = "black", inherit.aes = FALSE) +
  coord_sf(xlim = c(6, 19), ylim = c(37, 46.8)) +
  geom_raster(data = grid, aes(x = lon, y = lat, fill = pval_class)) +
  scale_fill_manual(values = cols, na.value = "transparent", drop = FALSE) +
  labs(title = "Trend significance map: more detail", x = "Longitude", y = "Latitude", fill = "Confidence") +
  theme_minimal()