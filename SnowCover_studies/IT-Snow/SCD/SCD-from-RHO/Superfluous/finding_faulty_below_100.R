# The main goal of this script is to find out why I have those faulty pixels. 
# I mean, why are there in the first place? Which filter is necessary to get rid
# of them? Those are the questions which need an answer.
rm(list = ls())
gc()

library(sf)
library(ncdf4)
library(terra)
library(ggplot2)
library(rnaturalearth)


# Setting directory and specifying correct file
setwd("/home/filippo/Desktop/Codicini/Master_Thesis/SnowCover_studies/IT-Snow/SCD-from-RHO/")
fname <- "Datas/mean_SCD_map.nc"

# Importing SCD map file
nc <- nc_open(fname)
lat <- ncvar_get(nc, "lat")
lon <- ncvar_get(nc, "lon")
scd <- ncvar_get(nc, "SCD")
nc_close(nc)

# Importing DEM. Here we need to make sure that we are importing the one of the 
# whole region, and not the one clipped only on Italy.
demR <- rast("../DEM/DEM_region.tif")
dem <- as.matrix(demR, wide = TRUE)
dem <- dem[nrow(dem):1, ]
dem <- t(dem)


# Checking how many of these pixel are part of our mean SCD map. I will be focusing at
# this stage only on the ones which have a snow cover duration of almost 100 days and 
# elevation close to sea level. We will also plot coordinates, so that we can identify 
# where this bug is present
mask <- scd > 30 & scd < 200 & dem < 50
if(length(scd[mask])){
  print("Numero di pixel difettosi pari a: ")
  print(length(scd[mask]))
}

# Finding pixel latitude and longitude in order to plot the investigated region. I feel
# like those could be a conseguence of a few faulty years, not such as Linosa whose bug 
# is present across the whole dataset
appo <- which(mask, arr.ind = TRUE)
lon_min <- min(appo[, 1])
lon_max <- max(appo[, 1])
lat_min <- min(appo[, 2])
lat_max <- max(appo[, 2])


# From now on the main focus is plotting faulty pixels located applying the previous mask.
appo_lat <- lat[lat_min : lat_max]
appo_lon <- lon[lon_min : lon_max]
appo_scd <- scd[lon_min:lon_max, lat_min:lat_max]
appo_scd[appo_scd < 30] <- NA


europe <- ne_countries(continent = "Europe", scale = 10, returnclass = "sf")
grid <- expand.grid(lon = appo_lon, lat = appo_lat)
grid$scd <- as.vector(appo_scd)

ggplot() +
  geom_sf(data = europe, fill = "grey90", color = "black", inherit.aes = FALSE) +
  coord_sf(xlim = c(appo_lon[1], appo_lon[length(appo_lon)]), ylim = c(appo_lat[1], appo_lat[length(appo_lat)])) +
  geom_raster(data = grid, aes(x = lon, y = lat, fill = scd)) +
  scale_fill_viridis_c(option = "C", na.value = "transparent") +
  labs(title = "Faulty pixels: SCD in (30, 100) and DEM < 40", x = "Longitude", y = "Latitude", fill = "Days") +
  theme_minimal()