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
setwd("/home/filippo/Desktop/Codicini/Master_Thesis/SnowCover_studies/IT-Snow/SCD-from-SWE/")
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
# this stage only on the ones which have a snow cover duration of above 300 days and 
# elevation below 500 meters. We will also plot coordinates, so that we can identify 
# where this bug is present
appo <- scd
if(length(appo[appo > 300 & dem < 500])){
  print("Numero di pixel difettosi pari a: ")
  print(length(appo[appo > 300 & dem < 500]))
}
which(appo > 300 & dem < 500, arr.ind = TRUE)


# Plotting faulty pixels.
appo_lat <- lat[140 : 150]
appo_lon <- lon[1260 : 1270]
appo_swe <- swe[1260:1270, 140:150]
appo_swe[appo_swe == 0] <- NA

europe <- ne_countries(continent = "Europe", scale = 10, returnclass = "sf")
grid <- expand.grid(lon = appo_lon, lat = appo_lat)
grid$swe <- as.vector(appo_swe)

ggplot() +
  geom_sf(data = europe, fill = "grey90", color = "black", inherit.aes = FALSE) +
  coord_sf(xlim = c(lon[1260], lon[1270]), ylim = c(lat[140], lat[150])) +
  geom_raster(data = grid, aes(x = lon, y = lat, fill = swe)) +
  scale_fill_viridis_c(option = "C", na.value = "transparent") +
  labs(title = "Faulty pixels: 31 August 2011", x = "Longitude", y = "Latitude", fill = "SWE (mm w.e.)") +
  theme_minimal()