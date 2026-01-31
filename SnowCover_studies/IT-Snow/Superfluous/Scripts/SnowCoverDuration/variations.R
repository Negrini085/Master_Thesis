# The main goal of this script is to evaluate which pixels have more (or less) 
# snow coverage than the avarage for their elevation band. We just have to load 
# average SWE map and then subtract accordingly for elevation bands
rm(list = ls())
gc()

library(sf)
library(ncdf4)
library(terra)
library(ggplot2)
library(matrixStats)
library(rnaturalearth)

setwd("/home/filippo/Desktop/Codicini/Master_Thesis/SnowCover_studies/IT-Snow")
fname <- "Datas/meanMapSCD.nc"

# Time window
bands <- seq(50, 4000, 50)
scd_appo <- NULL

# Loading elevation model
demR <- rast("DEM/DEM_Italy.tif")
dem <- as.matrix(demR, wide = TRUE)
dem <- dem[nrow(dem):1, ]
dem <- t(dem)

# Opening netCDF file containing mean SCD map
nc <- nc_open(fname)
scdM <- ncvar_get(nc, names(nc$var)[1])
lon <- ncvar_get(nc, names(nc$dim)[1])
lat <- ncvar_get(nc, names(nc$dim)[2])
nc_close(nc)

# Loading mean SCD values for elevation band
scdE <- read.table("Datas/scd_elevation_bands.dat")
scdE <- scdE$V1
print(scdE)

# Checking for NAs
for(i in 1:length(bands)){
  # Setting band limits
  if(i==1){
    liminf <- 0
  }
  else{
    liminf <- bands[i-1]
  }
  limsup <- bands[i]
  
  scdM[dem > liminf & dem <= limsup & !is.na(scdM)] <- scdM[dem > liminf & dem <= limsup & !is.na(scdM)] - scdE[i]
}

# Plotting difference from mean SCD map
grid <- expand.grid(lon = lon, lat = lat)
grid$scd <- as.vector(round(scdM, 0))

europe <- ne_countries(continent = "Europe", scale = 10, returnclass = "sf")
ggplot() +
  geom_sf(data = europe, fill = "grey90", color = "black", inherit.aes = FALSE) +
  coord_sf(xlim = c(6, 19), ylim = c(37, 46.8)) +
  geom_raster(data = grid, aes(x = lon, y = lat, fill = scd)) +
  scale_fill_gradient2(low = "red", mid = "white", high = "blue", midpoint = 0, limits = c(-40, 40), na.value = "transparent") + 
  labs(title = "Deviation from mean SCD over 2011 - 2025 period", x = "Longitude", y = "Latitude", fill = "Days") +
  theme_minimal()