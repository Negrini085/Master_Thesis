# The main focus of this script is to study the connection between snow cover
# duration and elevation. In particular, I would like to study the mean scd 
# values in different elevation bands, and the plot them together
rm(list = ls())
gc()

library(sf)
library(ncdf4)
library(terra)
library(ggplot2)
library(matrixStats)
library(rnaturalearth) 

# Function to compute mean snow cover duration values
meanBandSCD <- function(scd, dem, bands){
  
  liminf <- NULL
  limsup <- NULL
  appo <- numeric(length(bands))
  for(i in 1:length(bands)){
    
    # Selecting band limits (the higher one will be consideed as part of the band,
    # while the lower one will not be a part of it)
    if(i == 1){
      liminf <- 0
    }
    else{
      liminf <- bands[i-1]
    }
      limsup <- bands[i]
    
    appo[i] <- mean(scd[dem > liminf & dem <= limsup], na.rm = TRUE)
  }
  
  return(round(appo, 0))
}



setwd("/home/filippo/Desktop/Codicini/Master_Thesis/SnowCover_studies/IT-Snow")
fname <- "Datas/yearlySCD.nc"

# Time window
bands <- seq(50, 4000, 50)
scd_appo <- NULL

demR <- rast("DEM/DEM_Italy.tif")
dem <- as.matrix(demR, wide = TRUE)
dem <- dem[nrow(dem):1, ]
dem <- t(dem)

nc <- nc_open(fname)
scd <- ncvar_get(nc, names(nc$var)[1])
lon <- ncvar_get(nc, names(nc$dim)[1])
lat <- ncvar_get(nc, names(nc$dim)[2])
time <- ncvar_get(nc, names(nc$dim)[3])
nc_close(nc)

# Creating mean map (over hydrological years)
scd[is.na(scd)] <- 0
meanSCD <- rowMeans(scd, dims = 2)
latM <- matrix(rep(lat, each = length(lon)), nrow = length(lon), ncol = length(lat))

# Check to consider only italian territory
meanSCD[dem < -15] <- NA
meanSCD[meanSCD <= 0 & dem > 500 & latM > 44] <- NA

# Evaluating mean SCD values for elevation band
scd_appo <- meanBandSCD(scd = meanSCD, dem = dem, bands = bands)
write.table(scd_appo, file = "Datas/scd_elevation_bands.dat", row.names = FALSE, col.names = FALSE)

# Saving mean SCD map
lat_dim <- ncdim_def("lat", "degrees_north", lat)
lon_dim <- ncdim_def("lon", "degrees_east", lon)

scd_var <- ncvar_def(
  name = "SCD", units = "Days", dim = list(lon_dim, lat_dim),
  missval = NA, longname = "Snow Cover Duration", prec = "float"
)

nc_out <- nc_create("Datas/meanMapSCD.nc", vars = list(scd_var))
ncvar_put(nc_out, scd_var, meanSCD)
nc_close(nc_out)

# Plotting mean SCD map
grid <- expand.grid(lon = lon, lat = lat)
grid$scd <- as.vector(round(meanSCD, 0))

europe <- ne_countries(continent = "Europe", scale = 10, returnclass = "sf")
ggplot() +
  geom_sf(data = europe, fill = "grey90", color = "black", inherit.aes = FALSE) +
  coord_sf(xlim = c(6, 19), ylim = c(37, 46.8)) +
  geom_raster(data = grid, aes(x = lon, y = lat, fill = scd)) +
  scale_fill_viridis_c(option = "C", na.value = "transparent") +
  labs(title = "Mean annual SCD over 2011 - 2025 period", x = "Longitude", y = "Latitude", fill = "Days") +
  theme_minimal()