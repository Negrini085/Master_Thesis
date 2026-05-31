# The main goal of this script is to create a standard deviation map (and another
# one containing std/signal) to show inter-annual variability
rm(list = ls())
gc()

library(sf)
library(ncdf4)
library(terra)
library(ggplot2)
library(matrixStats)
library(rnaturalearth) 

setwd("/home/filippo/Desktop/Codicini/Master_Thesis/SnowCover_studies/IT-Snow/SCD/SCD-from-RHO")
fname_maps <- "Datas/season_maps_SCD.nc"



# Importing SCD annual maps for the whole time period. We will also need the mean
# map to compute noise over signal ratio
nc <- nc_open(fname_maps)
scd_maps <- ncvar_get(nc, names(nc$var)[1])
lon <- ncvar_get(nc, names(nc$dim)[1])
lat <- ncvar_get(nc, names(nc$dim)[2])
time <- ncvar_get(nc, names(nc$dim)[3])
nc_close(nc)



# Creating std map and noise over signal map. We should see a bigger variability 
# at higher elevations, but a bigger noise to signal ratio at lower ones.
scd_maps[is.na(scd_maps)] <- 0
std <- apply(scd_maps, c(1, 2), sd, na.rm = TRUE)
std <- round(std)



# Saving standard deviation map on netCDF file in order to later plot it/analyze it using tools
lat_dim <- ncdim_def("lat", "degrees_north", lat)
lon_dim <- ncdim_def("lon", "degrees_east", lon)

scd_var <- ncvar_def(
  name = "STD", units = "Days", dim = list(lon_dim, lat_dim),
  missval = NA, longname = "Standard deviation of snow cover duration", prec = "float"
)

nc_out <- nc_create("Datas/std_map.nc", vars = list(scd_var))
ncvar_put(nc_out, scd_var, std)
nc_close(nc_out)