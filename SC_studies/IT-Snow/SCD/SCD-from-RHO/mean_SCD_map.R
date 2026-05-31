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

# Setting directory and file name
setwd("/home/filippo/Desktop/Codicini/Master_Thesis/SnowCover_studies/IT-Snow/SCD-from-RHO")
fname <- "Datas/season_maps_SCD.nc"

# Importing SCD annual maps for the whole time period 
nc <- nc_open(fname)
scd <- ncvar_get(nc, names(nc$var)[1])
lon <- ncvar_get(nc, names(nc$dim)[1])
lat <- ncvar_get(nc, names(nc$dim)[2])
time <- ncvar_get(nc, names(nc$dim)[3])
nc_close(nc)

# Creating mean map for the whole period. I feel like there should not be NA values,
# but it's always better to be sure (so I will put an additional check on NAs)
scd[is.na(scd)] <- 0
meanSCD <- rowMeans(scd, dims = 2)
meanSCD <- round(meanSCD)


# Saving average map on netCDF file in order to later plot it/analyze it using tools
lat_dim <- ncdim_def("lat", "degrees_north", lat)
lon_dim <- ncdim_def("lon", "degrees_east", lon)

scd_var <- ncvar_def(
  name = "SCD", units = "Days", dim = list(lon_dim, lat_dim),
  missval = NA, longname = "Snow Cover Duration", prec = "float"
)

nc_out <- nc_create("Datas/mean_SCD_map.nc", vars = list(scd_var))
ncvar_put(nc_out, scd_var, meanSCD)
nc_close(nc_out)