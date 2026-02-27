# The main goal of this script is to produce a SWE mean map. I will just sum seasonal
# map together and then divide for the number of years.
rm(list = ls())
gc()

library(ncdf4)
library(matrixStats)

years <- 2011:2025
fname <- "Datas/swe_season_maps.nc"
setwd("/home/filippo/Desktop/Codicini/Master_Thesis/SnowCover_studies/IT-Snow")


# Getting swe maps
nc <- nc_open(fname)
lon <- ncvar_get(nc, names(nc$dim)[1])
lat <- ncvar_get(nc, names(nc$dim)[2])
swe <- ncvar_get(nc, names(nc$var)[1])
nc_close(nc)


# Taking mean across third dimension
mean_map <- rowMeans(swe, dims = 2)

mask <- mean_map == 0
mean_map[mask] <- NA

# Saving values on netCDF file
lat_dim <- ncdim_def("lat", "degrees_north", lat)
lon_dim <- ncdim_def("lon", "degrees_east", lon)

swe_var <- ncvar_def(
  name = "mean", units = "mm (w.e.)", dim = list(lon_dim, lat_dim),
  missval = -9999, longname = "Snow Water Equivalent", prec = "float"
)

nc_out <- nc_create("swe_mean_map.nc", vars = list(swe_var))
ncvar_put(nc_out, swe_var, mean_map)
nc_close(nc_out)
