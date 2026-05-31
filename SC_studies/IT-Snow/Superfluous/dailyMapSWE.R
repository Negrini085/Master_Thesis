# The main goal of this script is to create a netCDF file with only one SWE map,
# that needs to be used to create a raster loadable in QGIS. In particular, I would
# like to remap a DEM to my specific resolution to make elevation-wise analysis of 
# snow cover duration and snow water equivalent.
rm(list = ls())
gc()

library(ncdf4)

setwd("/home/filippo/Desktop/Codicini/Master_Thesis/SnowCover_studies/IT-Snow")


# Opening a netCDF file to extract a single daily map
nc <- nc_open("y2011/ITSNOW_SWE_201103.nc")
lat <- ncvar_get(nc, names(nc$dim)[1])
lon <- ncvar_get(nc, names(nc$dim)[2])
swe <- ncvar_get(nc, names(nc$var)[2], start = c(1, 1, 1), count = c(-1, -1, 1))
nc_close(nc)


# Creating netCDF dimensions
lat_dim <- ncdim_def("lat", "degrees_north", lat)
lon_dim <- ncdim_def("lon", "degrees_east", lon)

# Creating netCDF variable
swe_var <- ncvar_def(
  name = "SWE", units = "mm (w.e.)", dim = list(lon_dim, lat_dim),
  missval = -9999, longname = "Snow Water Equivalent", prec = "float"
)

# Creating netCDF and filling SWE values
nc_out <- nc_create("singleMap.nc", vars = list(swe_var))
ncvar_put(nc_out, swe_var, swe)
nc_close(nc_out)