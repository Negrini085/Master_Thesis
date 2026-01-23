# The main goal of this script is to understand how to create a nefCDF file useful
# to stack SWE maps. In particular I will try to stack 12 images to mimic what I
# will then do for months
rm(list = ls())
gc()

library(ncdf4)

setwd("/home/filippo/Desktop/Codicini/Master_Thesis/SnowCover_studies/IT-Snow")

nc <- nc_open("y2011/ITSNOW_SWE_201103.nc")
lat <- ncvar_get(nc, names(nc$dim)[1])
lon <- ncvar_get(nc, names(nc$dim)[2])
nlat <- length(lat)
nlon <- length(lon)
stackSWE <- array(0, dim = c(nlat, nlon, 12))


# Filling SWE stack
for(i in 1:12){
  swe <- ncvar_get(nc, names(nc$var)[2], start = c(1, 1, i), count = c(-1, -1, 1))
  stackSWE[, , i] <- swe
}


# Creating netCDF dimensions
lat_dim <- ncdim_def("lat", "degrees_north", lat)
lon_dim <- ncdim_def("lon", "degrees_east", lon)
time_dim <- ncdim_def("time", "days since 2011-03-01", 1:12, unlim = TRUE)

# Creating netCDF variable
swe_var <- ncvar_def(
  name = "SWE", units = "mm (w.e.)", dim = list(lat_dim, lon_dim, time_dim),
  missval = -9999, longname = "Snow Water Equivalent", prec = "float"
)

# Creating netCDF and filling SWE values
nc_out <- nc_create("exampleNetCDF.nc", vars = list(swe_var))
ncvar_put(nc_out, swe_var, stackSWE)
nc_close(nc_out)
