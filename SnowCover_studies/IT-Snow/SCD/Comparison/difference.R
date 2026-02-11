# The main goal is to compute the differences between annual SCD maps obtained
# using only SWE or a combination of SWE/snow density informations. The result
# will be saved in a netCDF file, in order to be usable in future analysis
rm(list = ls())
gc()

library(ncdf4)
library(matrixStats)

setwd("/home/filippo/Desktop/Codicini/Master_Thesis/SnowCover_studies/IT-Snow/SCD/Comparison/")
fname_from_swe <- "../SCD-from-SWE/Datas/season_maps_SCD.nc"
fname_from_rho <- "../SCD-from-RHO/Datas/season_maps_SCD.nc"



# Importing stack of maps spanning across the whole investigated period
nc <- nc_open(fname_from_swe)
lon <- ncvar_get(nc, names(nc$dim)[1])
lat <- ncvar_get(nc, names(nc$dim)[2])
time <- ncvar_get(nc, names(nc$dim)[3])
from_swe <- ncvar_get(nc, names(nc$var)[1])
nc_close(nc)

nc <- nc_open(fname_from_rho)
from_rho <- ncvar_get(nc, names(nc$var)[1])
nc_close(nc)



# Creating a container variable and filling it with map differences
appo <- array(0, dim = c(length(lon), length(lat), length(time)))
for(i in 1:length(time)){
  appo[, , i] <- from_rho[, , i] - from_swe[, , i]
}



# Saving difference maps in a netCDF file
# Creating netCDF dimensions
lat_dim <- ncdim_def("lat", "degrees_north", lat)
lon_dim <- ncdim_def("lon", "degrees_east", lon)
time_dim <- ncdim_def("time", "Hydrological years -> Sept to Aug", 1:length(time), unlim = TRUE)

# Creating netCDF variable
swe_var <- ncvar_def(
  name = "Diff", units = "Days", dim = list(lon_dim, lat_dim, time_dim),
  missval = -9999, longname = "Difference: from RHO minus from SWE", prec = "float"
)

# Creating netCDF and filling SWE values
nc_out <- nc_create("Datas/difference_maps.nc", vars = list(swe_var))
ncvar_put(nc_out, swe_var, appo)
nc_close(nc_out)