# The main goal of this script is to enable the user to compute standard deviation
# maps for both SCD products
rm(list = ls())
gc()

library(ncdf4)
library(matrixStats)

repo <- "SCD-from-SWE/"
fname_maps <- paste0(repo, "Datas/scd_annual_maps.nc")
setwd("/home/filippo/Desktop/Codicini/Master_Thesis/SnowCover_studies/IT-Snow/SCD")


# Getting swe maps
nc <- nc_open(fname_maps)
lon <- ncvar_get(nc, names(nc$dim)[1])
lat <- ncvar_get(nc, names(nc$dim)[2])
swe <- ncvar_get(nc, names(nc$var)[1])
nc_close(nc)


# Evaluating standard deviation. We need to make manual reshape.
dims <- dim(swe)
swe_2d <- matrix(swe, nrow = dims[1] * dims[2], ncol = dims[3])

std_vec <- rowSds(swe_2d, na.rm = TRUE)
std_map <- matrix(std_vec, nrow = dims[1], ncol = dims[2])


# Saving values on netCDF file
lat_dim <- ncdim_def("lat", "degrees_north", lat)
lon_dim <- ncdim_def("lon", "degrees_east", lon)

swe_var <- ncvar_def(
  name = "std", units = "Days", dim = list(lon_dim, lat_dim),
  missval = -9999, longname = "Snow Cover Days", prec = "float"
)

nc_out <- nc_create(paste0(repo, "Datas/scd_std_map.nc"), vars = list(swe_var))
ncvar_put(nc_out, swe_var, std_map)
nc_close(nc_out)
