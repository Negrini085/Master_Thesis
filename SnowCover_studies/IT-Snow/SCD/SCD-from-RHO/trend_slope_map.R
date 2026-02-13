# The main goal of this script is to evaluate trend slope by means of Sen-Theil test
rm(list = ls())
gc()

library(ncdf4)
library(trend)
library(future.apply)
plan(multicore, workers = 8)
options(future.globals.maxSize = 3500 * 1024^2)

setwd("/home/filippo/Desktop/Codicini/Master_Thesis/SnowCover_studies/IT-Snow/SCD/SCD-from-RHO")
fname <- "Datas/season_maps_SCD.nc"


get_sen_slope <- function(x) {
  
  # Checking data quality
  if (all(is.na(x)) || sum(!is.na(x)) < 3) return(NA)
  
  # Evaluating slope
  res <- sens.slope(x)
  return(as.numeric(res$estimates))
}


# Importing SCD maps to be used for MK testing
nc <- nc_open(fname)
lon <- ncvar_get(nc, names(nc$dim)[1])
lat <- ncvar_get(nc, names(nc$dim)[2])
scd <- ncvar_get(nc, names(nc$var)[1])
nc_close(nc)



# Sen-Theil test
slope_map <- future_apply(scd, c(1, 2), get_sen_slope, future.seed = TRUE)
rm(scd)
gc()



# Saving average map on netCDF file in order to later plot it/analyze it using tools
lat_dim <- ncdim_def("lat", "degrees_north", lat)
lon_dim <- ncdim_def("lon", "degrees_east", lon)

scd_var <- ncvar_def(
  name = "Slope", units = "Days/year", dim = list(lon_dim, lat_dim),
  missval = NA, longname = "Sen-Theil slope", prec = "float"
)

nc_out <- nc_create("Datas/slope_map.nc", vars = list(scd_var))
ncvar_put(nc_out, scd_var, slope_map)
nc_close(nc_out)
