# The main goal of this script is to evaluate trend significance by means of MK test
rm(list = ls())
gc()

library(ncdf4)
library(trend)
library(future.apply)
plan(multicore, workers = 8)
options(future.globals.maxSize = 3500 * 1024^2)

repo <- "SCD-from-SWE"
setwd(paste0("/home/filippo/Desktop/Codicini/Master_Thesis/SnowCover_studies/IT-Snow/SCD/", repo))
fname <- "Datas/scd_annual_maps.nc"
  
get_p_value <- function(x){
  
  # Quality check on NAs and data variability (maybe not the best for high elevation)
  if(sum(!is.na(x)) < 5) {
    return(NA)
  }
  else if(sum(x == 0) > 5){
    return(NA)
  }
  
  return(mk.test(x)$p.value)
}



# Importing SCD maps to be used for MK testing
nc <- nc_open(fname)
lon <- ncvar_get(nc, names(nc$dim)[1])
lat <- ncvar_get(nc, names(nc$dim)[2])
scd <- ncvar_get(nc, names(nc$var)[1])
nc_close(nc)



# MK test
mk_map <- future_apply(scd, c(1, 2), get_p_value, future.seed = TRUE)
rm(scd)
gc()


# Saving average map on netCDF file in order to later plot it/analyze it using tools
lat_dim <- ncdim_def("lat", "degrees_north", lat)
lon_dim <- ncdim_def("lon", "degrees_east", lon)

scd_var <- ncvar_def(
  name = "pval", units = "Probability", dim = list(lon_dim, lat_dim),
  missval = NA, longname = "P-value MK test", prec = "float"
)

nc_out <- nc_create("Datas/pval_map.nc", vars = list(scd_var))
ncvar_put(nc_out, scd_var, mk_map)
nc_close(nc_out)
