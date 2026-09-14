# The main goal of this script is to convert SWE maps into SCD maps, in order to 
# make snow cover duration analysis.
rm(list = ls())
gc()

library(ncdf4)

years <- 1951:2023
setwd("/home/filippo/Desktop/Codicini/Master_Thesis/SWE_analysis/")




# Importing longitude and latitude
nc <- nc_open("Dataset/SWE/SWE_1951.nc")
lon <- ncvar_get(nc, "lon")
lat <- ncvar_get(nc, "lat")
nc_close(nc)


# Cycle over years
for(y in years){
  
  # Selecting SWE for a given year
  nc <- nc_open(paste0("Dataset/SWE/SWE_", y, ".nc"))
  tunits <- ncatt_get(nc, "time", "units")$value
  tcal <- ncatt_get(nc, "time", "calendar")
  tvals  <- ncvar_get(nc, "time")
  swe <- ncvar_get(nc, "swe")
  snow_cover <- swe
  nc_close(nc)

  
  # Masking pixels to assess snow cover
  stopifnot(identical(dim(swe), c(length(lon), length(lat), length(tvals))))
  
  mask <- swe > 0 & !is.na(swe)
  snow_cover[mask] <- 1
  
  mask <- swe <= 0 & !is.na(swe)
  snow_cover[mask] <- 0
  
  
  # Saving snow cover to netCDF
  dim_lon  <- ncdim_def("lon", "degrees_east",  lon)
  dim_lat  <- ncdim_def("lat", "degrees_north", lat)
  dim_time <- ncdim_def("time", tunits, tvals, unlim = TRUE)
  
  var_sc <- ncvar_def(
    "sc", "1", list(dim_lon, dim_lat, dim_time), missval = -9999, 
    longname = "Snow cover flag", prec = "short", compression = 5)
  
  ncout <- nc_create(paste0("Dataset/SC/SC_", y, ".nc"), var_sc, force_v4 = TRUE)
  ncvar_put(ncout, var_sc, snow_cover)
  if (tcal$hasatt) ncatt_put(ncout, "time", "calendar", tcal$value)
  nc_close(ncout)
  
  rm(swe, snow_cover); gc()
  cat("Taken into account hydrological year", y, "of our record!", "\n")
}
