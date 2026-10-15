# The main goal of this script is to extract SWE maps of the first April, in order to make trend analyses.
rm(list = ls())
gc()

library(abind)
library(ncdf4)

years <- 1952:2023
setwd("/home/filippo/Desktop/Codicini/Master_Thesis/SWE_analysis/")



# Importing longitude and latitude
nc <- nc_open("Dataset/SWE/SWE_1945.nc")
lon <- ncvar_get(nc, "lon")
lat <- ncvar_get(nc, "lat")
nc_close(nc)



# Cycle over years
swe <- array(NA_real_, dim = c(length(lon), length(lat), length(years)))
for(y in years){
  
  
  nc <- nc_open(paste0("Dataset/SWE/SWE_", y, ".nc"))
  time <- ncvar_get(nc, "time")
  nc_close(nc)
  
  # Selecting first June date
  dates <- as.Date("1950-01-01") + time
  idx <- which(dates == as.Date(paste0(y, "-06-01")))
  if (length(idx) != 1) stop("Requested map not found for ", y, "\n")
  
  # Extracting swe map
  nc <- nc_open(paste0("Dataset/SWE/SWE_", y, ".nc"))
  appo <- ncvar_get(nc, "swe", start = c(1, 1, idx), count = c(-1, -1, 1))
  nc_close(nc)

  # Saving swe map
  swe[, , which(years == y)] <- appo
  cat("Extracted swe map for the 1 June of ", y, "!", "\n")
}



# Saving swe maps
hy <- years
londim  <- ncdim_def("lon", "degrees_east",  as.double(lon))
latdim  <- ncdim_def("lat", "degrees_north", as.double(lat))
timedim <- ncdim_def("hydro_year", "year",   as.double(hy))

swe_def <- ncvar_def(
  name        = "swe",
  units       = "mm",
  dim         = list(londim, latdim, timedim),
  missval     = -9999,
  longname    = "Swe 1 June",
  prec        = "float",
  compression = 5
)

ncout <- nc_create("Results/SWE/first_june_maps.nc", swe_def, force_v4 = TRUE)
ncvar_put(ncout, swe_def, swe)

ncatt_put(ncout, "lon", "axis", "X")
ncatt_put(ncout, "lat", "axis", "Y")
ncatt_put(ncout, 0, "title", "SWE at 1 June, 1952-2023")
ncatt_put(ncout, 0, "history", paste("Created on", Sys.time()))

nc_close(ncout)