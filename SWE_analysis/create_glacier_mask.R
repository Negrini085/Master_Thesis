# The main goal of this script is to create a map which I will use to mask glaciers
rm(list = ls())
gc()

library(ncdf4)

years <- 1952:2023
setwd("/home/filippo/Desktop/Codicini/Master_Thesis/SWE_analysis/")


# Cycle over years
appo <- list()
for(y in years){
  # Selecting SWE for a given year
  nc <- nc_open(paste0("Dataset/SWE/SWE_", y, ".nc"))
  swe <- ncvar_get(nc, "swe")
  len_year <- dim(swe)[3]
  nc_close(nc)
  
  
  # Masking pixels to map snow coverage
  mask <- swe > 0 & !is.na(swe)
  swe[mask] <- 1
  
  mask <- swe <= 0 & !is.na(swe)
  swe[mask] <- 0
  
  
  # Selecting pixels which are snow covered the whole year
  snow_season <- rowSums(swe, dims = 2, na.rm = FALSE)
  rm(swe, mask); invisible(gc())
  
  mask <- snow_season == len_year
  appo[[y - years[1]+1]] <- mask
  cat("Taken care of", y, "!\n")
}

nc <- nc_open("Dataset/SWE/SWE_1951.nc")
lon <- ncvar_get(nc, "lon")
lat <- ncvar_get(nc, "lat")
years_continuous_snow <- Reduce(`+`, appo)


# Saving years to file
dim_lon <- ncdim_def("lon", "degrees_east",  lon)
dim_lat <- ncdim_def("lat", "degrees_north", lat)

var_snow <- ncvar_def(
  name     = "years_continuous_snow",
  units    = "1",
  dim      = list(dim_lon, dim_lat),
  missval  = -9999L,
  longname = "Number of years with snow cover on every day of the year",
  prec     = "integer",
  compression = 5
)

ncout <- nc_create("Results/years_continuous_snow.nc", var_snow, force_v4 = TRUE)
ncvar_put(ncout, var_snow, years_continuous_snow)

ncatt_put(ncout, "lon", "standard_name", "longitude")
ncatt_put(ncout, "lat", "standard_name", "latitude")
ncatt_put(ncout, 0, "title",   "Pixels with continuous snow cover (glacier mask)")
ncatt_put(ncout, 0, "history", paste("Created", date(), "from SWE", min(years), "-", max(years)))

nc_close(ncout)
nc_close(nc)  