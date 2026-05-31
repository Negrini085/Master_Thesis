# The main goal of this script is to compute SCD maps in order to later test 
# whether there are some significant trend or not
rm(list = ls())
gc()

library(ncdf4)

fname <- "Dataset/SWE_1962_2023.nc"
setwd("/home/filippo/Desktop/Codicini/Master_Thesis/SnowCover_studies/Alpine3D/Historical/")

# Opening netCDF file connection
nc <- nc_open(fname)
lat <- ncvar_get(nc, "N")
lon <- ncvar_get(nc, "E")


# Cycle over years in order to create annual maps
conta <- 1
years <- 1962:2023
annual_maps <- array(0, dim = c(370, 265, length(years)))
for(year in years){
  
  # Importing daily swe maps for a given year (saving NA mask for later)
  if(year %% 4 == 0) len <- 366
  else len <- 365
  annual_swe <- ncvar_get(nc, "SWECLQMD", start = c(1, 1, conta), count = c(-1, -1, len))
  na_mask <- is.na(annual_swe[, , 1])
  
  # Converting from swe to sc
  mask <- annual_swe > 0.01 & !is.na(annual_swe)
  annual_swe[] <- as.integer(mask)
  rm(mask)
  gc()
  
  # Actual scd computation
  scd_layer <- apply(annual_swe, c(1, 2), sum)
  scd_layer[na_mask] <- NA
  
  annual_maps[, , year - years[1] + 1] <- scd_layer
  
  conta <- conta + len
  rm(annual_swe)
  gc()
  
  print(paste0("Made annual scd map for ", year))
}
nc_close(nc)


# Creating netCDF dimensions
lat_dim <- ncdim_def("lat", "swiss_lv95_coordinates", lat)
lon_dim <- ncdim_def("lon", "swiss_lv95_coordinates", lon)
time_dim <- ncdim_def("time", "Season maps Sept -> August: 1962 -> 2023", 1:length(1962:2023), unlim = TRUE)


# Creating netCDF variable
swe_var <- ncvar_def(
  name = "SCD", units = "Days", dim = list(lon_dim, lat_dim, time_dim),
  missval = NA, longname = "Snow Cover Duration", prec = "float"
)

# Creating netCDF and filling SWE values
nc_out <- nc_create("Datas/scd_maps.nc", vars = list(swe_var))
ncvar_put(nc_out, swe_var, annual_maps)
nc_close(nc_out)

