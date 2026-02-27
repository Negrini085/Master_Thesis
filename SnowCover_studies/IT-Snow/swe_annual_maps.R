# The main goal of this script is to enable the user to compute swe annual maps, so
# considering the whole hydrological year. We will also store what we will find in a netCDF file. 
rm(list = ls())
gc()

library(ncdf4)
library(matrixStats)

setwd("/home/filippo/Desktop/Codicini/Master_Thesis/SnowCover_studies/IT-Snow")

years <- 2011:2025
months <- c("09", "10", "11", "12", "01", "02", "03", "04", "05", "06", "07", "08")

nc <- nc_open("y2011/ITSNOW_SWE_201011.nc")
lat <- ncvar_get(nc, names(nc$dim)[1])
lon <- ncvar_get(nc, names(nc$dim)[2])
nlat <- length(lat)
nlon <- length(lon)
nc_close(nc)

stackMonth <- array(0, dim = c(nlon, nlat, length(months)))
stackSWE <- array(0, dim = c(nlon, nlat, length(years)))

for(y in years){
  for(i in 1:length(months)){
    if(i<=4){
      ypr = y-1
      fname = paste0("y", toString(y), "/ITSNOW_SWE_", toString(y-1), months[i], ".nc")
    }
    else{
      ypr = y
      fname = paste0("y", toString(y), "/ITSNOW_SWE_", toString(y), months[i], ".nc")
    }
    
    print(paste0("Dealing with ", months[i], "/", ypr," SWE maps"))
    
    nc <- nc_open(fname)
    swe <- ncvar_get(nc, names(nc$var)[2])
    
    swe[is.na(swe)] <- 0
    stackMonth[, , i] <- rowMeans(swe, dims = 2)
    nc_close(nc)
  }
  
  # We are dealing with a leap year or not? We would also have to check y %% 100
  # but that's not important for this script because our time window spans only from 
  # 2011 to 2025
  if(y%%4 == 0){
    den <- 30+31+30+31+31+29+31+30+31+30+31+31
    stackSWE[, , y-2010] <- (30*stackMonth[, , 1] + 31*stackMonth[, , 2] + 
                               30*stackMonth[, , 3] + 31*stackMonth[, , 4] + 
                               31*stackMonth[, , 5] + 29*stackMonth[, , 6] + 
                               31*stackMonth[, , 7] + 30*stackMonth[, , 8] + 
                               31*stackMonth[, , 9] + 30*stackMonth[, , 10] + 
                               31*stackMonth[, , 11] + 31*stackMonth[, , 12]
                               )/den
  }
  else{
    den <- 30+31+30+31+31+28+31+30+31+30+31+31
    stackSWE[, , y-2010] <- (30*stackMonth[, , 1] + 31*stackMonth[, , 2] + 
                               30*stackMonth[, , 3] + 31*stackMonth[, , 4] + 
                               31*stackMonth[, , 5] + 28*stackMonth[, , 6] + 
                               31*stackMonth[, , 7] + 30*stackMonth[, , 8] + 
                               31*stackMonth[, , 9] + 30*stackMonth[, , 10] + 
                               31*stackMonth[, , 11] + 31*stackMonth[, , 12]
                             )/den
  }
  print(paste0("Finished year ", y))
}

# Creating netCDF dimensions
lat_dim <- ncdim_def("lat", "degrees_north", lat)
lon_dim <- ncdim_def("lon", "degrees_east", lon)
time_dim <- ncdim_def("time", "Annual map", 1:length(2011:2025), unlim = TRUE)

# Creating netCDF variable
swe_var <- ncvar_def(
  name = "SWE", units = "mm (w.e.)", dim = list(lon_dim, lat_dim, time_dim),
  missval = -9999, longname = "Snow Water Equivalent", prec = "float"
)

# Creating netCDF and filling SWE values
nc_out <- nc_create("swe_annual_maps.nc", vars = list(swe_var))
ncvar_put(nc_out, swe_var, stackSWE)
nc_close(nc_out)
