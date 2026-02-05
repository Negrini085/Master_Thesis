# The main goal of this script is to compute snow cover duration at hydrological
# year scale, in order to later check if there is a dependence with elevation or not
rm(list = ls())
gc()

library(ncdf4)
library(matrixStats)

setwd("/home/filippo/Desktop/Codicini/Master_Thesis/SnowCover_studies/IT-Snow")

years <- 2011:2025
months <- c("09", "10", "11", "12", "01", "02", "03", "04", "05", "06", "07", "08")


# Opening a netCDF file in order to create an array with the correct dimension to 
# store SWE maps
nc <- nc_open("y2011/ITSNOW_SWE_201103.nc")
lat <- ncvar_get(nc, names(nc$dim)[1])
lon <- ncvar_get(nc, names(nc$dim)[2])
nlat <- length(lat)
nlon <- length(lon)
nc_close(nc)


appo <- array(0, dim = c(nlon, nlat))
stackSCD <- array(0, dim = c(nlon, nlat, length(years)))

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
    
    # Every pixel with a positive swe is treated as snow covered, otherwise as 
    # snow free
    swe[swe > 1e-5] <- 1
    swe[swe <= 0] <- 0
    
    monthMap <- rowSums(swe, dims = 2L, na.rm = TRUE)
    appo <- appo + monthMap
    
    nc_close(nc)
  }
  
  stackSCD[, , y-2010] <- appo
  appo <- array(0, dim = c(nlon, nlat))
}


# Creating netCDF dimensions
lat_dim <- ncdim_def("lat", "degrees_north", lat)
lon_dim <- ncdim_def("lon", "degrees_east", lon)
time_dim <- ncdim_def("time", "Hydrological years -> Sept to Aug", years, unlim = TRUE)

# Creating netCDF variable
swe_var <- ncvar_def(
  name = "SCD", units = "Days", dim = list(lon_dim, lat_dim, time_dim),
  missval = -9999, longname = "Snow Cover Duration", prec = "float"
)

# Creating netCDF and filling SWE values
nc_out <- nc_create("yearlySCD.nc", vars = list(swe_var))
ncvar_put(nc_out, swe_var, stackSCD)
nc_close(nc_out)