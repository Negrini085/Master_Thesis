# The main goal of this script is to compute snow cover duration at hydrological
# year scale, in order to later check if there is a dependence with elevation or not
rm(list = ls())
gc()

library(ncdf4)
library(matrixStats)

setwd("/home/filippo/Desktop/Codicini/Master_Thesis/SnowCover_studies/IT-Snow/SCD-from-RHO")

years <- 2011:2025
months <- c("09", "10", "11", "12", "01", "02", "03", "04", "05", "06", "07", "08")


# Here I open a monthly netCDF file in order to obtain latitude and longitude 
# dimensions. Those are necessary to create an array to store scd maps
nc <- nc_open("../y2011/ITSNOW_SWE_201103.nc")
lat <- ncvar_get(nc, names(nc$dim)[1])
lon <- ncvar_get(nc, names(nc$dim)[2])
nlat <- length(lat)
nlon <- length(lon)
nc_close(nc)


appo <- numeric(0)
stackSCD <- array(0, dim = c(nlon, nlat, length(years)))

for(y in years){
  for(i in 1:length(months)){
    
    # The main goal of the following lines of code is to create the correct 
    # filename to later open the correct netCDF file.
    if(i<=4){
      ypr = y-1
      fname_swe = paste0("../y", toString(y), "/ITSNOW_SWE_", toString(y-1), months[i], ".nc")
      fname_rho = paste0("../y", toString(y), "/ITSNOW_RhoS_", toString(y-1), months[i], ".nc")
    }
    else{
      ypr = y
      fname_swe = paste0("../y", toString(y), "/ITSNOW_SWE_", toString(y), months[i], ".nc")
      fname_rho = paste0("../y", toString(y), "/ITSNOW_RhoS_", toString(y), months[i], ".nc")
    }

    print(paste0("Dealing with ", months[i], "/", ypr," SWE maps"))
    
    
    # Opening swe and rho maps. Before evaluating snow cover fraction we have to
    # maake sure that those two coincide
    nc <- nc_open(fname_swe)
    swe <- ncvar_get(nc, names(nc$var)[2])
    nc_close(nc)
    
    nc <- nc_open(fname_rho)
    rho <- ncvar_get(nc, names(nc$var)[2])
    nc_close(nc)

    
    # Computing snow cover fraction
    initial_mask <- !is.na(swe) & !is.na(rho) & rho != 0
    swe[!initial_mask] <- 0
    rho[!initial_mask] <- 1
    
    appo_sc <- swe/(0.1 * rho)
    print(length(appo_sc[is.na(appo_sc)]))  # Should be zero
    appo_sc[is.na(appo_sc)] <- 0
    sc <- pmin(appo_sc, 1)
    
    sc[!initial_mask] <- 0
    sc[sc >= 0.5] <- 1
    sc[sc < 0.5] <- 0
    
    
    # Summing across time dimension, in order to create a monthly map
    monthMap <- rowSums(sc, dims = 2L, na.rm = TRUE)
    
    if(i == 1){
      appo <- monthMap
    }
    else{
      appo <- appo + monthMap
    }

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
nc_out <- nc_create("Datas/season_maps_SCD.nc", vars = list(swe_var))
ncvar_put(nc_out, swe_var, stackSCD)
nc_close(nc_out)
