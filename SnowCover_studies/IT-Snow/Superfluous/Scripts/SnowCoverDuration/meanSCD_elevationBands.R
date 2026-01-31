# The main goal here is to check if we are handling elevation band correctly 
# or not. We will now try to color italy based on 9 elevation bands (500 m thick)
rm(list = ls())
gc()

library(terra)
library(ncdf4)
library(ggplot2)

setwd("/home/filippo/Desktop/Codicini/Master_Thesis/SnowCover_studies/IT-Snow")
fname <- "Datas/meanMapSCD.nc"

# Opening digitalized elevation model
demR <- rast("DEM/DEM_Italy.tif")
dem <- as.matrix(demR, wide = TRUE)
dem <- dem[nrow(dem):1, ]
dem <- t(dem)

# Opening netCDF file containing mean SCD map
nc <- nc_open(fname)
scdM <- ncvar_get(nc, names(nc$var)[1])
lon <- ncvar_get(nc, names(nc$dim)[1])
lat <- ncvar_get(nc, names(nc$dim)[2])
nc_close(nc)

bands <- c(500, 1000, 1500, 2000, 2500, 3000, 3500, 4000, 5000)
scdStack <- array(0, dim = c(length(lon), length(lat), length(bands)))

for(i in 1:length(bands)){
  # Setting band limits
  if(i==1){
    liminf <- 0
  }
  else{
    liminf <- bands[i-1]
  }
  limsup <- bands[i]
  
  appo <- scdM
  appo[dem <= liminf | dem > limsup] <- NA
  scdStack[, , i] <- appo
}


# Creating netCDF dimensions
lat_dim <- ncdim_def("lat", "degrees_north", lat)
lon_dim <- ncdim_def("lon", "degrees_east", lon)
bands_dim <- ncdim_def("bands", "Elevation bands 500 meters thick", 1:9, unlim = TRUE)


# Creating netCDF variable
scd_var <- ncvar_def(
  name = "SCD", units = "Days", dim = list(lon_dim, lat_dim, bands_dim),
  missval = -9999, longname = "Snow Cover Duration (for elevation band)", prec = "float"
)

# Creating netCDF and filling SWE values
nc_out <- nc_create("Datas/meanSCD_elevationMaps.nc", vars = list(scd_var))
ncvar_put(nc_out, scd_var, scdStack)
nc_close(nc_out)