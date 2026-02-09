# The main goal of this script is to investigate the reasons behind the presence of 
# pixels that are almost always snow-covered, but at very low altitudes. What causes this?
rm(list = ls())
gc()

library(sf)
library(ncdf4)
library(terra)
library(ggplot2)
library(rnaturalearth)

setwd("/home/filippo/Desktop/Codicini/Master_Thesis/SnowCover_studies/IT-Snow")
fname <- "Datas/yearlySCD.nc"

# Importing DEM raster
demR <- rast("DEM/DEM_Italy.tif")
dem <- as.matrix(demR, wide = TRUE)
dem <- dem[nrow(dem):1, ]
dem <- t(dem)

# Maybe there is some problem with mean SCD map creation procedure, I'll try to check
# individually every map in order to find those pixels
nc <- nc_open(fname)
lat <- ncvar_get(nc, "lat")
lon <- ncvar_get(nc, "lon")
time <- ncvar_get(nc, "time")
scd <- ncvar_get(nc, "SCD")
nc_close(nc)

for(i in 1:length(time)){
  appo <- scd[, , i]
  if(length(appo[appo > 300 & dem < 500 & dem > -5])){
    print("Numero di pixel difettosi pari a: ")
    print(length(appo[appo > 300 & dem < 500 & dem > -5]))
  }
}

# So as we can check thank to the previous output, we have basically from 50 to 100
# pixels below 500 meters that are presumed to be snow covered for more than 300 days a
# year. For a country with Italy climate that is clearly impossible. The following part 
# of the script will focus on understanding if those snow covered pixel are really 
# part of IT-SNOW dataset, or if there is a bug in SCD evaluation procedure.
y <- 2011
months <- c("08")

for(i in 1:length(months)){
  
  # Logic conditions in order to create the correct file path
  fname <- paste("y", toString(y), "/ITSNOW_SWE_", toString(y), months[i], ".nc", sep="")
  
  # The real deal, opening netCDF4 datas and evaluating total SWE
  nc <- nc_open(fname)
  time <- ncvar_get(nc, names(nc$var[1]))
  for(j in time){
    k <- j+1
    
    swe <- ncvar_get(nc, names(nc$var[2]), start = c(1, 1, k), count = c(-1, -1, 1))
    swe[is.na(swe)] <- 0 
    print(paste0("Giorno ", toString(k), "/", months[i], "/", y, ": ", length(swe[dem > -5 & dem < 500 & swe > 0])))  
    
    if(length(swe[dem > -5 & dem < 500 & swe > 0]) == 15 & k > 28){
      print(which(dem > -5 & dem < 500 & swe > 0, arr.ind = TRUE))
    }
  }
  nc_close(nc)
}



# Here we want to plot the region which is giving faulty pixels
appo_lat <- lat[140 : 150]
appo_lon <- lon[1260 : 1270]
appo_swe <- swe[1260:1270, 140:150]
appo_swe[appo_swe == 0] <- NA

europe <- ne_countries(continent = "Europe", scale = 10, returnclass = "sf")
grid <- expand.grid(lon = appo_lon, lat = appo_lat)
grid$swe <- as.vector(appo_swe)

ggplot() +
  geom_sf(data = europe, fill = "grey90", color = "black", inherit.aes = FALSE) +
  coord_sf(xlim = c(lon[1260], lon[1270]), ylim = c(lat[140], lat[150])) +
  geom_raster(data = grid, aes(x = lon, y = lat, fill = swe)) +
  scale_fill_viridis_c(option = "C", na.value = "transparent") +
  labs(title = "Faulty pixels: 31 August 2011", x = "Longitude", y = "Latitude", fill = "SWE (mm w.e.)") +
  theme_minimal()


# The last question we want to answer is wether this bug is persistent across years or if 
# this 15 pixels change every year or so. To answer to this question, we will plot pixels 
# coordinate for at least two SCD maps
print(which(dem > -5 & dem < 500 & scd[, , 1] > 300, arr.ind = TRUE))
print(which(dem > -5 & dem < 500 & scd[, , 6] > 300, arr.ind = TRUE))
print(which(dem > -5 & dem < 500 & scd[, , 11] > 300, arr.ind = TRUE))
