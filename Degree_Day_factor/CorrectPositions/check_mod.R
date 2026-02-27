# The main goal of this script is to check whether the positions I found made a
# significant difference.
rm(list = ls())
gc()

library(terra)
setwd("/home/filippo/Desktop/Codicini/Master_Thesis/Degree_Day_factor/CorrectPositions/")


# Importing DEM and corrected dataset in order to check if now stations are still
# classified as faulty
dem <- rast("../DEM/DEM_stations_30.tif")
df <- read.table("correcting_dataset.dat")

mask <- df[[5]] == "MOD"
names <- df[[1]][mask]
lon_stations <- df[[2]][mask]
lat_stations <- df[[3]][mask]
ele_stations <- df[[4]][mask]


# Looking for an almost perfect match 
n_faulty <- 0
ele_lim <- 10
for(i in 1:length(lon_stations)){
  
  # Selecting correct raster pixel and evaluating elevation
  point <- matrix(c(as.numeric(lon_stations[i]), as.numeric(lat_stations[i])), ncol = 2, byrow = TRUE)
  ind <- cellFromXY(dem, point)
  dem_ele <- as.numeric(dem[ind])
  
  diff <- dem_ele - as.numeric(ele_stations[i])
  if(diff > ele_lim){
    print(diff)
    print(dem_ele)
    print(as.numeric(ele_stations[i]))
    print(paste0("Problems with ", names[i], " station"))
    
    n_faulty <- n_faulty + 1
  }
}

print(paste0("Finished! ", length(lon_stations) - diff, " stations recovered!"))