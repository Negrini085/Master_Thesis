# The main goal of this script is to check whether the positions I found made a
# significant difference.
rm(list = ls())
gc()

library(terra)
setwd("/home/filippo/Desktop/Codicini/Master_Thesis/Degree_Day_factor/Matiu/STATION_check/")


# Importing DEM and corrected dataset in order to check if now stations are still
# classified as faulty
dem <- rast("../DEM/DEM_region.tif")
df <- read.table("Correcting/correcting_dataset_ita.dat", header = FALSE)

mask <- df$V5 == "MOD" | df$V5 == "OK"
names <- df$V1[mask]
lon_stations <- df$V2[mask]
lat_stations <- df$V3[mask]
ele_stations <- df$V4[mask]


# Looking for an almost perfect match 
n_faulty <- 0
ele_lim <- 12
for(i in 1:length(lon_stations)){
  
  # Selecting correct raster pixel and evaluating elevation
  point <- matrix(c(as.numeric(lon_stations[i]), as.numeric(lat_stations[i])), ncol = 2, byrow = TRUE)
  ind <- cellFromXY(dem, point)
  dem_ele <- as.numeric(dem[ind])
  
  diff <- dem_ele - as.numeric(ele_stations[i])
  if(abs(diff) > ele_lim){
    print(diff)
    print(dem_ele)
    print(lat_stations[i])
    print(lon_stations[i])
    print(as.numeric(ele_stations[i]))
    print(paste0("Problems with ", names[i], " station"))
    
    n_faulty <- n_faulty + 1
  }
}

print(paste0("Finished! ", length(lon_stations) - n_faulty, " stations recovered!"))