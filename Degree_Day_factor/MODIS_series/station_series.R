# The main goal of this script is to extract from MODIS dataset station series. I
# would like to identify which pixel hosts a given aws and then evaluate whether
# there is snow coverage or not.
rm(list = ls())
gc()

library(terra)

setwd("/home/filippo/Desktop/Codicini/Master_Thesis/Degree_Day_factor/MODIS_series/")

# Function to extract station snow coverage series from MODIS dataset
snow_series <- function(station_name, station_lon, station_lat, start_year, end_year){

  sc_series <- numeric(0)
  if(start_year < 2000){ start_year <- 2000}
  
  years <- start_year:end_year
  for(y in years){
    
    # Evaluating number of files
    ndays <- length(list.files(paste0("../../SnowCover_studies/MODIS/Dataset/daily/", y), pattern = "\\.tif$"))
    for(i in seq_len(ndays)){
      
      # Importing daily map
      fname <- paste0("../../SnowCover_studies/MODIS/Dataset/daily/", y, "/day_", i, ".tif")
      daily_map <- rast(fname)
      
      # Selecting pixel
      ind <- cellFromXY(daily_map, cbind(station_lon, station_lat))
      appo <- daily_map[ind]
      
      # Elongting series
      if(is.na(appo)){ appo <- -90.0}
      sc_series <- c(sc_series, appo)
      rm(daily_map)
      gc()
    }
  }

  return(sc_series)
}


# Function to compute number of maps
num_maps <- function(){
  
  ntot <- 0
  years <- 2000:2025
  for(y in years){
    
    # Evaluating number of files
    ndays <- length(list.files(paste0("../../SnowCover_studies/MODIS/Dataset/daily/", y), pattern = "\\.tif$"))
    ntot <- ntot + ndays
  }
  
  return(ntot)
}





# Reading italian stations dataset
appo <- as.matrix(read.table("Datas/start_end_years_filtered.dat", header = FALSE))
coord_ele <- matrix(as.numeric(appo[, 2:3]), ncol = 2)
start_year <- as.numeric(appo[, 4])
end_year <- as.numeric(appo[, 5])
station_names <- appo[, 1]
rm(appo)
gc()


# Cycle over stations
ntot <- num_maps()
nstat <- length(station_names)
station_series <- array(NA, dim = c(ntot, nstat))

for(i in seq_len(length(station_names))){
  appo <-
}