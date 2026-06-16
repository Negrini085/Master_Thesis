# The main goal of this script is to extract from MODIS dataset station series. I
# would like to identify which pixel hosts a given aws and then evaluate whether
# there is snow coverage or not.
rm(list = ls())
gc()

library(terra)

setwd("/home/filippo/Desktop/Codicini/Master_Thesis/HS_series/Correct/MODIS_series/")

# Function to extract station snow coverage series from MODIS dataset
snow_series <- function(station_lon, station_lat, start_year, end_year){

  sc_series <- numeric(0)
  if(start_year < 2000){ start_year <- 2000}
  
  years <- start_year:end_year
  for(y in years){
    
    len <- 365
    if(y %% 4 == 0) len <- 366
    if(y == 2000) len <- 312
    
    # Evaluating number of files
    ndays <- length(list.files(paste0("../../../SC_studies/MODIS/Dataset/daily/", y), pattern = "\\.tif$"))
    if(ndays != len) stop("Not enough maps for a given year of our dataset!")
    
    for(i in seq_len(ndays)){
      
      # Importing daily map
      fname <- paste0("../../../SC_studies/MODIS/Dataset/daily/", y, "/day_", sprintf("%03d", i), ".tif")
      print(paste0("Dealing with day ", sprintf("%03d", i), " of ", ndays, " for year ", y, "!"))
      daily_map <- rast(fname)
      
      # Selecting pixel
      ind <- cellFromXY(daily_map, cbind(station_lon, station_lat))
      appo <- as.numeric(daily_map[ind])
      
      # Elongating series
      if(is.na(appo)){ appo <- -90.0}
      sc_series <- c(sc_series, appo)
      rm(daily_map)
      gc()
    }
  }

  return(sc_series)
}



# Reading italian stations dataset
appo <- as.matrix(read.table("Dataset/start_end_years_filtered.dat", header = FALSE))
coord_ele <- matrix(as.numeric(appo[, 2:3]), ncol = 2)
start_year <- as.numeric(appo[, 4])
end_year <- as.numeric(appo[, 5])
station_names <- appo[, 1]
rm(appo)
gc()



# Cycle over stations
for(i in seq_len(length(station_names))){
  
  # Evaluating start position and snow cover series
  print(station_names[i])
  appo <- snow_series(coord_ele[i, 1], coord_ele[i, 2], start_year[i], end_year[i])
  
  # Saving single station series
  df <- data.frame(days = seq_len(length(appo)), sc_series = appo)
  write.table(df, paste0("Dataset/modis_series/", station_names[i]), row.names = FALSE, col.names = FALSE, quote = FALSE)
  
  rm(appo)
  gc()
}