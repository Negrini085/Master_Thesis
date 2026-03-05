# The main goal of this script is to test the function I want to use to create
# snow cover series.
rm(list = ls())
gc()

setwd("/home/filippo/Desktop/Codicini/Master_Thesis/Degree_Day_factor/MODIS_series/")


# Function to be tested
snow_series <- function(station_lon, station_lat, start_year, end_year, ind_pix){
  
  sc_series <- numeric(0)
  if(start_year < 2000){ start_year <- 2000}
  
  years <- start_year:end_year
  for(y in years){
    
    # Evaluating number of files
    ndays <- length(list.files(paste0("Superfluous/Files/Tif/", y), pattern = "\\.tif$"))
    for(i in seq_len(ndays)){
      
      # Importing daily map
      fname <- paste0("Superfluous/Files/Tif/", y, "/test_", sprintf("%02d", i), ".tif")
      print(paste0("Dealing with day ", i, " of ", ndays, " for year ", y, "!"))
      daily_map <- rast(fname)
      
      # Selecting pixel
      ind <- ind_pix
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


# First test -> first row, third column
print("First test: ")
print(snow_series(station_lon = 60.0, station_lat = 120.0, start_year = 2000, end_year = 2000, ind_pix = 3))


# Second test -> second row, second column
print("Second test: ")
print(snow_series(station_lon = 0.0, station_lat = 0.0, start_year = 2001, end_year = 2003, ind_pix = 5))