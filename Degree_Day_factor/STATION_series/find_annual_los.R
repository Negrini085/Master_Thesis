# The main goal of this script is to find annual LOS values starting from station
# datas. I will consider as snow covered every day whose snow height is bigger than
# two centimeters. 
rm(list = ls())
gc()

fname <- "../MODIS_series/Datas/start_end_years_filtered.dat"
setwd("/home/filippo/Desktop/Codicini/Master_Thesis/Degree_Day_factor/STATION_series/")


# Function to find LOS duration for a given hydrological year
find_los_hydro <- function(hydro_sc, year, name){
  
  # Omitting NAs because those datas make the check harder
  appo <- na.omit(hydro_sc)
  
  # Snow height filter (above 2 cms is snow-covered, below no)
  mask <- appo > 2.0
  hydro_los <- length(appo[mask])
  
  # Checking if there is at least a snow covered day
  if(hydro_los == 0){
    print(paste0("No snow cover days for ", year, "! Station: ", name))
  }
  
  return(hydro_los)
}


# Function to find LOS duration for a given station
find_los_station <- function(station_name){
  
  # Importing snow cover series for a given station
  fname <- paste0("Datas/station_series/", station_name)
  df <- read.table(fname, header = FALSE)
  appo_years <- df$V1
  sc_series <- df$V2
  
  # Evaluating which hydrological year are taken into account for a given station
  station_los <- numeric(0)
  hydro_years <- sort(unique(appo_years))
  
  # Cycle to compute snow cover metrics across the whole investigated period
  for(year in hydro_years){
    mask <- appo_years == year
    hydro_sc <- sc_series[mask]
    
    # Actual computation
    appo <- find_los_hydro(hydro_sc = hydro_sc, year = year, name = station_name)
    
    # Saving snow cover metrics
    station_los <- c(station_los, appo)
  }
  
  return(data.frame(
    station  = station_name,
    year     = hydro_years,
    los = station_los
  ))
}


# Importing station names
appo <- as.matrix(read.table(fname, header = FALSE))
station_names <- appo[, 1]
rm(appo)
gc()

# Actually evaluating longest sc period
results <- data.frame()
for(name in station_names){
  appo <- find_los_station(station_name = name)
  results <- rbind(results, appo)
}

write.table(results, "Datas/los.dat", row.names = FALSE, quote = FALSE)