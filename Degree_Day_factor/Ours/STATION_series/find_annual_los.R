# The main goal of this script is to find annual LOS values starting from station
# datas. I will consider as snow covered every day whose snow height is bigger than
# two centimeters. 
rm(list = ls())
gc()

fname <- "Dataset/station_series/usable_stations.dat"
setwd("/home/filippo/Desktop/Codicini/Master_Thesis/Degree_Day_factor/Ours/STATION_series/")


# Function to find LOS duration for a given hydrological year
find_los_hydro <- function(hydro_sc, year, name){
  
  # First thing first I need to do some checks on data quality. I will try to 
  # use the same filters I was using back with our series
  if(sum(is.na(hydro_sc), na.rm = TRUE) > 250) return(NA)
  else if(sum(is.na(hydro_sc[92:212]), na.rm = TRUE) > 60) return(NA)
  else if(sum((is.na(hydro_sc) | hydro_sc == 0), na.rm = TRUE) == length(hydro_sc)) return(NA)
  
  # Omitting NAs because those datas make the check harder
  appo <- na.omit(hydro_sc)
  
  # Sum of annual datas (only ones and zeros)
  mask <- appo == 1
  hydro_los <- length(appo[mask])
  
  # Checking if there is at least a snow covered day
  if(hydro_los == 0){
    print(paste0("No snow cover days for ", year, "! Station: ", name))
  }
  
  return(hydro_los)
}


# Function to find LOS duration for a given station
find_los_station <- function(station_name, mark){
  
  # Importing snow cover series for a given station
  fname <- paste0("Dataset/sc_series/", station_name)
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
    los = station_los, 
    appo_m = rep(mark, length(hydro_years))
  ))
}


# Importing station names
df <- read.table(fname, header = FALSE)
station_names <- df$V1
mark <- df$V2
rm(df)
gc()

# Actually evaluating longest sc period
results <- data.frame()
for(i in seq_along(station_names)){
  appo <- find_los_station(station_names[i], mark[i])
  results <- rbind(results, appo)
}

write.table(results, "Results/los_filtered.dat", row.names = FALSE, col.names = FALSE, quote = FALSE)