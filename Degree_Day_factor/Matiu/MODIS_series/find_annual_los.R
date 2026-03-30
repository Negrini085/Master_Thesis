# The main goal of this script is to find LOS values for a given station of my
# dataset. I just need to find out how many ones there are in a given hydro year
rm(list = ls())
gc()

setwd("/home/filippo/Desktop/Codicini/Master_Thesis/Degree_Day_factor/Matiu/MODIS_series/")


# Function to find LOS duration for a given hydrological year
find_los_hydro <- function(hydro_sc, year, name){
  
  # Mask to pixel values (I'm looking for one, namely snow cover)
  mask <- hydro_sc == 1 & !is.na(hydro_sc)
  hydro_los <- length(hydro_sc[mask])
  
  # Checking if there is at least a snow covered day
  if(hydro_los == 0){
    print(paste0("No snow cover days for ", year, "! Station: ", name))
  }
  
  return(hydro_los)
}


# Function to find LOS duration for a given station
find_los_station <- function(station_name, mark){
  
  # Importing snow cover series for a given station
  if(mark == "NO") fname <- paste0("Dataset/hydro_series/non_compatible/", station_name)
  else fname <- paste0("Dataset/hydro_series/compatible/", station_name)
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
    year = hydro_years,
    los = station_los, 
    appo_m = rep(mark, length(station_los))
  ))
}





# Importing station names
appo <- as.matrix(read.table("Dataset/usable_modis.dat", header = FALSE))
station_names <- appo[, 1]
mark <- appo[, 6]
rm(appo)
gc()


# Actually evaluating longest sc period
results <- data.frame()
for(i in seq_along(station_names)){
  appo <- find_los_station(station_names[i], mark[i])
  results <- rbind(results, appo)
}

write.table(results, "Results/los.dat", row.names = FALSE, col.names = FALSE, quote = FALSE)