# The main goal of this script is to test find_annual_los procedure
rm(list = ls())
gc()

setwd("/home/filippo/Desktop/Codicini/Master_Thesis/Degree_Day_factor/MODIS_series/")


# Function to find LOS duration for a given hydrological year
find_los_hydro <- function(hydro_sc, year, name){
  
  # Mask to pixel values (I'm looking for one, namely snow cover)
  mask <- hydro_sc == 1
  hydro_los <- length(hydro_sc[mask])
  
  # Checking if there is at least a snow covered day
  if(hydro_los == 0){
    print(paste0("No snow cover days for ", year, "! Station: ", name))
  }
  
  return(hydro_los)
}


# Function to find LOS duration for a given station
find_los_station <- function(station_name){
  
  # Importing snow cover series for a given station
  fname <- paste0(station_name)
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


# First test
appo <- find_los_station("Superfluous/Files/test_los.dat")
print("First test made!")
print(appo)