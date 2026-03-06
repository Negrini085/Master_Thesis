# The main goal of this script is to check if there are some hydrological years
# in my dataset which start with a snow-covered day (I honestrly hope that this
# doesn't happen)
rm(list = ls())
gc()


setwd("/home/filippo/Desktop/Codicini/Master_Thesis/Degree_Day_factor/MODIS_series/")


# Function to find the longest period of snow coverage across a hydrological year. 
study_pixel <- function(station_name, year, hydro_sc, ele){
  if(hydro_sc[1] == 1 & ele > 2500){
    print(paste0("First day of ", year, " hydrological year is snow covered for station ", station_name))
  }
}



# Function to find the longest period of snow coverage for a given station across
# the whole investigated period. 
find_longest_sc_period_station <- function(station_name, ele){
  
  # Importing snow cover series for a given station
  fname <- paste0("Datas/modis_hydrological/", station_name)
  df <- read.table(fname, header = FALSE)
  appo_years <- df$V1
  sc_series <- df$V2
  
  # Evaluating which hydrological year are taken into account for a given station
  hydro_years <- sort(unique(appo_years))
  
  
  # Creating variables to contain snow cover metrics (duration, start, end)
  sc_end <- numeric(0)
  sc_start <- numeric(0)
  sc_duration <- numeric(0)
  
  
  # Cycle to compute snow cover metrics across the whole investigated period
  for(year in hydro_years){
    mask <- appo_years == year
    hydro_sc <- sc_series[mask]
    
    # Actual computation
    study_pixel(station_name, year, hydro_sc, ele)
  }
}



# Importing Italian AWS names, coordinates, start and end year to correctly deal
# with MODIS series. I also need station altitude in order to check if snow coverage 
# on the 1st of September is clearly a bug or not.
appo <- as.matrix(read.table("Datas/start_end_years_filtered.dat", header = FALSE))
station_names <- appo[, 1]
rm(appo)
gc()

appo <- as.matrix(read.table("Datas/quality_check_filtered.dat", header = FALSE))
station_names_for_ele <- appo[, 1]
station_ele <- appo[, 4]
rm(appo)
gc()


# Making the actual test
for(name in station_names){
  
  # Finding correct station elevation
  mask <- station_names_for_ele == name
  ele <- station_ele[mask]
  
  # Testing start of hydrological year for a given station
  find_longest_sc_period_station(name, ele)
}