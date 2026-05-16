# The main goal of this script is to test study pixel function
rm(list = ls())
gc()


setwd("/home/filippo/Desktop/Codicini/Master_Thesis/Degree_Day_factor/MODIS_series/")


# Function to find the longest period of snow coverage across a hydrological year. 
study_pixel <- function(station_name, year, hydro_sc){
  if(hydro_sc[1] == 1){
    print(paste0("First day of ", year, " hydrological year is snow covered for station ", station_name))
  }
}



# Function to find the longest period of snow coverage for a given station across
# the whole investigated period. 
find_longest_sc_period_station <- function(station_name){
  
  # Importing snow cover series for a given station
  fname <- paste0(station_name)
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
    study_pixel(station_name, year, hydro_sc)
  }
}


# First test
print("First check!")
find_longest_sc_period_station("Superfluous/Files/test_fday.dat")