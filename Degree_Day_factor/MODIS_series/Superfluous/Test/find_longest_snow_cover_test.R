# The main goal of this script is to test the procedure to find longest period
# of snow cover for a given station and a given hydrological year
rm(list = ls())
gc()

setwd("/home/filippo/Desktop/Codicini/Master_Thesis/Degree_Day_factor/MODIS_series/")


# Function to find the longest period of snow coverage across a hydrological year. 
# The output is a vector of size 2, to store the duration of continuous snow cover
# and when does it start
find_longest_sc_period_hydro <- function(hydro_sc){
  
  # Variables to store snow metrics
  max_sc <- numeric(0)
  start_sc <- numeric(0)
  
  # Checking if there is at least a snow covered day
  mask <- hydro_sc == 1
  if(sum(mask, na.rm = TRUE) == 0){
    max_sc <- 0
    start_sc <- NA
  } else {
    
    # Using rle() to find continuous sequences of numbers
    r <- rle(hydro_sc)
    
    # Finding the longest sequence and where does it start
    max_sc <- max(r$lengths[r$values == 1], na.rm = TRUE)
    sorted_sc <- sort(r$lengths[r$values == 1])
    
    if(max_sc == sorted_sc[length(sorted_sc) - 1]){
      print("Two period with same duration, selecting the first one!")
    }
    
    ends   <- cumsum(r$lengths)
    starts <- ends - r$lengths + 1
    idx_sc <- which(r$values == 1 & r$lengths == max_sc)
    start_sc <- starts[idx_sc]
  }
  
  return(c(max_sc, start_sc))
}



# Function to find the longest period of snow coverage for a given station across
# the whole investigated period. 
find_longest_sc_period_station <- function(station_name){
  
  # Importing snow cover series for a given station
  fname <- station_name
  df <- read.table(fname, header = FALSE)
  appo_years <- df$V1
  sc_series <- df$V2
  
  # Evaluating which hydrological year are taken into account for a given station
  hydro_years <- sort(unique(appo_years))
  
  
  # Creating variables to contain snow cover metrics (duration, start, end)
  sc_start <- numeric(0)
  sc_duration <- numeric(0)
  
  
  # Cycle to compute snow cover metrics across the whole investigated period
  for(year in hydro_years){
    mask <- appo_years == year
    hydro_sc <- sc_series[mask]
    
    # Actual computation
    appo <- find_longest_sc_period_hydro(hydro_sc = hydro_sc)
    
    # Saving snow cover metrics
    sc_duration <- c(sc_duration, appo[1])
    sc_start <- c(sc_start, appo[2])
  }
  
  if(length(hydro_years) == length(sc_duration)){
    print("We are in the gucci!")
  }
  
  return(data.frame(
    station  = station_name,
    year     = hydro_years,
    duration = sc_duration,
    start    = sc_start
  ))
}


# First test
appo <- find_longest_sc_period_station("Superfluous/Files/test_sc1.dat")
print("First test made!")
print(appo)
