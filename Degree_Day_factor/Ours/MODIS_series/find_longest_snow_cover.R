# The main goal of this script is to find the longest period of continuous snow
# coverage at a specific location (namely the station one) for every hydrological
# year taken into account. The output of this script is a file with four columns, 
# the first one being the station name, the second one being the duration of 
# continuous snow coverage. The last two columns will help us to locate this 
# period into the hydrological year.
rm(list = ls())
gc()

setwd("/home/filippo/Desktop/Codicini/Master_Thesis/Degree_Day_factor/MODIS_series/")


# Function to find the longest period of snow coverage across a hydrological year. 
# The output is a vector of size 2, to store the duration of continuous snow cover
# and when does it start
find_longest_sc_period_hydro <- function(hydro_sc, year, name){
  
  # Variables to store snow metrics
  max_sc <- numeric(0)
  start_sc <- numeric(0)
  
  # Checking if there is at least a snow covered day
  mask <- hydro_sc == 1
  if(sum(mask, na.rm = TRUE) == 0){
    print(paste0("No snow cover days for ", year, "! Station: ", name))
    max_sc <- 0
    start_sc <- NA
  } else {
    
    # Using rle() to find continuous sequences of numbers
    r <- rle(hydro_sc)
    
    # Finding the longest sequence and where does it start
    max_sc <- max(r$lengths[r$values == 1], na.rm = TRUE)
    sorted_sc <- sort(r$lengths[r$values == 1])
    
    if(length(sorted_sc) >= 2 && max_sc == sorted_sc[length(sorted_sc) - 1]){
      print(paste0("Two period with same duration, selecting the first one for ", year, "! Station: ", name))
    }
    
    ends   <- cumsum(r$lengths)
    starts <- ends - r$lengths + 1
    idx_sc <- which(r$values == 1 & r$lengths == max_sc)[1]
    start_sc <- starts[idx_sc]
  }
  
  return(c(max_sc, start_sc))
}



# Function to find the longest period of snow coverage for a given station across
# the whole investigated period. 
find_longest_sc_period_station <- function(station_name){
  
  # Importing snow cover series for a given station
  fname <- paste0("Datas/modis_hydrological/non_compatible/", station_name)
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
    appo <- find_longest_sc_period_hydro(hydro_sc = hydro_sc, year = year, name = station_name)
    
    # Saving snow cover metrics
    sc_duration <- c(sc_duration, appo[1])
    sc_start <- c(sc_start, appo[2])
  }
  
  return(data.frame(
    station  = station_name,
    year     = hydro_years,
    duration = sc_duration,
    start    = sc_start
  ))
}


# Importing station names
appo <- as.matrix(read.table("Datas/non_compatible/start_end_years_filtered.dat", header = FALSE))
station_names <- appo[, 1]
rm(appo)
gc()


# Actually evaluating longest sc period
results <- data.frame()
for(name in station_names){
  appo <- find_longest_sc_period_station(station_name = name)
  results <- rbind(results, appo)
}

write.table(results, "Datas/non_compatible/longest_periods_sc.dat", row.names = FALSE, quote = FALSE)