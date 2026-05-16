# The main goal of this script is to compute the longest period of snow coverage
# during a given hydrological year. This is the final step that I need to take in 
# order to be able to compute the DDF
rm(list = ls())
gc()

fname_usable <- "Dataset/USABLE_years.dat"
setwd("/home/filippo/Desktop/Codicini/Master_Thesis/Degree_Day_factor/SWE_series/DDF_computation/")


# Function to find the longest period of snow coverage across a hydrological year. 
# The output is a vector of size 2, to store the duration of continuous snow cover
# and when does it start
find_longest_sc_period_hydro <- function(hydro_sc, year, name){
  
  # Variables to store snow metrics
  max_sc <- numeric(0)
  start_sc <- numeric(0)
  
  # Checking if there is at least a snow covered day
  mask <- hydro_sc == 1 & !is.na(hydro_sc)
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


# Importing stations and hydrological years ready for DDF computation
df <- read.table(fname_usable)
station_names <- df$V1
station_years <- as.numeric(df$V2)


# Cycle across station names
appo_csc <- numeric(0)
appo_year <- numeric(0)
appo_start <- numeric(0)
appo_name <- character(0)
for(name in unique(station_names)){
  
  # Importing sc series evaluated from snow height measurements
  fname_sc <- paste0("../../Ours/STATION_series/Dataset/sc_series/", name)
  df_sc <- read.table(fname_sc)
  
  sc_years <- as.numeric(df_sc$V1)
  sc <- as.numeric(df_sc$V2)
  
  for(y in station_years[station_names == name]){
    
    # Selecting snow coverage for a given year
    mask <- sc_years == y
    sc_hydro <- sc[mask]
    
    # Actual csc computation
    appo <- find_longest_sc_period_hydro(hydro_sc = sc_hydro, year = y, name = name)
    appo_start <- c(appo_start, appo[2])
    appo_csc <- c(appo_csc, appo[1])
    appo_name <- c(appo_name, name)
    appo_year <- c(appo_year, y)
  }
  
  print(paste0("Made CSC computation for ", name))
}


# Saving to file
df_print <- data.frame(
  name = appo_name, 
  year = appo_year, 
  csc = appo_csc, 
  start = appo_start
)

write.table(df_print, "Results/csc.dat", row.names = FALSE, col.names = FALSE, quote = FALSE)