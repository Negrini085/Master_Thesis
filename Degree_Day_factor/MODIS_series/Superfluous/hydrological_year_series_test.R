# The main goal of this script is to test normal_to_hydro function
rm(list = ls())
gc()

setwd("/home/filippo/Desktop/Codicini/Master_Thesis/Degree_Day_factor/MODIS_series/")

# Function to switch from years to hydrological years
normal_to_hydro <- function(station_name, start_year, end_year){
  
  # Importing MODIS series for a given station
  fname <- paste0("Superfluous/Files/", station_name)
  sc_series <- read.table(fname, header = FALSE)$V2
  
  # Selecting the start of the sequence
  begin <- 9
  if(start_year == 2000) begin <- 191
  else if(start_year %% 4 == 0) begin <- begin + 1
  
  # Checking if I have at least two years in order to extract at least one hydrological year
  if(start_year == end_year){
    print(paste0("Not enough years: ", station_name))
    return(NULL)
  }
  
  # Assigning to every data the hydrological year to which it belongs
  h_len <- 12
  h_years <- numeric(0)
  for(y in (start_year+1):end_year){
    if(y %% 4 == 0) h_len <- 13
    
    h_years <- c(h_years, array(y, dim = c(h_len)))
    h_len <- 12
  }
  
  end <- length(sc_series)-4
  sc_series <- sc_series[begin:end]
  
  if(length(sc_series) < 12){
    print(paste0("Not enough datas: ", station_name))
    return(NULL)
  }
  
  if(length(sc_series) != length(h_years)){
    print(sc_series)
    print(length(sc_series))
    print(length(h_years))
    print("Incompatible vector lengths!")
    return(NULL)
  }
  
  # Creating data-frame to save values
  df <- data.frame(
    years = h_years,
    sc = sc_series
  )
  
  write.table(df, paste0("Datas/station_hydrological/", station_name), , row.names = FALSE, col.names = FALSE, quote = FALSE)
}


# First test
print("First check, look into Datas/station_hydrological folder!")
normal_to_hydro("test_1.dat", 2002, 2005)


# Second test
print("Second check, look into Datas/station_hydrological folder!")
normal_to_hydro("test_2.dat", 2002, 2002)
normal_to_hydro("test_2.dat", 2002, 2005)