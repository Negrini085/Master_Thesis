# The main goal of this script is to convert station series from conventional
# format (year starting on the 1st of January and ending on the 31th of December) 
# to hydrological years. I will consider as an hydrological year the period stretching
# from the 1st of September to the 31st of August in order to later focus my search 
# on single years.
rm(list = ls())
gc()

setwd("/home/filippo/Desktop/Codicini/Master_Thesis/Degree_Day_factor/MODIS_series/")

# Function to switch from years to hydrological years
normal_to_hydro <- function(station_name, start_year, end_year){
  
  # Importing MODIS series for a given station
  fname <- paste0("Datas/station_series/", station_name)
  sc_series <- read.table(fname, header = FALSE)$V2
  
  # Selecting the start of the sequence
  begin <- 244
  end <- length(sc_series)-122
  if(start_year == 2000) begin <- 191
  else if(start_year %% 4 == 0) begin <- begin + 1
  
  # Checking if I have at least two years in order to extract at least one hydrological year
  if(start_year == end_year){
    print(paste0("Not enough years: ", station_name))
    return(NULL)
  }
  
  if(end < begin){
    print(paste0("Series too short: ", station_name))
    return(NULL)
  }
  
  # Assigning to every data the hydrological year to which it belongs
  h_len <- 365
  h_years <- numeric(0)
  for(y in (start_year+1):end_year){
    if(y %% 4 == 0) h_len <- 366
    
    h_years <- c(h_years, array(y, dim = c(h_len)))
    h_len <- 365
  }
  
  sc_series <- sc_series[begin:end]
  
  if(length(sc_series) != length(h_years)){
    print("Incompatible vector lengths!")
    return(NULL)
  }
  
  # Creating data-frame to save values
  df <- data.frame(
    years = h_years,
    sc = sc_series
  )
  
  write.table(df, paste0("Datas/station_hydrological/", station_name) , row.names = FALSE, col.names = FALSE, quote = FALSE)
}


# Importing Italian AWS names, coordinates, start and end year to correctly deal
# with MODIS series (should be converted to hydrological years)
appo <- as.matrix(read.table("Datas/start_end_years_filtered.dat", header = FALSE))
start_year <- as.numeric(appo[, 4])
end_year <- as.numeric(appo[, 5])
station_names <- appo[, 1]
rm(appo)
gc()


# Correcting start year to preserve compatibility 
mask <- start_year < 2000
start_year[mask] <- 2000


# Actually converting to hydrological years
for(i in seq_along(station_names)){
  normal_to_hydro(station_names[i], start_year[i], end_year[i])
}