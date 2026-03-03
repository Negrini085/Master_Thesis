# The main goal of this script is to extract from MODIS dataset station series. I
# would like to identify which pixel hosts a given aws and then evaluate whether
# there is snow coverage or not.
rm(list = ls())
gc()

library(terra)

setwd("/home/filippo/Desktop/Codicini/Master_Thesis/Degree_Day_factor/MODIS_series/")

# Function to extract station snow coverage series from MODIS dataset
snow_series <- function(station_lon, station_lat, start_year, end_year){

  sc_series <- numeric(0)
  if(start_year < 2000){ start_year <- 2000}
  
  years <- start_year:end_year
  for(y in years){
    
    # Evaluating number of files
    ndays <- length(list.files(paste0("../../SnowCover_studies/MODIS/Dataset/daily/", y), pattern = "\\.tif$"))
    for(i in seq_len(ndays)){
      
      # Importing daily map
      fname <- paste0("../../SnowCover_studies/MODIS/Dataset/daily/", y, "/day_", sprintf("%03d", i), ".tif")
      print(paste0("Dealing with day ", sprintf("%03d", i), " of ", ndays, " for year ", y, "!"))
      daily_map <- rast(fname)
      
      # Selecting pixel
      ind <- cellFromXY(daily_map, cbind(station_lon, station_lat))
      appo <- as.numeric(daily_map[ind])
      
      # Elongating series
      if(is.na(appo)){ appo <- -90.0}
      sc_series <- c(sc_series, appo)
      rm(daily_map)
      gc()
    }
  }

  return(sc_series)
}


# Function to compute number of maps
num_maps <- function(){
  
  ntot <- 0
  years <- 2000:2025
  for(y in years){
    
    # Evaluating number of files
    ndays <- length(list.files(paste0("../../SnowCover_studies/MODIS/Dataset/daily/", y), pattern = "\\.tif$"))
    ntot <- ntot + ndays
  }
  
  return(ntot)
}


# Function to compute the start of MODIS series in result matrix
start_res <- function(start_year){
  start_pos <- 1
  
  # If measurements start before the year 2000, I will fill the column 
  # right from the start
  if(start_year <= 2000){ 
    return(start_pos)
    }
  
  # Here I actually have to compute start position. 312 is the number of MODIS 
  # daily maps available for year 2000
  start_pos <- 312
  
  if(start_year == 2001){
    return(start_pos+1)
  }
  
  fill_year <- 2001:(start_year-1)
  for(y in fill_year){
    if(y %% 4 == 0){ start_pos <- start_pos + 366}
    else{ start_pos <- start_pos + 365}
  }
  
  return(start_pos+1)
}



# Reading italian stations dataset
appo <- as.matrix(read.table("Datas/start_end_years_filtered.dat", header = FALSE))
coord_ele <- matrix(as.numeric(appo[, 2:3]), ncol = 2)
start_year <- as.numeric(appo[, 4])
end_year <- as.numeric(appo[, 5])
station_names <- appo[, 1]
rm(appo)
gc()


# Cycle over stations
ntot <- num_maps()
nstat <- length(station_names)
station_series <- array(NA, dim = c(ntot, nstat))

# Cycle over stations
for(i in seq_len(length(station_names))){
  
  # Evaluating start position and snow cover series
  print(station_names[i])
  start_pos <- start_res(start_year[i]) 
  appo <- snow_series(coord_ele[i, 1], coord_ele[i, 2], start_year[i], end_year[i])
  
  # Saving single station series
  df <- data.frame(days = seq_len(length(appo)), sc_series = appo)
  write.table(df, paste0("Datas/station_series/", station_names[i]))
  
  # Saving into massive container
  station_series[start_pos:(start_pos + length(appo)-1), i] <- appo
  rm(appo)
  gc()
}


# Saving station series
df_out <- as.data.frame(station_series)
colnames(df_out) <- station_names
write.table(df_out, "Datas/station_series_all.dat", row.names = FALSE, col.names = TRUE, quote = FALSE, sep = "\t")