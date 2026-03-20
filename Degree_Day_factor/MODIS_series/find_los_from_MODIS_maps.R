# The main goal of this script is to compare modis series los with modis actual 
# values. To be fair, this isn't a perfectly fine comparison because MODIS uses 
# 1st October --> 30th September as an hydrological year, whilst I worked with 
# 1st September --> 31th August. Considering that, I don't expect a perfect match, 
# but not even a big dispersion from the y = x diagonal if working with a scatter.
rm(list = ls())
gc()

library(terra)

setwd("/home/filippo/Desktop/Codicini/Master_Thesis/Degree_Day_factor/MODIS_series/")

# Function to extract station los series from MODIS los maps
extract_los_series <- function(station_name, station_lon, station_lat, start_year, end_year){
  
  los_series <- numeric(0)
  if(start_year <= 2000){ start_year <- 2001}
  
  years <- start_year:end_year
  for(y in years){
    
    # Importing annual los map
    fname <- paste0("../../SnowCover_studies/MODIS/Dataset/annual_maps/LOS/los_", y, ".tif")
    annual_map <- rast(fname)
      
    # Selecting pixel
    ind <- cellFromXY(annual_map, cbind(station_lon, station_lat))
    appo <- as.numeric(annual_map[ind])
      
    # Elongating series
    if(is.na(appo)){ appo <- -90.0}
    los_series <- c(los_series, appo)
    rm(annual_map)
    gc()
  }
  
  print(paste0("Taken care of ", station_name, " LOS series!"))
  return(los_series)
}



# Reading italian stations dataset
appo <- as.matrix(read.table("Datas/non_compatible/start_end_years_filtered.dat", header = FALSE))
coord_ele <- matrix(as.numeric(appo[, 2:3]), ncol = 2)
start_year <- as.numeric(appo[, 4])
end_year <- as.numeric(appo[, 5])
station_names <- appo[, 1]
rm(appo)
gc()



# Cycle over stations
all_los_series <- numeric(0)
all_name_series <- character(0)

for(i in seq_len(length(station_names))){
  
  # Evaluating start position and snow cover series
  appo <- extract_los_series(station_names[i], coord_ele[i, 1], coord_ele[i, 2], start_year[i], end_year[i])
  
  # Saving single station series
  all_los_series <- c(all_los_series, appo)
  all_name_series <- c(all_name_series, rep(station_names[i], length(appo)))
}


# Saving station series
df_los <- data.frame(
  name_series = all_name_series, 
  los_series = all_los_series
)
write.table(df_los, "Datas/non_compatible/los_from_modis_maps.dat", row.names = FALSE, col.names = FALSE, quote = FALSE)