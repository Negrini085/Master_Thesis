# The main goal of this script is to evauate whether all station files contain whole
# years or if some has no annual duration for snow height datas
rm(list = ls())
gc()

setwd("/home/filippo/Desktop/Codicini/Master_Thesis/Degree_Day_factor")
fname <- "Dataset/ok_stations_dem_30.dat"

# Function to check if files contain whole years or not
check_whole_years <- function(fname){
  
  # Loading whole file in order to later select the first column of the dataset, 
  # namely the one containing years
  df <- read.table(fname, header = FALSE, sep = "", fill = TRUE)
  years <- as.numeric(df[[1]])
  
  # Evaluating the number of times a different item pops up in years array. We than
  # want to drop names to check just on data frequency.
  appo <- table(years)
  names(appo) <- NULL
  
  # Finally evaluating whether every year appears in 12 different rows or not
  whole_log <- all(appo == 12)
  return(whole_log)
}



# Importing stations whose elevation is comparable to the one of a DEM with
# resolution of 30 meters. We will select the name of one station and then we
# will discard the rest.
appo <- read.table(fname, header = TRUE, fill = TRUE)
station_name <- appo[[1]][148]

print(paste0("The following analysis will be on ", station_name, " which is a station"))
print(paste0("sitting at ", appo[[4]][148], " m asl.   Coordinates: ", appo[[2]][148], " E, ", appo[[3]][148], " N"))



fname <- paste0("Dataset/HSD_", station_name)
print(check_whole_years(fname))