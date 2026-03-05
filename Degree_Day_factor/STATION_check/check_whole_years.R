# The main goal of this script is to evauate whether all station files contain whole
# years or if some has no annual duration for snow height datas
rm(list = ls())
gc()

setwd("/home/filippo/Desktop/Codicini/Master_Thesis/Degree_Day_factor")
fname <- "Dataset/ok_stations_dem_30.dat"



# Function to check if files contain whole years or not
check_whole_years <- function(fname){
  
  # Check on file existence
  if(!file.exists(fname)){
    print(paste0("File ", fname, " does not exists! Returning false by default"))
    return(FALSE)
  }
  
  # Loading whole file in order to later select the first column of the dataset, 
  # namely the one containing years
  df <- read.table(fname, header = FALSE, sep = "", fill = TRUE)
  years <- as.numeric(df[[1]])
  
  # Checking for possible NAs
  if (anyNA(years)) {
    warning("Found NA years in file: ", fname)
    return(FALSE)
  }
  
  # Evaluating the number of times a different item pops up in years array. We than
  # want to drop names to check just on data frequency.
  appo <- table(years)
  names(appo) <- NULL
  
  # Finally evaluating whether every year appears in 12 different rows or not
  whole_log <- all(appo == 12)
  rm(df, years, appo)
  gc()
  
  return(whole_log)
}



# Importing stations whose elevation is comparable to the one of a DEM with
# resolution of 30 meters. We want to check whether all of these files contain
# whole years or some have only some months.
appo <- read.table(fname, header = TRUE, fill = TRUE)
station_names <- appo[[1]]

count_false <- 0
for(name in station_names){
  fname_stat <- paste0("Dataset/HSD_", name)
  print(paste0("Considering ", name, " station"))
  
  # Checking year completeness or not
  if(!check_whole_years(fname_stat)){
    count_false <- count_false + 1
  }
}

print(paste0("The number of false checks was: ", count_false))