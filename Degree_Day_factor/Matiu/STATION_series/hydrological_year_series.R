# The main goal of this script is to convert station series from conventional
# format (year starting on the 1st of January and ending on the 31th of December) 
# to hydrological years. I will consider as an hydrological year the period stretching
# from the 1st of September to the 31st of August in order to later focus my search 
# on single years. I will also check whether some years are missing, in order to 
# lighten the dataset.
rm(list = ls())
gc()

fname <- "../MODIS_series/Dataset/usable_modis.dat"
setwd("/home/filippo/Desktop/Codicini/Master_Thesis/Degree_Day_factor/Matiu/STATION_series/")


# Function to switch from years to hydrological years
normal_to_hydro <- function(station_name, mark){
  
  # Importing snow height series for a specific station
  fname <- paste0("../Dataset/data/", station_name)
  df_hs <- read.table(fname, header = FALSE)
  
  
  # Finding first and last year
  min_year <- min(as.numeric(df_hs$V1))
  max_year <- max(as.numeric(df_hs$V1))
  
  
  # The station is operational on the whole period (min_year, max_year)
  years <- unique(df_hs$V1)
  if(length(years) != (max_year + 1 - min_year)){
    # print(paste0("Not whole period covered for station", station_name))
    return(0)
  }
  
  
  # In order to convert to hydrological year format I need more than one year (so
  # I have to discard those stations which only cover a given year)
  if(max_year == min_year){
    # print(paste0("HS series one year long for ", station_name, " station."))
    return(0)
  }
  
  
  # Omitting January to August of the start year and September to December of the 
  # last one, because those datas can't fill a whole hydrological year.
  start_ind <- 244
  if(min_year %% 4 == 0) start_ind <- start_ind + 1
  
  end_ind <- 122
  hs_series <- as.numeric(df_hs$V2)[start_ind:(length(df_hs$V2) - end_ind)]
  
  
  # Checking if hs_series is as long as I would expect or if there are some missing
  # datas
  conta <- 0
  appo_years <- numeric(0)
  for(year in years[2:length(years)]){
    
    # Setting year length
    len <- 365
    if(year %% 4 == 0){ len <- len + 1}
    
    # Evaluating the number of days I would expect to be covered in hs_series
    conta <- conta + len
    appo_years <- c(appo_years, rep(year, len))
  }
  
  if(conta != length(hs_series)){
    print(paste0("Some HS datas are missing for station ", station_name))
    return(0)
  }
  
  
  # Saving to file
  df_print <- data.frame(
    year = appo_years, 
    hs = hs_series
  )
  
  if(mark == "NO") fout <- paste0("Dataset/hydro_hs_series/non_compatible/", station_name)
  else fout <- paste0("Dataset/hydro_hs_series/compatible/", station_name)
  
  write.table(df_print, fout, row.names = FALSE, col.names = FALSE, quote = FALSE)
  return(1)
}


# Importing station names and operation period
df <- read.table(fname, header = FALSE)
station_names <- df$V1
mark <- df$V6


# Cycle on stations
appo_mark <- character(0)
appo_names <- character(0)
for(i in seq_len(length(station_names))){
  appo <- normal_to_hydro(station_names[i], mark[i])
  if(appo == 1){
    appo_names <- c(appo_names, station_names[i])
    appo_mark <- c(appo_mark, mark[i])
  }
}

df_usable <- data.frame(
  names = appo_names, 
  mark = appo_mark
)
write.table(df_usable, "Dataset/hydro_hs_series/usable_stations.dat", row.names = FALSE, col.names = FALSE, quote = FALSE)