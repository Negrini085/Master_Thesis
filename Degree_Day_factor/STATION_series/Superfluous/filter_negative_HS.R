# The main goal of this script is to find negative hs values and correct them, 
# replacing them with zeros
rm(list = ls())
gc()

st_fname <- "Datas/results/raw/usable_stations_raw.dat"
setwd("/home/filippo/Desktop/Codicini/Master_Thesis/Degree_Day_factor/STATION_series/")


# Importing station names
station_names <- as.matrix(read.table(st_fname)$V1)
usable_names <- character(0)

for(name in station_names){
  
  # Importing snow heigth datas
  df <- read.table(paste0("Datas/station_series/raw/", name))
  filtered_years <- as.numeric(df$V1)
  filtered_hs <- as.numeric(df$V2)
  
  # Selecting negative hs values
  mask <- filtered_hs < 0
  filtered_hs[mask] <- 0
  
  
  if(length(filtered_hs) != length(filtered_years)){
    print("Vectors don't have compatible lenghts!")
    break
  }
  
  else if(length(filtered_hs) != 0){
    
    # Creating dataset
    df_print <- data.frame(
      years = filtered_years, 
      hs = filtered_hs
    )
    
    # Saving to file
    fname <- paste0("Datas/station_series/no_negative_hs/", name)
    write.table(df_print, fname, row.names = FALSE, col.names = FALSE, quote = FALSE)
    usable_names <- c(usable_names, name)
    
    print(paste0("Filtered ", name, " station datas!"))
  }
  
  else{
    print(paste0("No valid years for station ", name))  
  }
}


# Saving usable stations
df_names <- data.frame(
  names = usable_names
)

write.table(df_names, "Datas/station_series/no_negative_hs/usable_stations_no_negative_hs.dat", row.names = FALSE, col.names = FALSE, quote = FALSE)