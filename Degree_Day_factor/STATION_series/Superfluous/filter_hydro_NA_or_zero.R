# The main goal of this script is to filter out those years which contain only NAs
# or zeros, in order to get rid of some faulty csc periods previously determined.
rm(list = ls())
gc()

st_fname <- "Datas/results/raw/usable_stations_raw.dat"
setwd("/home/filippo/Desktop/Codicini/Master_Thesis/Degree_Day_factor/STATION_series/")


# Importing station names
station_names <- as.matrix(read.table(st_fname)$V1)
usable_names <- character(0)
all_dataset <- 0L
conta <- 0L

for(name in station_names){
  
  # Importing snow heigth datas
  df <- read.table(paste0("Datas/station_series/raw/", name))
  filtered_years <- numeric(0)
  filtered_hs <- numeric(0)
  years <- unique(df$V1)
  
  # Cycle on hydrological years to check whether they pass or not
  for(year in years){
    
    # Selecting specific hydrological year
    mask <- df$V1 == year
    appo <- df[mask, ]
    
    # Actual logic test
    mask <- is.na(as.numeric(appo$V2)) | as.numeric(appo$V2) == 0
    if(sum(mask, na.rm = TRUE) != length(as.numeric(appo$V2))){
      filtered_years <- c(filtered_years, as.numeric(appo$V1))
      filtered_hs <- c(filtered_hs, as.numeric(appo$V2))
    }
    else{
      conta <- conta + 1
    }
  }
  
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
    fname <- paste0("Datas/station_series/na_or_zero_filter/", name)
    write.table(df_print, fname, row.names = FALSE, col.names = FALSE, quote = FALSE)
    usable_names <- c(usable_names, name)
    
    print(paste0("Filtered ", name, " station datas!"))
  }
  
  else{
    print(paste0("No valid years for station ", name))  
  }
  
  all_dataset <- all_dataset + length(years)
}


# Saving usable stations
df_names <- data.frame(
  names = usable_names
)

print(paste0(conta, " on ", all_dataset, " pass the test!"))
write.table(df_names, "Datas/station_series/na_or_zero_filter/usable_stations_na_or_zero.dat", row.names = FALSE, col.names = FALSE, quote = FALSE)