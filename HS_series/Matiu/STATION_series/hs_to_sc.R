# The main goal of this script is to convert station series from snow height to 
# snow cover. I will apply a filter on snow height, so that every day with more
# than zero centimeters of snow cover will be labeled as snow covered, whilst the
# others will be taken as snow free.
rm(list = ls())
gc()

fname <- "Dataset/hydro_hs_series/usable_stations.dat"
setwd("/home/filippo/Desktop/Codicini/Master_Thesis/Degree_Day_factor/Matiu/STATION_series/")

# Importing station names
df <- read.table(fname, header = FALSE)
station_names <- df$V1
marks <- df$V2


# Cycle on usable stations
for(i in seq_along(station_names)){
  
  # Importing station series (already on hydrological years)
  if(marks[i] == "NO") fname <- paste0("Dataset/hydro_hs_series/non_compatible/", station_names[i])
  else fname <- paste0("Dataset/hydro_hs_series/compatible/", station_names[i])
  df <- read.table(fname, header = FALSE, sep = "")
  
  years <- as.numeric(df$V1)
  hs_series <- as.numeric(df$V2)
  
  rm(df)
  gc()
  
  # Filters to get actual snow coverage datas. Every day with more than zero cm of
  # snow will be labeled as snow covered, NAs will be kept NAs and all the other 
  # days will be considered as snow free.
  mask <- !is.na(hs_series) & hs_series <= 0
  hs_series[mask] <- 0
  
  mask <- !is.na(hs_series) & hs_series > 0
  hs_series[mask] <- 1 
  
  
  # Creating dataframe, ready to save to file
  df <- data.frame(
    years = years, 
    sc_series = hs_series
  )
  
  
  # Saving to file
  if(marks[i] == "NO") fout <- paste0("Dataset/hydro_sc_series/non_compatible/", station_names[i])
  else fout <- paste0("Dataset/hydro_sc_series/compatible/", station_names[i])
  
  write.table(df, fout, row.names = FALSE, col.names = FALSE, quote = FALSE)
  print(paste0("Made conversion for: ", station_names[i]))
}