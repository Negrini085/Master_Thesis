# The main goal of this script is to convert station series from snow height to 
# snow cover. I will apply a filter on snow height, so that every day with more
# than two centimeters of snow cover will be labeled as snow covered, whilst the
# others will be taken as snow free.
rm(list = ls())
gc()

fname <- "Datas/usable_stations.dat"
setwd("/home/filippo/Desktop/Codicini/Master_Thesis/Degree_Day_factor/STATION_series/")

# Importing station names
station_names <- as.matrix(read.table(fname)$V1)

# Cycle on usable stations
for(name in station_names){
  
  # Importing station series (already on hydrological years)
  fname <- paste0("Datas/station_series/", name)
  df <- read.table(fname, header = FALSE, sep = "")
  
  years <- as.numeric(df$V1)
  hs_series <- as.numeric(df$V2)
  
  rm(df)
  gc()
  
  # Filters to get actual snow coverage datas. Every day with more than two cm of
  # snow will be labeled as snow covered, NAs will be kept NAs and all the other 
  # days will be considered as snow free.
  mask <- !is.na(hs_series) & hs_series < 2
  hs_series[mask] <- 0
  
  mask <- !is.na(hs_series) & hs_series >= 2
  hs_series[mask] <- 1 
  
  
  # Creating dataframe, ready to save to file
  df <- data.frame(
    years = years, 
    sc_series = hs_series
  )
  
  
  # Saving to file
  write.table(df, paste0("Datas/sc_series/", name), row.names = FALSE, col.names = FALSE, quote = FALSE)
  print(paste0("Made conversion for: ", name))
}