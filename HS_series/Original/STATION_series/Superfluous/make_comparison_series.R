# The main goal of this script is to create hs series with indexes to spot outlier
# easier than before. I will just create a sub-repo of na_or_zero, in order to 
# leave other procedures unfazed.
rm(list = ls())
gc()

fname_usable <- "Datas/station_series/na_or_zero_filter/usable_stations_na_or_zero.dat"
setwd("/home/filippo/Desktop/Codicini/Master_Thesis/Degree_Day_factor/STATION_series/")


# Importing usable station names
df <- read.table(fname_usable, header = FALSE)
station_names <- df$V1
rm(df)
gc()


# Cycle across stations
for(name in station_names){
  
  # Importing snow height series
  df <- read.table(paste0("Datas/station_series/na_or_zero_filter/", name), header = FALSE)
  hs_series <- as.numeric(df$V2)
  years <- as.numeric(df$V1)
  
  # Creating index
  index <- seq_len(length(years))
  
  # Creating data-frame
  df <- data.frame(
    index = index, 
    years = years, 
    hs_series = hs_series
  )
  
  # Saving to file
  write.table(df, paste0("Datas/station_series/na_or_zero_filter/to_inspect/", name), row.names = FALSE, col.names = FALSE, quote = FALSE)
}