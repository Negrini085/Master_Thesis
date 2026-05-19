# The main goal of this script is to find the first and last hydrological year 
# of measures for a given station
rm(list = ls())
gc()

fname <- "Dataset/station_series/usable_stations.dat"
setwd("/home/filippo/Desktop/Codicini/Master_Thesis/HS_series/Original/STATION_series/")


# Importing station names
df <- read.table(fname, header = FALSE)
station_names <- sort(df$V1)
first_y <- numeric(0)
last_y <- numeric(0)


# Cycle across stations
for(name in station_names){
  
  # Importing HS series
  fname <- paste0("Dataset/station_series/", name)
  df_hs <- read.table(fname)
  
  # Finding first and last hydrological years
  first_y <- c(first_y, min(as.numeric(df_hs$V1), na.rm = TRUE))
  last_y <- c(last_y, max(as.numeric(df_hs$V1), na.rm = TRUE))
}


# Data quality
if(any(is.na(first_y))) stop("At least one of the first years is NA")
if(any(is.na(last_y))) stop("At least one of the last years is NA")
if(length(station_names) != length(first_y)) stop("No compatible length")
if(length(first_y) != length(last_y)) stop("No compatible length")


# Saving dataset
df_print <- data.frame(name = station_names, first = first_y, last = last_y)
write.table(df_print, "Results/time_window_original.dat", row.names = FALSE, col.names = TRUE, quote = FALSE)