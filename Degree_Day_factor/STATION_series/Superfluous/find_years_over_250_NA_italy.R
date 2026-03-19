# The main goal of this script is to find all those hydrological years with more 
# than 250 NAs, but only for italian stations.
rm(list = ls())
gc()

library(ggplot2)

fname_italian <- "../MODIS_series/Datas/ITALIAN_STATIONS"
fname_usable <- "../STATION_series/Datas/station_series/na_or_zero_filter/usable_stations_na_or_zero.dat"
setwd("/home/filippo/Desktop/Codicini/Master_Thesis/Degree_Day_factor/STATION_series/")


# Importing names of italian stations and names of usable stations
df <- read.table(fname_italian, header = FALSE)
name_italian <- df$V1

df <- read.table(fname_usable, header = FALSE)
name_usable <- df$V1


# Finding common names
common_names <- intersect(name_italian, name_usable)


# Cycle over stations
faulty_names <- character(0)
faulty_years <- numeric(0)
for(name in common_names){
  
  # Importing series
  fname <- paste0("../STATION_series/Datas/station_series/na_or_zero_filter/", name)
  df <- read.table(fname, header = FALSE)
  hs_series <- as.numeric(df$V2)
  years <- as.numeric(df$V1)
  
  
  # Cycle over years
  for(year in unique(years)){
    
    # Mask over years
    mask <- years == year
    hs_appo <- hs_series[mask]
    
    # Finding out if there are more than 250 NA days
    if(sum(is.na(hs_appo), na.rm = TRUE) > 250){
      faulty_names <- c(faulty_names, name)
      faulty_years <- c(faulty_years, year)
    }
  }  
}


# Saving to file
df <- data.frame(
  station = faulty_names, 
  years = faulty_years
)

write.table(df, "Datas/results/over_250_NAs.dat", , row.names = FALSE, col.names = FALSE, quote = FALSE)