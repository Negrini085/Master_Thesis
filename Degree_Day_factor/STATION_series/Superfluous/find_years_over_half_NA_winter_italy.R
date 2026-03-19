# The main goal of this script is to find those years which have more than 50% 
# datas missing across the winter period. I will consider as winter the period
# stretching from the first of december till the 31st of march
rm(list = ls())
gc()

fname_italian <- "../MODIS_series/Datas/ITALIAN_STATIONS"
fname_not_to_consider <- "Datas/results/over_250_NAs_italian_no_lomb.dat"
fname_usable <- "Datas/station_series/na_or_zero_filter/usable_stations_na_or_zero.dat"
setwd("/home/filippo/Desktop/Codicini/Master_Thesis/Degree_Day_factor/STATION_series/")


# Importing names of italian stations and names of usable stations
df <- read.table(fname_italian, header = FALSE)
name_italian <- df$V1

df <- read.table(fname_usable, header = FALSE)
name_usable <- df$V1


# Finding common names
common_names <- intersect(name_italian, name_usable)


# Importing datas to not consider
df <- read.table(fname_not_to_consider, header = FALSE)
name_to_omit <- df$V1
year_to_omit <- as.numeric(df$V2)


# Cycle over stations
faulty_names <- character(0)
faulty_years <- numeric(0)
for(name in common_names){
  
  # Importing series
  fname <- paste0("../STATION_series/Datas/station_series/na_or_zero_filter/", name)
  df <- read.table(fname, header = FALSE)
  hs_series <- as.numeric(df$V2)
  years <- as.numeric(df$V1)
  
  # Selecting years to omit
  mask <- name_to_omit == name
  appo <- year_to_omit[mask]
  
  # Cycle over years
  for(year in unique(years)){
    
    # First case --> Nothing to omit
    if(length(appo) == 0 | !(year %in% appo)){
      
      # Considering single hydrological year
      mask <- years == year
      hs_appo <- hs_series[mask]
      
      if(sum(is.na(hs_appo[92:212])) > 61){
        faulty_names <- c(faulty_names, name)
        faulty_years <- c(faulty_years, year)
      }
      
    }
    
    else next
  }
}



# Saving to file
df <- data.frame(
  station = faulty_names, 
  years = faulty_years
)

write.table(df, "Datas/results/over_half_winter_NA.dat", , row.names = FALSE, col.names = FALSE, quote = FALSE)