# The main goal of this script is to count how many hydrological years contain 
# only NAs or zeros. I will do this analysis on the whole dataset, just to 
# have an order of magnitude
rm(list = ls())
gc()

fname <- "Datas/usable_stations_raw.dat"
setwd("/home/filippo/Desktop/Codicini/Master_Thesis/Degree_Day_factor/STATION_series/")


# Useful variables
all_dataset <- 0
na_or_zeros <- 0
station_names <- as.matrix(read.table(fname)$V1)


# Cycle on station names in order to import snow height series already on 
# hydrological year format
for(name in station_names){
  df <- read.table(paste0("Datas/station_series/raw/", name))
  years <- unique(df$V1)
  
  # Cycle on hydrological years to check whether they pass or not
  for(year in years){
    
    # Selecting specific hydrological year
    mask <- df$V1 == year
    appo <- df[mask, ]
    
    # Actual logic test
    mask <- is.na(as.numeric(appo$V2)) | as.numeric(appo$V2) == 0
    if(sum(mask, na.rm = TRUE) == length(as.numeric(appo$V2))) na_or_zeros <- na_or_zeros + 1
  }
  
  all_dataset <- all_dataset + length(years)
}

print(paste0(na_or_zeros, " on ", all_dataset, " pass the test!"))