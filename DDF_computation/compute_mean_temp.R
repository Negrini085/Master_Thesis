# The main goal of this script is to compute mean temperature series for every 
# usable year. This is a very important step in DDF computation.
rm(list = ls())
gc()

fname_usable <- "Dataset/USABLE_years.dat"
setwd("/home/filippo/Desktop/Codicini/Master_Thesis/Degree_Day_factor/SWE_series/DDF_computation/")


# Importing usable names and years
df <- read.table(fname_usable)
station_names <- df$V1
station_years <- as.numeric(df$V2)




# Cycle over stations
for(name in unique(station_names)){
  
  # Importing lowest and highest temperature for a given station
  fname_tmin <- paste0("../Dataset/model_runs/hydro/TMND/DV_", name)
  df_tmin <- read.table(fname_tmin)
  
  fname_tmax <- paste0("../Dataset/model_runs/hydro/TMXD/DV_", name)
  df_tmax <- read.table(fname_tmax)
  
  
  # Series selection
  tmin_years <- as.numeric(df_tmin$V1)
  tmax_years <- as.numeric(df_tmax$V1)
  tmax <- as.numeric(df_tmax$V2)
  tmin <- as.numeric(df_tmin$V2)
  
  
  # Checking series length and mean temperature computation
  if(length(tmin) != length(tmax)) stop(paste0("Lowest and highest temperature series of incompatible lenght for ", name))
  if(!all(tmin_years == tmax_years)) stop(paste0("Year series non compatible for ", name))
  tmean <- (tmax + tmin)/2
  
  
  
  # Cycle across hydrological years for a given station
  appo_tmean <- numeric(0)
  appo_years <- numeric(0)
  for(y in station_years[station_names == name]){
    
    # Check if those hydrological years are actually part of our dataset
    if(!(y %in% tmin_years)) next
    else if(!(y %in% tmax_years)) next
    
    # Selecting datas for a given hydrological year
    mask <- tmin_years == y
    tmean_hydro <- tmean[mask]
    
    appo_years <- c(appo_years, rep(y, length(tmean_hydro)))
    appo_tmean <- c(appo_tmean, tmean_hydro)
  }
  
  
  # Saving to file
  df_print <- data.frame(
    year = appo_years,
    tmean = appo_tmean
  )
  
  fname_tmean <- paste0("Dataset/TMEAN/V_", sub("^HSD", "TMEAN", name))
  print(paste0("Made mean computation for ", name))
  write.table(df_print, fname_tmean, row.names = FALSE, col.names = FALSE, quote = FALSE)
}