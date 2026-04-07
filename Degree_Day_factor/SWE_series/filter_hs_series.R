# The main goal of this script is to filter hs records in order to select only
# those hydrological years which don't have any recording gaps. This is a much 
# needed step toward dataset correction.
rm(list = ls())
gc()

fname <- "../Ours/STATION_series/Dataset/station_series/usable_stations.dat"
setwd("/home/filippo/Desktop/Codicini/Master_Thesis/Degree_Day_factor/SWE_series/")


# Importing station names
df <- read.table(fname)
station_names <- df$V1
flags <- df$V2


# Cycle over stations
appo_year <- numeric(0)
appo_mark <- character(0)
appo_names <- character(0)

for(i in seq_along(station_names)){
  
  # Reading hs series (already in hydrological year form)
  df <- read.table(paste0("../Ours/STATION_series/Dataset/station_series/", station_names[i]))
  hs_series <- as.numeric(df$V2)
  years <- as.numeric(df$V1)
  single_y <- unique(years)
  
  for(y in single_y){
    
    # Selecting given hydrological year
    mask <- years == y
    appo_hs <- hs_series[mask]
    summer <- tail(appo_hs, 60)
    
    if(all(!is.na(appo_hs)) & mean(summer) == 0){
      appo_year <- c(appo_year, y)
      appo_mark <- c(appo_mark, flags[i])
      appo_names <- c(appo_names, station_names[i])
    }
    else{
      hs_series <- hs_series[!mask]
      years <- years[!mask]
    }
  }
  
  if(length(hs_series < 365)){
    print(paste0("No valid years for ", station_names[i]))
  }
  else{
    print(paste0("Some valid years for ", station_names[i]))
    
    # Saving what's left of the starting series
    df_print <- data.frame(years = years, hs_series = hs_series)
    write.table(df_print, paste0("Dataset/hs_series/best_series/", station_names[i]), row.names = FALSE, col.names = FALSE, quote = FALSE)
  }
}


# Saving complete series metadata
df_print <- data.frame(
  station_names = appo_names, 
  hydro_year = appo_year, 
  flag = appo_mark
)

write.table(df_print, "Dataset/hs_series/best_series/best_series.dat", row.names = FALSE, col.names = FALSE, quote = FALSE)