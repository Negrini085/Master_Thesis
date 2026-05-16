# The main goal of this script is to correct snow height series with summer average
# in order to deal with faulty zero readings.
rm(list = ls())
gc()

fname_ana <- "../Dataset/ANAGRAFICA"
fname_usable <- "Datas/station_series/na_or_zero_filter/usable_stations_na_or_zero.dat"
setwd("/home/filippo/Desktop/Codicini/Master_Thesis/Degree_Day_factor/STATION_series/")


# Importing important infos
df <- read.table(fname_ana, header = FALSE)
name_ana <- df$V1
elev_ana <- df$V4

df <- read.table(fname_usable, header = FALSE)
name_usable <- df$V1


# Cycle over station names
for(name in name_usable){
  
  # Finding station elevation
  mask <- name_ana == name
  appo_ele <- elev_ana[mask]
  
  # Importing data
  df <- read.table(paste0("Datas/station_series/na_or_zero_filter/", name), header = FALSE)
  
  if(appo_ele < 2500){
    
    # Opening datas
    years <- df$V1
    hs_series <- df$V2
    
    # Cycle over years
    correct_hs <- numeric(0)
    for(year in unique(years)){
      
      # Selecting hs series section
      mask <- years == year
      appo_hs <- hs_series[mask]
      to_mean <- tail(appo_hs, 62)
      
      # Computing mean value for july and august
      if(all(is.na(to_mean))) mean_val <- 0
      else mean_val <- mean(to_mean, na.rm = TRUE)
      
      # Adjusting series and concatenate
      appo_hs <- appo_hs - mean_val
      correct_hs <- c(correct_hs, appo_hs)
    }
    
    # Saving to file
    df_correct <- data.frame(
      years = years, 
      hs_series = correct_hs
    )
    
    write.table(df_correct, paste0("Datas/station_series/correct_with_summer_average/", name), row.names = FALSE, col.names = FALSE, quote = FALSE) 
  }
  else{
    write.table(df, paste0("Datas/station_series/correct_with_summer_average/", name), row.names = FALSE, col.names = FALSE, quote = FALSE)
  }
  
  print(paste0("Taken care of ", name, " station."))
}
