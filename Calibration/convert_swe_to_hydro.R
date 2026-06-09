# The main goal of this script is to convert SWE series generated via the model 
# from classic annual format to hydrological format.
rm(list = ls())
invisible(gc())

library(parallel)

setwd("/home/filippo/Desktop/Codicini/Master_Thesis/Calibration/")

solar_to_hydro <- function(name){
  # Importing temporal series
  df <- read.table(paste0("Results/raw/", name), header = FALSE)
  years <- unique(as.numeric(df$V1))
  swe_series <- as.numeric(df$V4)
  
  # Arrays to contain datas
  appo_swe <- numeric(0)
  appo_year <- numeric(0)
  
  # Cycle over years
  start <- 244
  for(y in years[2:length(years)]){
    
    # Selecting swe for a given year
    if(y > 2023) next
    
    len <- 365
    if(y %% 4 == 0) len <- 366
    hydro_swe <- swe_series[start:(start + len - 1)]
    start <- start + len
    
    appo_year <- c(appo_year, rep(y, length(hydro_swe)))
    appo_swe <- c(appo_swe, hydro_swe)
  }
  
  # Saving hydro years to file
  df_print <- data.frame(year = appo_year, swe = appo_swe)
  write.table(df_print, paste0("Results/hydro/", name), row.names = FALSE, col.names = FALSE, quote = FALSE)
}


# SWE series names
files <- list.files(path = "Results/raw", full.names = TRUE)
station_names <- sub("Results/raw/", "", files)

# Parallel evolution
n_cores <- 8
invisible(mclapply(station_names, solar_to_hydro, mc.cores = n_cores))