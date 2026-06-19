# The main goal of this script is to convert SWE series generated via the model 
# from classic annual format to hydrological format.
rm(list = ls())
invisible(gc())

library(parallel)

setwd("/home/filippo/Desktop/Codicini/Master_Thesis/Calibration/")

solar_to_hydro <- function(name){
  # Importing temporal series
  df <- read.table(paste0("Results/raw/", name), header = FALSE)
  years <- sort(unique(as.numeric(df$V1)))
  swe_series <- as.numeric(df$V4)
  
  # Creating dates array
  dates <- seq.Date(from = as.Date(paste0(years[1], "-01-01")), to = as.Date(paste0(years[length(years)], "-12-31")), by = "day")
  if(length(swe_series) != length(dates)) stop(paste0("No compatible length for SWE and dates series at ", name))
  if(any(years > 2023)) stop(paste0("SWE values outside precipitation period at ", name))
  
  # Support variables
  appo_swe <- numeric(0)
  appo_year <- numeric(0)
  
  # Cycle over years
  for(y in years[2:length(years)]){
    
    # Selecting SWE for a given year
    mask <- dates >= as.Date(paste0((y-1), "-09-01")) & dates <= as.Date(paste0(y, "-08-31"))
    hydro_swe <- swe_series[mask]
    
    len <- 365
    if((y %% 4 == 0 & y %% 100 != 0) | (y %% 400 == 0)) len <- 366
    if(length(hydro_swe) != len) stop(paste0("Some days are missing on SWE series at ", name, " during ", y))
    
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