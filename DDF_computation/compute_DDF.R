# The main goal of this script is to filter out those USABLE_years which don't 
# suit our criteria for DDF computation. What we are looking for is a match between
# start of snow coverage and solid precipitation, in order to be sure that we are
# making scientifically accurate estimate.
rm(list = ls())
gc()

fname <- "Results/csc.dat"
setwd("/home/filippo/Desktop/Codicini/Master_Thesis/Degree_Day_factor/SWE_series/DDF_computation/")


# Function to compute DDF
compute_DDF <- function(csc_snw, csc_tmean, name, year){
  
  # Evaluating temperature excess
  mask <- csc_tmean > 0
  t_ex <- sum(csc_tmean[mask], na.rm = TRUE)
  
  # Evaluating solid precipitation
  tot_snw <- sum(csc_snw, na.rm = TRUE)
  
  # DDF computation
  if(t_ex <= 0) return(NULL)
  ddf <- tot_snw/t_ex
  print(ddf)
  
  return(c(tot_snw, t_ex, ddf))
}



# Importing csc datas
df_csc <- read.table(fname)
station_names <- df_csc$V1
csc <- as.numeric(df_csc$V3)
start <- as.numeric(df_csc$V4)
station_years <- as.numeric(df_csc$V2)



# Cycle across stations
appo_snw <- numeric(0)
appo_DDF <- numeric(0)
appo_name <- character(0)
appo_year <- character(0)
appo_excess <- numeric(0)
for(name in unique(station_names)){
  
  # Selecting hydrological years for a given station
  mask <- station_names == name
  years <- station_years[mask]
  
  
  # Importing solid precipitation and mean temperature dataset
  fname_snw <- paste0("Dataset/SNW/V_", sub("HSD_", "SNW_", name))
  df_snw <- read.table(fname_snw)
  
  fname_tmean <- paste0("Dataset/TMEAN/V_", sub("HSD_", "TMEAN_", name))
  df_tmean <- read.table(fname_tmean)

  
  # Series extraction
  snw_years <- as.numeric(df_snw$V1)
  snw <- as.numeric(df_snw$V2)
  
  tmean_years <- as.numeric(df_tmean$V1)
  tmean <- as.numeric(df_tmean$V2)
  
  
  # Cycle across station years
  for(y in years){
    
    # Check if we actually have this specific hydrological year in our dataset
    if(!(y %in% snw_years)) stop("No year ", y, " in solid precipitation file for ", name)
    if(!(y %in% tmean_years)) stop("No year ", y, " in mean temperature file file for ", name)
    
    
    # Selecting series for a given hydrological year
    mask <- snw_years == y
    snw_hydro <- snw[mask]
    
    mask <- tmean_years == y
    tmean_hydro <- tmean[mask]
    
    
    # Selecting start of csc period
    mask <- station_names == name & station_years == y
    start_hydro <- start[mask]
    csc_hydro <- csc[mask]
    
    
    # Checking if on the day previous to the beginning of snow coverage there was a solid precipitation
    if(is.na(start_hydro)) next
    if(snw_hydro[(start_hydro-1)] > 0 & (start_hydro-1) > 0){
      start_hydro <- start_hydro - 1
      csc_hydro <- csc_hydro + 1
      
      appo <- compute_DDF(csc_snw = snw_hydro[start_hydro:(start_hydro+csc_hydro)], csc_tmean = tmean_hydro[start_hydro:(start_hydro+csc_hydro)], name = name, year = y)
      if(is.null(appo)) next
      
      appo_excess <- c(appo_excess, appo[2])
      appo_snw <- c(appo_snw, appo[1])
      appo_DDF <- c(appo_DDF, appo[3])
      appo_name <- c(appo_name, name)
      appo_year <- c(appo_year, y)
    }
    # Checking if on the day there was a solid precipitation
    else if(snw_hydro[start_hydro] > 0){
      appo <- compute_DDF(csc_snw = snw_hydro[start_hydro:(start_hydro+csc_hydro)], csc_tmean = tmean_hydro[start_hydro:(start_hydro+csc_hydro)], name = name, year = y)
      if(is.null(appo)) next
      
      appo_excess <- c(appo_excess, appo[2])
      appo_snw <- c(appo_snw, appo[1])
      appo_DDF <- c(appo_DDF, appo[3])
      appo_name <- c(appo_name, name)
      appo_year <- c(appo_year, y)
    }
  }
  
}



# Saving to file
df_ddf <- data.frame(
  name = appo_name, 
  year = appo_year, 
  snow = appo_snw, 
  t_excess = appo_excess, 
  ddf = appo_DDF
)

write.table(df_ddf, "Results/ddf.dat", row.names = FALSE, col.names = TRUE, quote = FALSE)