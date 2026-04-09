# The main goal of this script is to compute daily solid precipitations to later
# use for DDF computations. I will use as threshold Tmin = 0 °C and Tmax = 3 °C.
rm(list = ls())
gc()

fname_ok <- "Dataset/OK_years.dat"
setwd("/home/filippo/Desktop/Codicini/Master_Thesis/Degree_Day_factor/SWE_series/DDF_computation/")


# Function to compute solid precipitation fraction. The idea is that above a certain
# threshold the precipitation is only liquid (it rains), under another one it's only
# solid (it rains) and in between it's a mix of both.
compute_snw_hydro <- function(prec_hydro, tmin_hydro, tmax_hydro, name, year, T1 = 0, T2 = 3){
  
  # Check if threshold are correctly ordered or not
  if(T1 > T2){
    appo = T1
    T1 = T2
    T2 = appo
  }
  
  # Check on array lengths
  if((length(prec_hydro) != length(tmin_hydro)) | (length(tmin_hydro) != length(tmax_hydro))){
    print(length(prec_hydro))
    print(length(tmin_hydro))
    print(length(tmax_hydro))
    stop(paste0("Precipitation and temperature series have incompatible length for ", name, " during ", y))
  }
  
  # Support variable and mean temperature computation
  appo <- array(0, dim = c(length(prec_hydro)))
  tmean_hydro <- (tmin_hydro + tmax_hydro)/2
  
  
  # Solid fraction computation
  mask <- tmean_hydro <= T1
  appo[mask] <- prec_hydro[mask] 
  
  mask <- tmean_hydro > T1 & tmean_hydro < T2
  appo[mask] <- prec_hydro[mask] * (T2 - tmean_hydro[mask])/(T2 - T1)
  
  return(appo)
}



# Importing stations and hydrological years ready for DDF computation
df <- read.table(fname_ok)
station_names <- df$V1
station_years <- as.numeric(df$V2)



# Cycle over stations
usable_years <- numeric(0)
usable_stations <- character(0)
for(name in unique(station_names)){
  
  # Temperature files & precipitation file names
  fname_prec <- paste0("../Dataset/model_runs/hydro/PCPD/DV_", name)
  fname_tmin <- paste0("../Dataset/model_runs/hydro/TMND/DV_", name)
  fname_tmax <- paste0("../Dataset/model_runs/hydro/TMXD/DV_", name)
  
  
  # Importing series
  df_prec <- read.table(fname_prec)
  prec_years <- as.numeric(df_prec$V1)
  prec <- as.numeric(df_prec$V2)
  
  df_tmin <- read.table(fname_tmin)
  tmin_years <- as.numeric(df_tmin$V1)
  tmin <- as.numeric(df_tmin$V2)
  
  df_tmax <- read.table(fname_tmax)
  tmax_years <- as.numeric(df_tmax$V1)
  tmax <- as.numeric(df_tmax$V2)
  
  
  # Cycle over precipitation years (last year taken care of is 2023)
  appo_snw <- numeric(0)
  appo_years <- numeric(0)
  for(y in station_years[station_names == name]){
    
    # Check if those hydrological years are actually part of our dataset
    if(!(y %in% prec_years)) next
    else if(!(y %in% tmin_years)) next
    else if(!(y %in% tmax_years)) next

    # Selecting datas for a given hydrological years
    mask <- prec_years == y
    prec_hydro <- prec[mask]

    mask <- tmin_years == y
    tmin_hydro <- tmin[mask]

    mask <- tmax_years == y
    tmax_hydro <- tmax[mask]


    # Actual SWN computation
    snw_hydro <- compute_snw_hydro(prec_hydro = prec_hydro, tmin_hydro = tmin_hydro, tmax_hydro = tmax_hydro, name = name, year = y)
    appo_years <- c(appo_years, rep(y, length(snw_hydro)))
    appo_snw <- c(appo_snw, snw_hydro)
    
    
    # Updating usable years
    usable_years <- c(usable_years, y)
    usable_stations <- c(usable_stations, name)
  }
  
  
  # Saving to file
  df_print <- data.frame(
    year = appo_years,
    snow = appo_snw
  )
  
  fname_snw <- paste0("Dataset/SNW/V_", sub("^HSD", "SNW", name))
  print(paste0("Made solid precipitation computation for ", name))
  write.table(df_print, fname_snw, row.names = FALSE, col.names = FALSE, quote = FALSE)
}



# Saving usable years
df_usable <- data.frame(
  station = usable_stations, 
  years = usable_years
)

write.table(df_usable, "Dataset/USABLE_years.dat", row.names = FALSE, col.names = FALSE, quote = FALSE)