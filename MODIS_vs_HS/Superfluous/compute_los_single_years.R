# The main goal of this script is to compute single year los values, in order 
# to be able to check bias behavior.
rm(list = ls())
gc()

setwd("/home/filippo/Desktop/Codicini/Master_Thesis/MODIS_vs_HS/")
files <- list.files(path = "../HS_series/Correct/MODIS_series/Dataset/modis_hydrological", full.names = TRUE)
names <- sub("../HS_series/Correct/MODIS_series/Dataset/modis_hydrological/", "", files)


# Useful variables
los_hs <- numeric(0)
los_mod <- numeric(0)
appo_year <- numeric(0)
appo_name <- character(0)


# Cycle over station names
for(name in names){
  
  # Importing HS and MODIS series
  fname_hs <- paste0("../HS_series/Correct/STATION_series/Dataset/", name)
  df_hs <- read.table(fname_hs, header = FALSE)
  
  fname_mod <- paste0("Dataset/sc_hydro/", name)
  df_mod <- read.table(fname_mod, header = FALSE)
  
  
  # Selecting only overlapping periods
  mask <- as.numeric(df_hs$V1) > 2000
  df_hs <- df_hs[mask, ]
  
  mask <- as.numeric(df_mod$V1) %in% unique(as.numeric(df_hs$V1))
  df_mod <- df_mod[mask, ]
  
  if(nrow(df_mod) != nrow(df_hs)) stop(paste0("Mismatch in data-frame lenght for ", name, "! ", nrow(df_mod), " & ", nrow(df_hs)))
  if(sum(as.numeric(df_hs$V2), na.rm = TRUE) == 0) stop("No HS datas for ", name)
  
  for(y in unique(as.numeric(df_mod$V1))){
    
    # Computing LOS from two different snow sources
    mask <- as.numeric(df_mod$V1) == y
    appo_mod <- sum(as.numeric(df_mod$V2)[mask], na.rm = TRUE)
    
    mask <- as.numeric(df_hs$V1) == y
    appo_sta <- sum(as.numeric(df_hs$V2)[mask], na.rm = TRUE)
    
    # Saving values
    appo_year <- c(appo_year, y)
    los_hs <- c(los_hs, appo_sta)
    los_mod <- c(los_mod, appo_mod)
    appo_name <- c(appo_name, name)
  }
}


# Saving procedure
df_print <- data.frame(
  name = appo_name, 
  year = appo_year, 
  los_HS = los_hs, 
  los_MOD = los_mod
)

write.table(df_print, "Results/compare_los.dat", row.names = FALSE, col.names = TRUE, quote = FALSE)