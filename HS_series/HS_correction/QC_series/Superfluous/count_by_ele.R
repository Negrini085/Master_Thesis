# The main goal of this script is to count the number of complete hydrological years 
# for a given elevation band. I need to do so in order to later make elevation-wise
# analysis such as quantiles for summer period. I would like to use the same bands
# I worked with when making comparison plots
rm(list = ls())
gc()

fname_all <- "../Dataset/hs_series/all_complete/all_complete.dat"
fname_ana <- "../../Ours/STATION_check/Correcting/ANAGRAFICA_CORRECT"
setwd("/home/filippo/Desktop/Codicini/Master_Thesis/Degree_Day_factor/SWE_series/QC_series/")


# Importing ANAGRAFICA and all hydrological years which don't have gaps
df_all <- read.table(fname_all)
all_names <- df_all$V1

df_ana <- read.table(fname_ana, header = TRUE)
station_names <- df_ana$station_name
ele <- as.numeric(df_ana$ele_rev)


# Selecting elevation for a given station
all_ele <- array(NA, dim = c(length(all_names)))
for(name in unique(all_names)){
  
  # Selecting elevation for a given station
  mask <- station_names == name
  station_ele <- ele[mask]
  if(length(station_ele) != 1) stop(paste0("Problem with elevation length for ", name))
  
  # Filling elevation array
  mask <- all_names == name
  all_ele[mask] <- station_ele
}


# Checking if there are some missing datas left
mask <- is.na(all_ele)
if(sum(mask, na.rm = TRUE) > 0) stop(paste0("Problem! Some missing datas are left in elevation array!"))


# Counting hydrological years for a given elevation band
ele_split <- c(0, 500, 1000, 1500, 2000, 2500, 5000)
count_years <- array(0, dim = c(6))
for(i in 1:6){
  
  # Selecting years for a given elevation band
  mask <- all_ele >= ele_split[i] & all_ele < ele_split[i+1]
  count_years[i] <- sum(mask, na.rm = TRUE)
}

print(count_years)