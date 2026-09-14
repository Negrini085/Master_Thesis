# The main goal of this script is to find the peak total swe volume and the 1st 
# April total swe volume stored within the snowpack during each hydrological year 
# in order to make some trend analysis.
rm(list = ls())
gc()

years <- 1952:2023
fname <- "Results/SWE/masked_swe_evolution.dat"
setwd("/home/filippo/Desktop/Codicini/Master_Thesis/SWE_analysis/")


# Importing SWE datas
df <- read.table(fname, header = TRUE)
dates <- as.Date(df$data)
swe <- as.numeric(df$swe)


# Cycle over years
max_swe <- numeric(0)
fap_swe <- numeric(0)
for(y in years){
  
  # Selecting datas for a given hydrological year
  filter_dates <- seq(from = as.Date(paste0(y-1, "-10-01")), to = as.Date(paste0(y, "-09-30")), by = "day")
  mask <- dates %in% filter_dates
  swe_hydro <- swe[mask]
  
  
  # Finding maximum SWE volume and 1st April SWE volume
  appo_max <- max(swe_hydro, na.rm = TRUE)
  appo_fap <- swe[dates == as.Date(paste0(y, "-04-01"))]
  
  max_swe <- c(max_swe, appo_max)
  fap_swe <- c(fap_swe, appo_fap)
}


# Saving to file
df_save <- data.frame(
  years = years,
  max_swe = max_swe,
  fap_swe = fap_swe
)
write.table(df_save, "Results/SWE/peak_and_first_april_swe_volume.dat", row.names = FALSE, col.names = TRUE, quote = FALSE)