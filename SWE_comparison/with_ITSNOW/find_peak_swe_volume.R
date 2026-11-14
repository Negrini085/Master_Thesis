# The main goal of this script is to find the peak total swe volume and the 1st 
# April total swe volume stored within the snowpack during each hydrological year 
# in order to make some trend analysis.
rm(list = ls())
gc()

library(trend)

years <- 2011:2023
fname <- "Results/itsnow_swe_series.dat"
setwd("/home/filippo/Desktop/Codicini/Master_Thesis/SWE_comparison/with_ITSNOW/")


# Importing SWE datas
df <- read.table(fname, header = TRUE)
dates <- as.Date(df$dates)
swe <- as.numeric(df$swe)


# Cycle over years
max_swe <- numeric(0)
fap_swe <- numeric(0)
appo_in <- numeric(0)
for(y in years){
  
  # Selecting datas for a given hydrological year
  filter_dates <- seq(from = as.Date(paste0(y-1, "-09-01")), to = as.Date(paste0(y, "-08-31")), by = "day")
  mask <- dates %in% filter_dates
  swe_hydro <- swe[mask]
  
  
  # Finding maximum SWE volume and 1st April SWE volume
  appo_max <- max(swe_hydro, na.rm = TRUE)
  ind <- which.max(swe_hydro)
  appo_fap <- swe[dates == as.Date(paste0(y, "-04-01"))]
  
  max_swe <- c(max_swe, appo_max)
  fap_swe <- c(fap_swe, appo_fap)
  appo_in <- c(appo_in, ind)
}


# Saving to file
df_save <- data.frame(
  years = years,
  max_swe = max_swe,
  fap_swe = fap_swe
)

print("Climatological peak SWE value:")
mean(as.numeric(df_save$max_swe))
print("Climatological peak date:")
mean(appo_in)
sd(appo_in)
print("Climatological 1 April SWE value:")
mean(as.numeric(df_save$fap_swe))
#write.table(df_save, "Results/po_basin_swe_series.dat", row.names = FALSE, col.names = TRUE, quote = FALSE)