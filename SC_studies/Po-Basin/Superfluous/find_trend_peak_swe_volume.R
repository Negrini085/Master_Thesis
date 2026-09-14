# The main goal of this script is to find the peak total swe volume and the 1st 
# April total swe volume stored within the snowpack during each hydrological year 
# in order to make some trend analysis.
rm(list = ls())
gc()

library(trend)

years <- 1992:2021
fname <- "Datas/swe_evolution.dat"
setwd("/home/filippo/Desktop/Codicini/Master_Thesis/SC_studies/Po-Basin/")


# Importing SWE data
df <- read.table(fname, header = FALSE)
swe <- as.numeric(df$V1)


# Creating date array and checking whether length are the same or not
dates <- numeric(0)
for(y in years){
  appo <- seq(from = as.Date(paste0(y-1, "-10-03")), to = as.Date(paste0(y, "-07-01")), by = "day")
  if(y==1992) dates <- appo
  else dates <- c(dates, appo)
}
stopifnot(length(swe) == length(dates))


# Cycle over years
max_swe <- numeric(0)
for(y in years){
  
  # Selecting datas for a given hydrological year
  filter_dates <- seq(from = as.Date(paste0(y-1, "-10-03")), to = as.Date(paste0(y, "-07-01")), by = "day")
  mask <- dates %in% filter_dates
  swe_hydro <- swe[mask]
  
  
  # Finding maximum SWE volume and 1st April SWE volume
  appo_max <- max(swe_hydro, na.rm = TRUE)
  max_swe <- c(max_swe, appo_max)
}


mk.test(max_swe)
sens.slope(max_swe)