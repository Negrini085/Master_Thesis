# The main goal of this script is to find the peak total swe volume and the 1st 
# April total swe volume stored within the snowpack during each hydrological year 
# in order to make some trend analysis.
rm(list = ls())
gc()

library(trend)

years <- 1992:2021
fname <- "Datas/swe_evolution_elevation.dat"
setwd("/home/filippo/Desktop/Codicini/Master_Thesis/SC_studies/Po-Basin/")


# Importing SWE data
df <- read.table(fname, header = FALSE)
swe <- as.numeric(df$V2)


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
doy_max <- numeric(0)
date_max <- as.Date(character(0))
for(y in years){
  
  # Selecting datas for a given hydrological year
  filter_dates <- seq(from = as.Date(paste0(y-1, "-10-03")), to = as.Date(paste0(y, "-07-01")), by = "day")
  mask <- dates %in% filter_dates
  swe_hydro <- swe[mask]
  
  
  # Finding maximum SWE volume and 1st April SWE volume
  appo_max <- max(swe_hydro, na.rm = TRUE)
  
  
  # Finding SWE maximum date
  idx <- which.max(swe_hydro)
  appo_max_date <- filter_dates[idx]
  appo_doy <- as.numeric(appo_max_date - as.Date(paste0(y - 1, "-10-01")))
  

  # Saving values
  max_swe <- c(max_swe, appo_max)
  date_max <- c(date_max, appo_max_date)
  doy_max  <- c(doy_max, appo_doy)
}

# Plotting SWE metrics
mean_doy <- mean(doy_max)
mean_date <- as.Date("2000-10-01") + mean_doy

cat("Mean peak SWE volume: ", mean(max_swe), "+/-", sd(max_swe), "Gm^3", "\n")
cat("Mean peak SWE date:", format(mean_date, "%d %B"), "+/-", round(sd(doy_max), 1), "days\n")
cat("\n\n\n")

mk.test(max_swe)
sens.slope(max_swe)