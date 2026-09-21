# The main goal of this script is to compute the peak volume of water stored within the snowpack according these two models.
rm(list = ls())
gc()

years <- 2003:2021
fname_po <- "appo_SWE.dat"
fname_itsnow <- "Dataset/total_swe_ITSNOW.dat"
setwd("/home/filippo/Desktop/Codicini/Master_Thesis/SC_studies/Comparison/PO_vs_ITSNOW/")



# Importing ITSNOW file
df <- read.table(fname_itsnow, header = TRUE)
date <- df$dates
swe <- as.numeric(df$swe)

appo_swe <- numeric(0)
appo_date <- as.Date(character(0))
for(y in years){
  
  # Selecting filenames
  dates_first <-seq(as.Date(paste0(y-1, "-09-01")), as.Date(paste0(y-1, "-12-31")))
  dates_second <-seq(as.Date(paste0(y, "-01-01")), as.Date(paste0(y, "-08-31")))
  dates <- c(dates_first, dates_second)
  
  mask <- date %in% dates
  swe_hydro <- swe[mask]
  max_swe <- max(swe_hydro, na.rm = TRUE)
  idx <- which(swe_hydro == max_swe)[1]
  appo_swe <- c(appo_swe, max_swe)
  appo_date <- c(appo_date, dates[idx])
}

print(mean(appo_swe))
print(sd(appo_swe))
print(appo_date)



# Importing PO file
df <- read.table(fname_po, header = TRUE)
date <- df$dates
swe <- as.numeric(df$swe)

appo_swe <- numeric(0)
appo_date <- as.Date(character(0))
for(y in years){
  
  # Selecting filenames
  dates_first <-seq(as.Date(paste0(y-1, "-10-03")), as.Date(paste0(y-1, "-12-31")))
  dates_second <-seq(as.Date(paste0(y, "-01-01")), as.Date(paste0(y, "-07-01")))
  dates <- c(dates_first, dates_second)
  
  mask <- date %in% dates
  swe_hydro <- swe[mask]
  max_swe <- max(swe_hydro, na.rm = TRUE)
  print(max_swe)
  idx <- which(swe_hydro == max_swe)[1]
  appo_swe <- c(appo_swe, max_swe)
  appo_date <- c(appo_date, dates[idx])
}

print(mean(appo_swe))
print(sd(appo_swe))
print(appo_date)