# The main goal of this script is to evaluate total swe evolution across the 
# investigated period, namely being from 1962 to 2023
rm(list = ls())
gc()

library(ncdf4)
library(parallel)

fname <- "Dataset/SWE_1962_2023.nc"
setwd("/home/filippo/Desktop/Codicini/Master_Thesis/SnowCover_studies/Alpine3D/Historical/")


# Function to compute swe evolution for a given time slice. Considering that pixels are 
# 1000 meters x 1000 meters I will just multiply for a 10^-3 factor instead of a 10^-9 one
annual_swe <- function(start, end, fname){
  
  # Importing SWE maps for a given period. To avoid RAM saturation I will try to
  # considedr dataset slices spanning 1000 days.
  nc <- nc_open(fname)
  print(c(start, end))
  swe_maps <- ncvar_get(nc, "SWECLQMD", start = c(1, 1, start), count = c(-1, -1, end))
  nc_close(nc)
  
  # Apply to compute SWE total volumes
  swe_series <- apply(swe_maps, 3, function(x) sum(x, na.rm = TRUE) * 10^-3)
  rm(swe_maps)
  gc()

  return(swe_series)
}


# SWE series evaluation
nc <- nc_open(fname)
time <- ncvar_get(nc, "time")
print(length(time))
nc_close(nc)


# Computing starts and finishes
chunk_size <- 999
starts <- seq(1, length(time), by = chunk_size)
ends   <- pmin(chunk_size, length(time) - starts + 1)


# Actual swe series computation
swe_tot <- numeric(0)
for(i in seq_len(length(starts))){
  appo <- annual_swe(start = starts[i], end = ends[i], fname = fname)
  swe_tot <- c(swe_tot, appo)
}


# Ready to print datas
df <- data.frame(
  day = 1:length(swe_tot),
  swe = swe_tot
)

write.table(df$swe, file = "Datas/swe_evolution.dat", row.names = FALSE, col.names = FALSE)