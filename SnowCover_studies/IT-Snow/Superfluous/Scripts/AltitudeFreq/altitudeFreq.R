# The goal of this script is to understand which altitude range is more present for a
# given daily snapshot. I feel like that could be interesting to show some snow dynamics.
rm(list = ls())
gc()

library(ncdf4)
library(terra)

# The goal of this function is to enable the user to understand how snow is 
# distributed based on elevation. The function returns an array as long as 
# the number of elevation bands you want to analyze. Each cell represents 
# the number of snow-covered pixels for a given elevation band.
altitudeFreq <- function(swe, dem, alti_bands){
  
  # Here I'll assume that I'm only interested in pixel that sit above sea level
  freq <- numeric(length(alti_bands))
  
  for(i in 1:length(alti_bands)){
    appo <- swe
    
    # Selecting swe pixels
    if(i == 1){
      appo[dem > alti_bands[i]] <- 0 
      appo[dem <= 0] <- 0
    }
    else{
      appo[dem > alti_bands[i]] <- 0 
      appo[dem <= alti_bands[i-1]] <- 0
    }
    
    # Evaluating pixel number
    appo[appo <= 0] <- 0
    appo[appo > 0] <- 1
    freq[i] <- sum(appo, na.rm = TRUE)
  }
  
  return(freq/sum(freq))
}

setwd("/home/filippo/Desktop/Codicini/Master_Thesis/SnowCover_studies/IT-Snow")


# Opening resized DEM
demR <- rast("DEM_Italy_resized.tif")
dem <- as.matrix(demR, wide = TRUE)
dem <- dem[nrow(dem):1, ]
dem <- t(dem)

# Opening netCDF file containing snow cover duration across the whole period
f_name = "y2011/ITSNOW_SWE_201103.nc"
nc <- nc_open(f_name)
lat <- ncvar_get(nc, "Latitude")
lon <- ncvar_get(nc, "Longitude")
swe <- ncvar_get(nc, "SWE", start = c(1, 1, 10), count = c(-1, -1, 1))
nc_close(nc)

# Testing function
bands <- c(500, 1000, 1500, 2000, 2500, 3000, 3500, 4000, 4500)
freqD <- altitudeFreq(swe = swe, dem = dem, alti_bands = bands)
print(freqD)