# The goal of this script is to check wether I wrote correctly the frequency function
rm(list = ls())
gc()

library(ncdf4)
library(terra)

# Function to test
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


# First study case
swe <- array(1, dim = c(500, 9))
dem <- array(1:4500, dim = c(500, 9))
bands <- c(500, 1000, 1500, 2000, 2500, 3000, 3500, 4000, 4500)

print("First test gives as result: ")
print(altitudeFreq(swe = swe, dem = dem , alti_bands = bands))


# Second study case
swe <- array(0, dim = c(500, 9))
swe[, 3] <- 1
dem <- array(1:4500, dim = c(500, 9))
bands <- c(500, 1000, 1500, 2000, 2500, 3000, 3500, 4000, 4500)

print("Second test gives as result: ")
print(altitudeFreq(swe = swe, dem = dem , alti_bands = bands))


# Third study case
swe <- array(0, dim = c(500, 9))
swe[, 3] <- 1; swe[, 5] <- 1
dem <- array(1:4500, dim = c(500, 9))
bands <- c(500, 1000, 1500, 2000, 2500, 3000, 3500, 4000, 4500)

print("Third test gives as result: ")
print(altitudeFreq(swe = swe, dem = dem , alti_bands = bands))


# Fourth study case
swe <- array(0, dim = c(500, 9))
swe[1:250, 3] <- 1; swe[1:250, 5] <- 1; swe[, 4] <- 1
dem <- array(1:4500, dim = c(500, 9))
bands <- c(500, 1000, 1500, 2000, 2500, 3000, 3500, 4000, 4500)

print("Fourth test gives as result: ")
print(altitudeFreq(swe = swe, dem = dem , alti_bands = bands))