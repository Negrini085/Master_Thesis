# The goal of this script is to study altitude band frequency (of snow-covered 
# pixels for a whole year
rm(list = ls())
gc()

library(ncdf4)
library(terra)

# Function to evaluate altitude frequency for a given day
altitudeFreq <- function(swe, dem, alti_bands){
  
  freq <- numeric(length(alti_bands))
  
  for(i in 1:length(alti_bands)){
    appo <- swe
    
    if(i == 1){
      appo[dem > alti_bands[i]] <- 0 
      appo[dem < 0] <- 0
    }
    else{
      appo[dem > alti_bands[i]] <- 0 
      appo[dem < alti_bands[i-1]] <- 0
    }
    
    appo[appo <= 0] <- 0
    appo[appo > 0] <- 1
    freq[i] <- sum(appo, na.rm = TRUE)
  }
  
  return(freq/sum(freq))
}

setwd("/home/filippo/Desktop/Codicini/Master_Thesis/SnowCover_studies/IT-Snow")


# Selecting bands and opening resized DEM
bands <- c(500, 1000, 1500, 2000, 2500, 3000, 3500, 4000, 4500)
demR <- rast("DEM_Italy_resized.tif")
dem <- as.matrix(demR, wide = TRUE)
dem <- dem[nrow(dem):1, ]
dem <- t(dem)

ind <- 1
year <- 2011
freqHydro <- array(0, dim = c(365, length(bands)))
months <- c("09", "10", "11", "12", "01", "02", "03", "04", "05", "06", "07", "08")

for(i in 1:length(months)){
  if(i<=4){
    ypr = year-1
    fname = paste0("y", toString(year), "/ITSNOW_SWE_", toString(year-1), months[i], ".nc")
  }
  else{
    ypr = year
    fname = paste0("y", toString(year), "/ITSNOW_SWE_", toString(year), months[i], ".nc")
  }
  
  
  nc <- nc_open(fname)
  time <- ncvar_get(nc, names(nc$var)[1])
  swe <- ncvar_get(nc, names(nc$var)[2])
  nc_close(nc)
  
  for(j in 1:length(time)){
    print(paste0("Dealing with ", sprintf("%02d", j), "/", months[i], "/", ypr," SWE maps"))
    appo <- altitudeFreq(swe = swe[, , j], dem = dem, alti_bands = bands)
    
    for(k in 1:length(appo)){
      freqHydro[ind, k] <- appo[k]
    }
    ind <- ind + 1
  }
  
}

# Saving result on a .dat file
df <- data.frame(
  f1 = freqHydro[, 1],
  f2 = freqHydro[, 2],
  f3 = freqHydro[, 3],
  f4 = freqHydro[, 4],
  f5 = freqHydro[, 5],
  f6 = freqHydro[, 6],
  f7 = freqHydro[, 7],
  f8 = freqHydro[, 8],
  f9 = freqHydro[, 9]
)

write.table(
  format(df, scientific = FALSE, digits = 6),
  file = "freqAltitude2011.dat", 
  row.names = FALSE, 
  col.names = FALSE, 
  quote = FALSE)