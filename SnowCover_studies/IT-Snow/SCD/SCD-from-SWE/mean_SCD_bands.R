# The main goal of this script is to compute elevation-wise mean SCD values. I'll
# try to work with 25 meter wide elevation bands, but probably above a certain 
# threshold there will be just too little pixels. That's the main reason why we 
# will stop at 4000 meters. We will also need to filter out faulty pixels!
rm(list = ls())
gc()

library(terra)
library(ncdf4)

# Function to compute mean SCD values for a given elevation band. In order to 
# select the correct pixels, superior and inferior limits must be specified. One
# also has to give as a function the input DEM model and the SCD mean map.
scd_per_band <- function(dem, scd, lim_inf, lim_sup){
  
  # Creating correct mask
  if(lim_inf == 0){
    mask_ele <- !is.na(dem) & dem <= lim_sup
  }
  else{
    mask_ele <- !is.na(dem) & dem > lim_inf & dem <= lim_sup
  }
  
  # Evaluating value 
  appo <- mean(scd[mask_ele], na.rm = TRUE)
  return(round(appo))
}

setwd("/home/filippo/Desktop/Codicini/Master_Thesis/SnowCover_studies/IT-Snow/SCD/SCD-from-SWE")
fname <- "Datas/mean_SCD_map.nc"

bands <- c(seq(from = 25, to = 3000, by = 25), seq(from = 3050, to = 3500, by = 50), seq(from = 3600, to = 4000, by = 100))



# Importing mean SCD map and DEM in order to check if a 25 meter window is wide 
# enough, or we just have to stick with 50 meters as we did before. The main problem
# is that at high elevation there are not plenty of pixels, so I decided to just 
# consider 25 meter bands until 3000 meters, than 50 meter bands for pixels laying
# between 3000 and 3500 meters. Finally, in order to reach 4000 meters we will use
# 100 meter wide bands. Doing so, we are trying to keep statistically significative
# the pixel number.
nc <- nc_open(fname)
scd <- ncvar_get(nc, "SCD")
nc_close(nc)

dem <- rast("Datas/DEM_Italy.tif")
dem <- as.matrix(dem, wide = TRUE)
dem <- dem[nrow(dem):1, ]
dem <- t(dem)

for(i in 1:length(bands)){
  
  # Creating correct mask: we have to check that DEM model is defined for a given
  # point and that its elevation meets our requirements
  appo <- dem > 1000 & scd == 0
  if(i == 1){
    mask_ele <- !is.na(dem) & dem <= bands[i] & !appo
  }
  else{
    mask_ele <- !is.na(dem) & dem > bands[i-1] & dem <= bands[i] & !appo
  }
  
  print(length(scd[mask_ele]))
}



# First thing we need to do is to take care of faulty pixels we found in previous
# analysis. We will just mark them as not assigned.
mask_faulty <- scd > 300 & dem < 500
scd[mask_faulty] <- NA

mask_faulty <- scd > 30 & scd < 200 & dem < 50
scd[mask_faulty] <- NA

mask_faulty <- scd == 0 & dem > 1000 & !is.na(dem)
scd[mask_faulty] <- NA



# We will now compute mean SCD values for every elevation band
scd_bands <- array(0, dim = c(length(bands)))
for(i in 1:length(scd_bands)){
  if(i == 1){
    scd_bands[i] <- scd_per_band(dem = dem, scd = scd, lim_inf = 0, lim_sup = bands[i])
  }
  else{
    scd_bands[i] <- scd_per_band(dem = dem, scd = scd, lim_inf = bands[i-1], lim_sup = bands[i])
  }
}



# Saving those elevation-wise SCD means on a .dat file
df <- data.frame(
  band = 1:length(scd_bands),
  scd_bands = scd_bands
)
write.table(scd_bands, file = "mean_SCD_per_band.dat", row.names = FALSE, col.names = FALSE)