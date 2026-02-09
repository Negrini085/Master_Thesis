# The main goal of this script is to determine swe evolution of the snowpack. I will 
# try to make use of functions, in order to make code easier to read. We will make 
# use of linear interpolation in order to overcame dataset gaps.

library(terra)
library(ggplot2)
setwd("/home/filippo/Desktop/Codicini/Master_Thesis/SnowCover_studies/Po-Basin")



# Function which its main goal is to take care of not assigned values is order to 
# overcome the presence of corrupted files in the dataset
naCare <- function(swe){
  
  # First check we need to make is whether the first value is not assigned. If 
  # that's the case we will scroll through the array until we find a numeric value
  if(is.na(swe[1])){
    
    i <- 1
    while(is.na(swe[i])){
      i <- i+1
    }
    
    j <- i+1
    while(is.na(swe[j])){
      j <- j+1
    }
    
    delta <- (swe[j] - swe[i])/(j-i)
    
    # Filling gaps till the first numeric value
    for(k in 1:(i-1)){
      swe[i-k] <- swe[i-k+1] - delta
      if(swe[i-k] < 0) swe[i-k] <- 0
    }
  }
  
  
  # Second check we need to make is whether the last value is not assigned. If 
  # that's the case we will scroll backwards through the array until we find a numeric value
  if(is.na(swe[length(swe)])){
    
    i <- length(swe)
    while(is.na(swe[i])){
      i <- i-1
    }
    
    j <- i-1
    while(is.na(swe[j])){
      j <- j-1
    }
    
    delta <- (swe[i] - swe[j])/(i-j)
    
    # Filling the gaps till the last numeric value
    for(k in (i+1):length(swe)){
      swe[k] <- swe[k-1] + delta
      if(swe[k] < 0) swe[k] <- 0
    }
  }
  
  
  # Last check we need to make regards the array as a whole, because there could
  # be not assigned values in places that aren't arrays head or tail. We will now
  # scroll the whole array looking for those inner NAs. After that, everything
  # should be accounted for.
  for(i in 2:(length(swe)-1)){
    if(is.na(swe[i])){
      
      j <- i+1
      while(is.na(swe[j])){
        j <- j+1
      }
      
      delta <- (swe[j] - swe[i-1])/(j-i+1)
      
      # Filling gaps till next numeric value
      for(k in i:(j-1)){
        swe[k] <- swe[k-1] + delta
        if(swe[k] < 0) swe[k] <- 0
      }
    }
  }
  
  
  for(i in swe){
    if(is.na(i)) print("NA handling procedure produced an error, you should check!")
  }
  
  return(swe)
}



# Function to make elevation-wise SWE analysis. The main point here is to use the 
# digitalized elevation model as a tool to understand which pixels to discard and which not to
sweElevation <- function(swe, dem, mask_italy, bands){
  
  # We will split the map across a number of band that is equal to lenght(bands)
  swe_elevation <- numeric(length(bands))
  maskP <- NULL
  
  for(i in 1:length(bands)){
    
    # Creating mask to select only pixels that respect a certain set of conditions
    if(i == 1){
      maskP <- dem < bands[i] & !is.na(swe) & !is.na(mask_italy)
    }
    else{
      maskP <- dem < bands[i] & dem >= bands[i-1] & !is.na(swe) & !is.na(mask_italy)
    }
    
    # Summing over pixels selected thanks to logic mask. Here we find the total 
    # SWE volume for a certain altitude band, because the swe input map is already
    # considering pixel volumes
    appo <- mask(swe, maskP, maskvalues = FALSE)
    swe_elevation[i] <- global(appo, fun="sum", na.rm = TRUE)$sum[1]
  }
  
  return(swe_elevation)
}






# Specifying years and altitude band limits
years <- 1992:2021
bands <- c(500, 1000, 1500, 2000, 2500, 5000)
dayMax <- c(31, 30, 31, 31, 28, 31, 30, 31, 30, 31)
months <- c("10", "11", "12", "01", "02", "03", "04", "05", "06", "07")

# Importing DEM model
dem <- rast("Dataset/Po_DEM.tif")

# Importing Italian Mask
mask_italy <- rast("Dataset/Italy_Mask.tif")



# Whole SWE volume series container and counter
contaTot <- 1
swe_evolution <- array(0, dim = c(8168, 6))

# Beginning cycle across years
for(y in years){
  
  # SWE volume annual container and counter
  conta <- 1
  swe_year <- NULL
  
  # Leap year or not? In the former case, the snow season has a 273 days duration, 
  # whilst in the latter only 272 (as one would expect considering the one day difference)
  if(y%%4 == 0){
    dur <- 273
    dayMax[5] <- 29
    swe_year <- array(0, dim = c(dur, 6))
  }
  else{
    dur <- 272
    swe_year <- array(0, dim = c(dur, 6))
  }
  
  # Beginning cycle across months (here we are considering a single snow season)
  for(i in 1:length(months)){
    
    # Creating the correct file path
    if(i > 3){
      fname <- paste0("Dataset/", toString(y), "/SWE_", toString(y), "-", months[i], "-")
      print(paste0("Considering month  ", months[i], "/", y, " for SWE evaluation."))
    }
    else{
      fname <- paste0("Dataset/", toString(y), "/SWE_", toString(y-1), "-", months[i], "-")
      print(paste0("Considering month  ", months[i], "/", y-1, " for SWE evaluation."))
    }
    
    # Considering single day swe maps
    for(d in 1:dayMax[i]){
      
      # Here we are taking care of the fact that the snow season begins on the 
      # 3rd of October and ends on the 1st of July
      if(d < 3 & i == 1){
        next
      }
      else if(d > 1 & i == 10){
        break
      }
      
      # Final name
      appo <- paste0(fname, sprintf("%02d", d), ".tif")
      
      # Checking if file exists or not
      if(file.exists(appo)){
        # SWE volume computation
        swe_tif <- rast(appo)
        area <- cellSize(swe_tif, unit="m")
        swe <- area*swe_tif*10^-12
        
        # Storing swe value
        swe_year[conta, ] <- sweElevation(swe = swe, dem = dem, mask_italy = mask_italy, bands = bands)
        conta <- conta + 1
      }
      
      else{
        swe_year[conta, ] <- array(NA_real_, dim = c(6))
        conta <- conta + 1
      }
      
    }
  }
  
  # Here we have to take care of possible NAs and to link the seasonal datas to 
  # the whole series (saved in swe_evolution)
  for(h in 1:6){
    swe_year[, h] <- naCare(swe_year[, h])
    swe_evolution[contaTot:(contaTot+dur-1), h] <- swe_year[, h]
  }
  contaTot <- contaTot + dur
  
  # Saving seasonal SWE volume in a .dat file
  df <- as.data.frame(swe_year)
  write.table(df, file = paste0("Datas/Volume_SWE/Evo_Italian_Ele_SWE/swe_evolution", y, ".dat"), row.names = FALSE, col.names = FALSE)
  
  dayMax[5] <- 28
}


# Saving SWE volume in a .dat file
df <- as.data.frame(swe_evolution)
write.table(df, file = "Datas/swe_italian_po_evolution_elevation.dat", row.names = FALSE, col.names = FALSE)