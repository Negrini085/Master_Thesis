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





years <- 1992:2021
months <- c("10", "11", "12", "01", "02", "03", "04", "05", "06", "07")
dayMax <- c(31, 30, 31, 31, 28, 31, 30, 31, 30, 31)

print(length(months))
print(length(dayMax))

swe_evolution <- numeric(0)
for(y in years){
  
  swe_year <- numeric(0)
  
  if(y%%4 == 0){
    dayMax[5] <- 29
  }
  
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
      if(d < 3 & i == 1){
        next
      }
      else if(d > 1 & i == 10){
        break
      }
      
      # Final name
      appo <- paste0(fname, sprintf("%02d", d), ".tif")
      
      if(file.exists(appo)){
        # SWE volume computation
        swe_tif <- rast(appo)
        area <- cellSize(swe_tif, unit="m")
        total_swe <- global(area*swe_tif*10^-12, fun="sum", na.rm=TRUE)$sum[1]
        
        # Storing swe value
        swe_year <- c(swe_year, total_swe)
      }
      
      else{
        swe_year <- c(swe_year, NA)
      }

    }
  }
  
  swe_year <- naCare(swe_year)
  swe_evolution <- c(swe_evolution, swe_year)
  
  # Saving SWE volume in a .dat file
  df <- data.frame(len = 1:length(swe_evolution), swe = swe_evolution)
  write.table(df$swe, file = paste0("Datas/EvoSWE/swe_evolution", y, ".dat"), row.names = FALSE, col.names = FALSE)

  dayMax[5] <- 28
}


# Saving SWE volume in a .dat file
df <- data.frame(len = 1:length(swe_evolution), swe = swe_evolution)
write.table(df$swe, file = "Datas/swe_evolution.dat", row.names = FALSE, col.names = FALSE)

# Plot
ggplot(df, aes(x = len, y = swe)) + 
  geom_line(color = "blue", size = 1.5) +
  labs(title = "SWE evolution: 2011 to 2025", x = "Days", y = "SWE Gm^3") +
  theme_minimal()