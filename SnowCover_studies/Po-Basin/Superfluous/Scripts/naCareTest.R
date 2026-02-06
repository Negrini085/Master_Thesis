# The main goal of this script is to test the procedure we will be use to handle
# dataset gaps resulting in NA values in swe time series.


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




# First check -> NA at array head
appo <- array(1:20)
appo[1] <- NA
appo[2] <- NA
appo[3] <- NA
print("First check result: ")
print(appo)
print(naCare(appo))


# Second check -> NA at array tail
appo <- array(1:20)
appo[18] <- NA
appo[19] <- NA
appo[20] <- NA
print("Second check result: ")
print(appo)
print(naCare(appo))


# Third check -> NA in the middle
appo <- array(1:20)
appo[2] <- NA
appo[14] <- NA
appo[15] <- NA
appo[19] <- NA
print("Third check result: ")
print(appo)
print(naCare(appo))


# Fourth check -> a little bit of everything
appo <- array(1:20)
appo[1] <- NA
appo[14] <- NA
appo[15] <- NA
appo[19] <- NA
appo[20] <- NA
print("Fourth check result: ")
print(appo)
print(naCare(appo))