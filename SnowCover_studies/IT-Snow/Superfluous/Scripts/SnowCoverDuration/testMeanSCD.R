# The main focus of this script is to develop a routine to compute mean snow cover 
# duration for a given altitude band. We will first write the function, and then we 
# will test it on some test situation.
rm(list = ls())
gc()

library(matrixStats)

# Function to compute mean snow cover duration across the whole Italian territory
meanSCD <- function(scd, dem){
  
  # Making sure to just check pixels within Italy
  appo <- scd
  appo[dem < -100] <- NA
  
  return(round(mean(appo, na.rm = TRUE)))
}


# Function to compute mean elevation-based snow cover duration across the whole 
# Italian territory. The user can specify band limits.
meanBandSCD <- function(scd, dem, bands){
  
  liminf <- NULL
  limsup <- NULL
  appo <- numeric(length(bands))
  for(i in 1:length(bands)){
    
    # Selecting band limits (the higher one will be consideed as part of the band,
    # while the lower one will not be a part of it)
    if(i == 1){
      liminf <- 0
      limsup <- bands[i]
    }
    else{
      liminf <- bands[i-1]
      limsup <- bands[i]
    }
    
    appo[i] <- mean(scd[dem > liminf & dem <= limsup], na.rm = TRUE)
  }
  
  # Making sure to just check pixels within Italy
  return(round(appo, 0))
}





# First study case
swe <- array(1:4500, dim = c(500, 9))
dem <- array(1:4500, dim = c(500, 9))
bands <- c(500, 1000, 1500, 2000, 2500, 3000, 3500, 4000, 4500)

print("First test gives as result: ")
print(meanSCD(scd = swe, dem = dem))
print(meanBandSCD(scd = swe, dem = dem , bands = bands))


# Second study case
swe <- array(0, dim = c(500, 9))
swe[, 3] <- 1:500
dem <- array(1:4500, dim = c(500, 9))
bands <- c(500, 1000, 1500, 2000, 2500, 3000, 3500, 4000, 4500)

print("Second test gives as result: ")
print(meanSCD(scd = swe, dem = dem))
print(meanBandSCD(scd = swe, dem = dem , bands = bands))


# Third study case
swe <- array(0, dim = c(500, 9))
swe[, 3] <- 1:500; swe[, 5] <- 1:500
dem <- array(1:4500, dim = c(500, 9))
bands <- c(500, 1000, 1500, 2000, 2500, 3000, 3500, 4000, 4500)

print("Third test gives as result: ")
print(meanSCD(scd = swe, dem = dem))
print(meanBandSCD(scd = swe, dem = dem , bands = bands))


# Fourth study case
swe <- array(0, dim = c(500, 9))
swe[1:250, 3] <- 40; swe[1:250, 5] <- 30; swe[, 4] <- 100
dem <- array(1:4500, dim = c(500, 9))
bands <- c(500, 1000, 1500, 2000, 2500, 3000, 3500, 4000, 4500)

print("Fourth test gives as result: ")
print(meanSCD(scd = swe, dem = dem))
print(meanBandSCD(scd = swe, dem = dem , bands = bands))
