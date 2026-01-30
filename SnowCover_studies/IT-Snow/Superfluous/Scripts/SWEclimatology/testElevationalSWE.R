# The main goal of this script is to develop and test a function that we could then
# us to make estimates of SWE elevation-wise.
rm(list = ls())
gc()

# Function to make elevation-wise SWE analysis. The main point here is to use the 
# digitalized elevation model as a tool to understand which pixels to discard and which not to
sweElevation <- function(swe, dem, bands){
  
  # We will split the map across a number of band that is equal to lenght(bands)
  swe_elevation <- numeric(length(bands))
  
  for(i in 1:length(bands)){
    appo <- swe 
    
    if(i == 1){
      appo[dem > bands[i]] <- 0
      appo[dem <= 0] <- 0
    }
    else{
      appo[dem > bands[i]] <- 0
      appo[dem <= bands[i-1]] <- 0
    }
    
    swe_elevation[i] <- sum(appo, na.rm = TRUE)
  }
  
  return(swe_elevation)
}




# First study case
swe <- array(1, dim = c(500, 9))
dem <- array(1:4500, dim = c(500, 9))
bands <- c(500, 1000, 1500, 2000, 2500, 3000, 3500, 4000, 4500)

print("First test gives as result: ")
print(sweElevation(swe = swe, dem = dem , bands = bands))


# Second study case
swe <- array(0, dim = c(500, 9))
swe[, 3] <- 1
dem <- array(1:4500, dim = c(500, 9))
bands <- c(500, 1000, 1500, 2000, 2500, 3000, 3500, 4000, 4500)

print("Second test gives as result: ")
print(sweElevation(swe = swe, dem = dem , bands = bands))


# Third study case
swe <- array(0, dim = c(500, 9))
swe[, 3] <- 1; swe[, 5] <- 1
dem <- array(1:4500, dim = c(500, 9))
bands <- c(500, 1000, 1500, 2000, 2500, 3000, 3500, 4000, 4500)

print("Third test gives as result: ")
print(sweElevation(swe = swe, dem = dem , bands = bands))


# Fourth study case
swe <- array(0, dim = c(500, 9))
swe[1:250, 3] <- 1; swe[1:250, 5] <- 1; swe[, 4] <- 1
dem <- array(1:4500, dim = c(500, 9))
bands <- c(500, 1000, 1500, 2000, 2500, 3000, 3500, 4000, 4500)

print("Fourth test gives as result: ")
print(sweElevation(swe = swe, dem = dem , bands = bands))