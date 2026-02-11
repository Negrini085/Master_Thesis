# The main goal of this script is to do an elevation-based SWE evolution analysis
# across the whole investigated period. In particular, I would like to divide the 
# whole area across 9 elevation bands, each one about 500 meters thick. Then, I 
# would like to plot singularly every climatology in a 3x3 plot and also make a 
# plot in which they all sum up to the total SWE known climatology

library(ncdf4)
library(terra)
library(ggplot2)

# Function to make elevation-wise SWE analysis. The main point here is to use the 
# digitalized elevation model as a tool to understand which pixels to discard and which not to
sweElevation <- function(swe, dem, bands){
  
  # We will split the map across a number of band that is equal to lenght(bands)
  swe_elevation <- numeric(length(bands))
  
  for(i in 1:length(bands)){
    
    # Creating correct mask in order to consider different elevation bands
    if(i == 1){
      mask <- !is.na(dem) & dem <= bands[i]
    }
    else{
      mask <- !is.na(dem) & dem > bands[i-1] & dem <= bands[i]
    }
    
    swe_elevation[i] <- sum(swe[mask], na.rm = TRUE)
  }
  
  return(swe_elevation)
}




start_time <- Sys.time()
setwd("/home/filippo/Desktop/Codicini/Master_Thesis/SnowCover_studies/IT-Snow")

# Time window
years <- 2011:2025
evo_appo <- array(0, dim = c(5479, 9))
bands <- c(500, 1000, 1500, 2000, 2500, 3000, 3500, 4000, 5000)
months <- c("09", "10", "11", "12", "01", "02", "03", "04", "05", "06", "07", "08")

demR <- rast("DEM/DEM_Italy.tif")
dem <- as.matrix(demR, wide = TRUE)
dem <- dem[nrow(dem):1, ]
dem <- t(dem)


#----------------------------------------------#
#              Creating area map               #
#----------------------------------------------#
nc <- nc_open("y2011/ITSNOW_SWE_201103.nc")
lat <- ncvar_get(nc, "Latitude")
lon <- ncvar_get(nc, "Longitude")
nc_close(nc)

# Here we are going to assume that earth is a perfect sphere, in order to apply
# basic geometric reasoning.
rEarth <-  6371005.0
colat <- (90 - lat)*2*pi/360          # Evaluating colatitude as radiant variable
sp <- (lat[45] - lat[44])*2*pi/360    # We have to do degree -> radiant conversion
area <- array(rEarth^2 * sp^2, dim = c(length(lon), length(lat)))

# Final pixel area computation
for(i in 1:length(lat)){
  area[, i] <- sin(colat[i]) * area[, i]
}


#----------------------------------------------#
#            Evaluating SWE evo                #
#----------------------------------------------#
ind = 1
for(y in years){
  for(i in 1:length(months)){
    
    # Logic conditions in order to create the correct file path
    if(i > 4){
      fname <- paste("y", toString(y), "/ITSNOW_SWE_", toString(y), months[i], ".nc", sep="")
    }
    else{
      fname <- paste("y", toString(y), "/ITSNOW_SWE_", toString(y-1), months[i], ".nc", sep="")
    }
    
    # The real deal, opening netCDF4 datas and evaluating total SWE
    nc <- nc_open(fname)
    time <- ncvar_get(nc, names(nc$var[1]))
    for(j in time){
      k <- j+1
      swe <- ncvar_get(nc, names(nc$var[2]), start = c(1, 1, k), count = c(-1, -1, 1))
      
      if(i > 4){
        print(paste("Calcolando lo SWE per il giorno ", toString(k), "/", months[i], "/", y, sep=""))  
      }
      else{
        print(paste("Calcolando lo SWE per il giorno ", toString(k), "/", months[i], "/", y-1, sep=""))
      }
      
      # SWE evaluation and storing
      mask <- !is.na(dem)
      swe[mask] <- swe[mask] * area[mask] * 10^-12
      swe[!mask] <- NA
      
      # Dealing with different altitude bands
      appo <- sweElevation(swe = swe, dem = dem, bands = bands)
      for(h in 1:length(appo)){
        evo_appo[ind, h] <- appo[h]
      }
      ind = ind + 1
    }
    nc_close(nc)
  }
}

df <- data.frame(
  b1 <- evo_appo[, 1],
  b2 <- evo_appo[, 2],
  b3 <- evo_appo[, 3],
  b4 <- evo_appo[, 4],
  b5 <- evo_appo[, 5],
  b6 <- evo_appo[, 6],
  b7 <- evo_appo[, 7],
  b8 <- evo_appo[, 8],
  b9 <- evo_appo[, 9]
  )

write.table(
  format(df, scientific = FALSE, digits = 6),
  file = "Datas/swe_evolution_elevation.dat", 
  row.names = FALSE, 
  col.names = FALSE, 
  quote = FALSE)

end_time <- Sys.time()
elapsed <- end_time - start_time
elapsed