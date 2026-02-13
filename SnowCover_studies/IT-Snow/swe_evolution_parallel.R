# The main goal of this script is to compute SWE series with a parallel coding 
# approach. I would like to split SWE matrix in sub-matrices, in order to make 
# total volume computation faster
rm(list = ls())
gc()

library(ncdf4)
library(parallel)

start_time <- Sys.time()
setwd("/home/filippo/Desktop/Codicini/Master_Thesis/SnowCover_studies/IT-Snow")


# Function to compute area map. This step is important because pixels are not of
# the same dimension, but there is a dependence on latitude. We are going to assume 
# that the earth is a perfect sphere, in order to apply basic geometric reasoning.
area_map <- function(lat, lon){
  
  rEarth <-  6371005.0
  colat <- (90 - lat)*2*pi/360          # Evaluating colatitude as radiant variable
  sp <- (lat[45] - lat[44])*2*pi/360    # We have to do degree -> radiant conversion
  area <- array(rEarth^2 * sp^2, dim = c(length(lon), length(lat)))
  
  # Final pixel area computation
  for(i in 1:length(lat)){
    area[, i] <- sin(colat[i]) * area[, i]
  }
  
  return(area)
}


# Function to compute swe evolution for a given year. The parallelization will be
# done on annual computations. Doing what I wanted to it's not a great idea because
# I will be spending most of the time opening/killing processes
annual_swe <- function(year, months, area){
  
  swe_year <- numeric(0)
  for(i in 1:length(months)){
    
    # Logic conditions in order to create the correct file path
    if(i > 4){
      fname <- paste("y", toString(year), "/ITSNOW_SWE_", toString(year), months[i], ".nc", sep="")
      print(paste("Evaluating SWE total volume ", months[i], "/", year, sep=""))
    }
    else{
      fname <- paste("y", toString(year), "/ITSNOW_SWE_", toString(year-1), months[i], ".nc", sep="")
      print(paste("Evaluating SWE total volume ", months[i], "/", year-1, sep=""))
    }
    
    # The real deal, opening netCDF4 datas and evaluating total SWE
    nc <- nc_open(fname)
    swe_maps <- ncvar_get(nc, names(nc$var[2]))
    nc_close(nc)
    
    swe_month <- numeric(0)
    swe_month <- apply(swe_maps, 3, function(x) sum(x * area, na.rm = TRUE) * 10^-12)
    
    swe_year <- c(swe_year, swe_month)
    rm(swe_maps, swe_month)
    gc()
  }
  
  return(swe_year)
}






# Time window
years <- 2011:2025
months <- c("09", "10", "11", "12", "01", "02", "03", "04", "05", "06", "07", "08")



# Importing latitude and longitude to compute area map
nc <- nc_open("y2011/ITSNOW_SWE_201103.nc")
lat <- ncvar_get(nc, "Latitude")
lon <- ncvar_get(nc, "Longitude")
nc_close(nc)

area <- area_map(lat, lon)
rm(lat, lon)
gc()



# SWE series evaluation. Here we want to go parallel. In particular, every core 
# will take care of a different year and the result of mclapply will be a list
# with [[i]]-th element corresponding to the i-th input
n_cores <- 8
result <- mclapply(years, function(y) {annual_swe(year = y, months = months, area = area)}, mc.cores = n_cores)
swe_evolution <- unlist(result)

df <- data.frame(
  day = 1:length(swe_evolution),
  swe = swe_evolution
)

data0 <- as.Date("2010-09-01")

# Plotting options (we will do a better job when it's all finished)
write.table(df$swe, file = "Datas/swe_evolution_parallel.dat", row.names = FALSE, col.names = FALSE)

end_time <- Sys.time()
elapsed <- end_time - start_time
elapsed
