# The main goal of this script is to evaluate total swe evolutino across the 
# investigated period, namely being 2016 to 2025
rm(list = ls())
gc()

library(ncdf4)
library(parallel)

start_time <- Sys.time()
setwd("/home/filippo/Desktop/Codicini/Master_Thesis/SnowCover_studies/Alpine3D/Recent/")


# Function to compute area map. Here all pixels are squares, so area computation is
# straight forward
area_map <- function(lat, lon){
  
  # Computing latitude and longitude differences
  diff_lat <- abs(lat[41] - lat[40]) 
  diff_lon <- abs(lon[41] - lon[40])
  
  # Area computation
  area <- array(diff_lat*diff_lon, dim = c(length(lat), length(lon)))
  return(area)
}


# Function to compute swe evolution for a given year.
annual_swe <- function(year, months, area){
  
  swe_year <- numeric(0)
  for(i in 1:length(months)){
    
    # Logic conditions in order to create the correct file path
    if(i > 4){
      fname <- paste("Dataset/", toString(year), "/OSHD_DATA_", toString(year), "-", months[i], ".nc", sep="")
      print(paste("Evaluating SWE total volume ", months[i], "/", year, sep=""))
    }
    else{
      fname <- paste("Dataset/", toString(year), "/OSHD_DATA_", toString(year-1), "-", months[i], ".nc", sep="")
      print(paste("Evaluating SWE total volume ", months[i], "/", year-1, sep=""))
    }
    
    # The real deal, opening netCDF4 datas and evaluating total SWE
    nc <- nc_open(fname)
    swe_maps <- ncvar_get(nc, "swe")
    nc_close(nc)
    
    swe_month <- numeric(0)
    swe_month <- apply(swe_maps, 1, function(x) sum(x * area, na.rm = TRUE) * 10^-12)
    
    swe_year <- c(swe_year, swe_month)
    rm(swe_maps, swe_month)
    gc()
  }
  
  return(swe_year)
}





# Time window
years <- 2016:2025
months <- c("09", "10", "11", "12", "01", "02", "03", "04", "05", "06", "07", "08")


# Importing latitude and longitude to compute area map
nc <- nc_open("Dataset/2016/OSHD_DATA_2015-09.nc")
lat <- ncvar_get(nc, "northing")
lon <- ncvar_get(nc, "easting")
nc_close(nc)

area <- area_map(lat, lon)
rm(lat, lon)
gc()


# SWE series evaluation
n_cores <- 5
result <- mclapply(years, function(y) {annual_swe(year = y, months = months, area = area)}, mc.cores = n_cores)
swe_evolution <- unlist(result)

df <- data.frame(
  day = 1:length(swe_evolution),
  swe = swe_evolution
)


write.table(df$swe, file = "Datas/swe_evolution_parallel.dat", row.names = FALSE, col.names = FALSE)

end_time <- Sys.time()
elapsed <- end_time - start_time
elapsed
