# The main goal of this script is to find faulty days during 2003 season. I would
# like to select a specific location and then analyze its time series. How many NAs
# there are? Is it something I can deal with or not?
rm(list = ls())
gc()

library(terra)
setwd("/home/filippo/Desktop/Codicini/Master_Thesis/SnowCover_studies/Po-Basin")

year <- 2003
months <- c("10", "11", "12", "01", "02", "03", "04", "05", "06", "07")
dayMax <- c(31, 30, 31, 31, 28, 31, 30, 31, 30, 31)



# Selecting a point beeing part of the excluded region
lon_point <- 6.815498
lat_point <- 45.064986

point <- vect(data.frame(x=lon_point, y=lat_point), geom=c("x","y"), crs="EPSG:4326")



# Cycle to select daily values for the whole 2003 season
for(i in 1:length(months)){
  
  # Creating the correct file path
  if(i > 3){
    ypr <- year
    fname <- paste0("Dataset/", toString(year), "/SWE_", toString(ypr), "-", months[i], "-")
  }
  else{
    ypr <- year-1
    fname <- paste0("Dataset/", toString(year), "/SWE_", toString(ypr), "-", months[i], "-")
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
    final_fname <- paste0(fname, sprintf("%02d", d), ".tif")
    
    # Selecting pixel and saving its value
    daily_map <- rast(final_fname)
    appo <- extract(daily_map, point)[1, 2]
    
    if(is.na(appo)){
      print(paste0("Problemi con il valore del giorno: ", sprintf("%02d", d), "/", months[i], "/", ypr))
    }
  }
}  